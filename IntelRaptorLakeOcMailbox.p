//  IntelRaptorLakeOcMailbox - A narrow PawnIO module for MSR 0x150 OC-mailbox
//  voltage offsets on the tested Raptor Lake-S desktop model family.
//
//  Copyright (C) 2026  RaptorLakeVF contributors
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  SPDX-License-Identifier: LGPL-2.1-or-later
//
//  STATUS: SOURCE ONLY. This module is NOT built, NOT signed, NOT loaded, and is
//  NOT loadable in this repository. It is prepared for review and (later) for
//  submission to the PawnIO maintainers. A locally built unsigned artifact is
//  REVIEW ONLY and must never be placed where the application could discover it.
//  The REAL build toolchain is the pinned PawnIO.Modules compiler (pawncc
//  4.1.7152 from the `_pawn/` RPMs); the C file in this directory is an
//  AUXILIARY syntax mirror only (gcc stub) and is NOT the module.
//
//  PRIVILEGED SURFACE (the only operations this module performs):
//    ioctl_query_ia_core_offset()      -> legacy plane-offset QUERY, domain 0
//    ioctl_query_ring_offset()         -> legacy plane-offset QUERY, domain 2
//    ioctl_set_ia_core_neg_offset()    -> legacy plane-offset SET, domain 0
//    ioctl_set_ring_neg_offset()       -> legacy plane-offset SET, domain 2
//
//  HARD INVARIANTS:
//    - NO caller-selected MSR number. The only MSR ever accessed is 0x150
//      (IA32_OC_MAILBOX); the internal mailbox helper rejects anything else even
//      if called with a different address (defense in depth).
//    - NO caller-supplied raw 64-bit request. Requests are built ONLY from the
//      typed operations above with validated command/domain/payload fields.
//    - Non-positive only: SET operations accept raw_steps <= 0 (zero is
//      intentionally permitted — an exact restore state may be 0 offset —
//      negatives are accepted) and reject positive payloads before any
//      privileged access; the signed payload is validated against the signed
//      11-bit representation (bits [31:21], 1/1024 V quantum).
//    - Reserved bits outside [31:21] are rejected.
//    - No power-limit, VR-mailbox, ratio, thermal, firmware-storage, or I/O-port
//      access of any kind.
//
//  QUERY PROTOCOL EVIDENCE (4N.2): the non-mutating query packet
//  (bit63 busy + cmd 0x10 in [39:32] + domain in [47:40]) is line-confirmed in
//  five independent implementations (georgewhewell/undervolt,
//  kitsunyan/intel-undervolt, wesmar/UnderVolter, subratamal/VoltageShift,
//  xCuri0/VoltageShiftSecure); wesmar/UnderVolter CpuData.c lists family 6
//  model 183 (0xB7) as Raptor Lake-S B0/C0 desktop — the tested i5-13600KF.
//  RaptorLakeVF's evidence policy therefore reports the query protocol
//  Supported; live execution is still separately gated (signed module + platform
//  proof) and never happens in this repository.

#include <pawnio.inc>

// The ONLY writable MSR this module ever touches.
#define RAPTORLAKEVF_ONLY_MSR 0x150

// Legacy global-offset OC-mailbox packet fields (source-golden, 4N.2
// line-confirmed across five independent implementations).
#define OC_MAILBOX_REQUEST_BUSY_BIT (1 << 63)
#define OC_MAILBOX_COMMAND_MASK     (0xFF << 32)
#define OC_MAILBOX_QUERY_COMMAND    (0x10 << 32)
#define OC_MAILBOX_SET_COMMAND      (0x11 << 32)
#define OC_MAILBOX_OFFSET_MASK      0xFFE00000          // bits [31:21]
#define OC_MAILBOX_DOMAIN_IA_CORE   0
#define OC_MAILBOX_DOMAIN_RING      2

// CPU family/model gate (Milestone 4N.1/4N.2). The first proposed release is
// gated to the tested Raptor Lake-S model family: family 6, model 0xB7 (183),
// stepping 1 — the tested i5-13600KF (0xB7/1 = RPL-S B0 per wesmar/UnderVolter
// CpuData.c). The gate is FAIL-CLOSED: only the evidenced stepping is accepted.
// It is never broadened to every Intel CPU.
#define RAPTORLAKEVF_GATED_FAMILY 6
#define RAPTORLAKEVF_GATED_MODEL 0xB7
#define RAPTORLAKEVF_GATED_STEPPING 1

// Bounded busy-poll budget (same order as the independent implementations).
#define OC_MAILBOX_MAX_SPINS 8

/// Validate the CPU gate (family 6, model 0xB7, stepping 1).
stock bool:cpu_gate_ok() {
    new fms = get_cpu_fms();
    return cpu_fms_family(fms) == RAPTORLAKEVF_GATED_FAMILY
        && cpu_fms_model(fms) == RAPTORLAKEVF_GATED_MODEL
        && cpu_fms_stepping(fms) == RAPTORLAKEVF_GATED_STEPPING;
}

/// Send one OC-mailbox packet to MSR 0x150 and wait for the busy marker to
/// clear, returning the raw response. Rejects any other MSR address even if
/// called with one (defense in depth: no caller-selected MSR can ever reach a
/// privileged write).
stock NTSTATUS:mailbox_send(msr, value, &response) {
    if (msr != RAPTORLAKEVF_ONLY_MSR)
        return STATUS_ACCESS_DENIED;

    new NTSTATUS:status = msr_write(msr, value);
    if (status != STATUS_SUCCESS)
        return status;

    new i;
    for (i = 0; i < OC_MAILBOX_MAX_SPINS; i++) {
        new r = 0;
        status = msr_read(msr, r);
        if (status != STATUS_SUCCESS)
            return status;
        if ((r & OC_MAILBOX_REQUEST_BUSY_BIT) == 0) {
            response = r;
            return STATUS_SUCCESS;
        }
        microsleep(50);
    }
    return STATUS_TIMEOUT;
}

/// Build the legacy global-offset packet. Only the two typed domains and the
/// two typed commands exist; the signed payload must be pre-validated and must
/// only set bits [31:21].
stock NTSTATUS:build_legacy_packet(domain, command, payload, &packet) {
    if (domain != OC_MAILBOX_DOMAIN_IA_CORE && domain != OC_MAILBOX_DOMAIN_RING)
        return STATUS_INVALID_PARAMETER;
    if (command != OC_MAILBOX_QUERY_COMMAND && command != OC_MAILBOX_SET_COMMAND)
        return STATUS_INVALID_PARAMETER;
    if ((payload & ~OC_MAILBOX_OFFSET_MASK) != 0)
        return STATUS_INVALID_PARAMETER;
    packet = OC_MAILBOX_REQUEST_BUSY_BIT | (domain << 40) | command | payload;
    return STATUS_SUCCESS;
}

/// Non-positive signed payload check: raw_steps must be <= 0 (zero accepted —
/// an exact restore state may be 0 offset — negative accepted) and within the
/// signed 11-bit representation. Positive values are rejected. Returns the
/// [31:21] payload via `payload`.
stock NTSTATUS:validate_negative_steps(raw_steps, &payload) {
    if (raw_steps > 0)
        return STATUS_INVALID_PARAMETER;
    if (raw_steps < -1024)
        return STATUS_INVALID_PARAMETER;
    payload = (raw_steps << 21) & OC_MAILBOX_OFFSET_MASK;
    return STATUS_SUCCESS;
}

/// Query the legacy IA-core offset (domain 0).
/// @param in_size Must be 0
/// @param out [0] = raw mailbox response
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_query_ia_core_offset, 0, 1) {
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    new packet = 0;
    new NTSTATUS:status = build_legacy_packet(OC_MAILBOX_DOMAIN_IA_CORE, OC_MAILBOX_QUERY_COMMAND, 0, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    status = mailbox_send(RAPTORLAKEVF_ONLY_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Query the legacy Ring/Cache offset (domain 2).
/// @param in_size Must be 0
/// @param out [0] = raw mailbox response
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_query_ring_offset, 0, 1) {
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    new packet = 0;
    new NTSTATUS:status = build_legacy_packet(OC_MAILBOX_DOMAIN_RING, OC_MAILBOX_QUERY_COMMAND, 0, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    status = mailbox_send(RAPTORLAKEVF_ONLY_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Set a non-positive IA-core offset (domain 0). The caller supplies RAW STEPS
/// (<= 0 — zero accepted for an exact restore-to-0 state — within the signed
/// 11-bit range); the module encodes bits [31:21].
/// @param in [0] = raw steps (non-positive)
/// @param in_size Must be 1
/// @param out [0] = raw mailbox response
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_ia_core_neg_offset, 1, 1) {
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    new raw_steps = in[0];
    new payload = 0;
    new NTSTATUS:status = validate_negative_steps(raw_steps, payload);
    if (status != STATUS_SUCCESS)
        return status;

    new packet = 0;
    status = build_legacy_packet(OC_MAILBOX_DOMAIN_IA_CORE, OC_MAILBOX_SET_COMMAND, payload, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    // Mutation is never retried silently: the busy timeout or unexpected
    // response is returned to the caller as an explicit failure.
    status = mailbox_send(RAPTORLAKEVF_ONLY_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Set a non-positive Ring/Cache offset (domain 2). The caller supplies RAW STEPS
/// (<= 0 — zero accepted for an exact restore-to-0 state — within the signed
/// 11-bit range); the module encodes bits [31:21].
/// @param in [0] = raw steps (non-positive)
/// @param in_size Must be 1
/// @param out [0] = raw mailbox response
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_ring_neg_offset, 1, 1) {
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    new raw_steps = in[0];
    new payload = 0;
    new NTSTATUS:status = validate_negative_steps(raw_steps, payload);
    if (status != STATUS_SUCCESS)
        return status;

    new packet = 0;
    status = build_legacy_packet(OC_MAILBOX_DOMAIN_RING, OC_MAILBOX_SET_COMMAND, payload, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    status = mailbox_send(RAPTORLAKEVF_ONLY_MSR, packet, response);
    out[0] = response;
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    // Fail closed on any CPU outside the evidenced model/stepping.
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
