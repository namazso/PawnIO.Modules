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
//  PURPOSE
//  -------
//  Provides typed access to the Intel OC-mailbox (MSR 0x150) used by
//  Raptor Lake-S undervolting: read the legacy global voltage offset for the
//  IA/Core and Ring/Cache planes, and set a non-positive offset on either
//  plane. The surface is intentionally narrow: four typed operations, no
//  caller-selected MSR, domain, command, or raw mailbox packet.
//
//  SUPPORTED CPU SCOPE
//  -------------------
//  Family 6, model 0xB7 (183), stepping 1 — Intel Core i5-13600KF (Raptor
//  Lake-S B0 desktop). The module refuses to load on any other CPU.
//
//  PRIVILEGED SURFACE (the only operations this module performs):
//    ioctl_query_ia_core_offset()      -> OC-mailbox QUERY, domain 0 (IA/Core)
//    ioctl_query_ring_offset()         -> OC-mailbox QUERY, domain 2 (Ring/Cache)
//    ioctl_set_ia_core_neg_offset()    -> OC-mailbox SET, domain 0 (IA/Core)
//    ioctl_set_ring_neg_offset()       -> OC-mailbox SET, domain 2 (Ring/Cache)
//
//  PROTOCOL FIELDS (legacy global-offset OC-mailbox packet, MSR 0x150)
//  ------------------------------------------------------------------
//    bit 63         request/busy marker
//    bits [47:40]   domain (0 = IA/Core, 2 = Ring/Cache)
//    bits [39:32]   command (0x10 = query/read, 0x11 = set/write)
//    bits [31:21]   signed offset payload, 1/1024 V per step
//
//  SET accepts NON-POSITIVE raw steps only (raw_steps <= 0): zero is accepted
//  (an exact restore state may be 0 offset) and negatives are accepted;
//  positive values are rejected before any privileged access.
//
//  RESPONSE SEMANTICS
//  ------------------
//  Every IOCTL returns the raw 64-bit mailbox response cell. The module does
//  NOT decode or verify the response: interpretation (e.g. decoding the offset
//  field, comparing a readback) is performed by the userspace typed transport
//  that calls this module.
//
//  SECURITY RESTRICTIONS
//  ---------------------
//    - Only MSR 0x150 is ever accessed; the mailbox helper rejects any other
//      address even if called with one (defense in depth).
//    - No caller-supplied raw 64-bit request: packets are built only from the
//      typed operations above with validated command/domain/payload fields.
//    - No power-limit, VR-mailbox, ratio, thermal, firmware-storage, or I/O
//      access; no OC-lock/UVP/CFG-lock bypass; no security-state access.
//    - Bounded busy polling (8 spins, 50 us sleep); a timeout is an explicit
//      failure; mutations are never retried silently.
//
//  SOURCE REFERENCES
//  -----------------
//  The packet layout is corroborated by independent implementations:
//    - georgewhewell/undervolt (2d825f96): undervolt.py pack_offset()
//    - kitsunyan/intel-undervolt (ea0e74c5): undervolt.c (rdval/wrval)
//    - wesmar/UnderVolter (716df96a): VfCurve.c, OcMailbox.c, CpuData.c
//    - subratamal/VoltageShift (33aab6ae), xCuri0/VoltageShiftSecure
//      (6b4612c0): main.mm writeOCMailBox/readOCMailBox
//  wesmar/UnderVolter CpuData.c lists family 6 model 183 (0xB7) as
//  "RaptorLake" RPL-S B0 desktop (B0700) — an exact match for this module's
//  tested target.

#include <pawnio.inc>

// The ONLY writable MSR this module ever touches.
#define OC_MAILBOX_MSR 0x150

// Legacy global-offset OC-mailbox packet fields (corroborated by the
// independent implementations listed above).
#define OC_MAILBOX_REQUEST_BUSY_BIT (1 << 63)
#define OC_MAILBOX_COMMAND_MASK     (0xFF << 32)
#define OC_MAILBOX_QUERY_COMMAND    (0x10 << 32)
#define OC_MAILBOX_SET_COMMAND      (0x11 << 32)
#define OC_MAILBOX_OFFSET_MASK      0xFFE00000          // bits [31:21]
#define OC_MAILBOX_DOMAIN_IA_CORE   0
#define OC_MAILBOX_DOMAIN_RING      2

// Supported CPU scope: family 6, model 0xB7 (183), stepping 1 — the tested
// Intel Core i5-13600KF (Raptor Lake-S B0 desktop). The gate is FAIL-CLOSED:
// only the tested stepping is accepted, and it is never broadened to every
// Intel CPU.
#define SUPPORTED_CPU_FAMILY 6
#define SUPPORTED_CPU_MODEL 0xB7
#define SUPPORTED_CPU_STEPPING 1

// Bounded busy-poll budget (same order as the independent implementations).
#define OC_MAILBOX_MAX_SPINS 8

/// Validate the CPU gate (family 6, model 0xB7, stepping 1).
stock bool:cpu_gate_ok() {
    new fms = get_cpu_fms();
    return cpu_fms_family(fms) == SUPPORTED_CPU_FAMILY
        && cpu_fms_model(fms) == SUPPORTED_CPU_MODEL
        && cpu_fms_stepping(fms) == SUPPORTED_CPU_STEPPING;
}

/// Send one OC-mailbox packet to MSR 0x150 and wait for the busy marker to
/// clear, returning the raw response. Rejects any other MSR address even if
/// called with one (defense in depth: no caller-selected MSR can ever reach a
/// privileged write). The response is returned RAW — the module does not
/// interpret its contents; that is the caller's responsibility.
stock NTSTATUS:mailbox_send(msr, value, &response) {
    if (msr != OC_MAILBOX_MSR)
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
stock NTSTATUS:validate_non_positive_steps(raw_steps, &payload) {
    if (raw_steps > 0)
        return STATUS_INVALID_PARAMETER;
    if (raw_steps < -1024)
        return STATUS_INVALID_PARAMETER;
    payload = (raw_steps << 21) & OC_MAILBOX_OFFSET_MASK;
    return STATUS_SUCCESS;
}

/// Query the legacy IA-core offset (domain 0).
/// The raw mailbox response is returned in out[0]; decoding/verification is
/// performed by the userspace typed transport.
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
    status = mailbox_send(OC_MAILBOX_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Query the legacy Ring/Cache offset (domain 2).
/// The raw mailbox response is returned in out[0]; decoding/verification is
/// performed by the userspace typed transport.
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
    status = mailbox_send(OC_MAILBOX_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Set a non-positive IA-core offset (domain 0). The caller supplies RAW STEPS
/// (<= 0 — zero accepted for an exact restore-to-0 state — within the signed
/// 11-bit range); the module encodes bits [31:21]. The raw mailbox response is
/// returned in out[0]; the caller is responsible for readback verification.
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
    new NTSTATUS:status = validate_non_positive_steps(raw_steps, payload);
    if (status != STATUS_SUCCESS)
        return status;

    new packet = 0;
    status = build_legacy_packet(OC_MAILBOX_DOMAIN_IA_CORE, OC_MAILBOX_SET_COMMAND, payload, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    // Mutation is never retried silently: a busy timeout is returned as an
    // explicit failure. The raw response is returned for caller-side
    // verification; the module does not claim success from a status field.
    status = mailbox_send(OC_MAILBOX_MSR, packet, response);
    out[0] = response;
    return status;
}

/// Set a non-positive Ring/Cache offset (domain 2). The caller supplies RAW
/// STEPS (<= 0 — zero accepted for an exact restore-to-0 state — within the
/// signed 11-bit range); the module encodes bits [31:21]. The raw mailbox
/// response is returned in out[0]; the caller is responsible for readback
/// verification.
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
    new NTSTATUS:status = validate_non_positive_steps(raw_steps, payload);
    if (status != STATUS_SUCCESS)
        return status;

    new packet = 0;
    status = build_legacy_packet(OC_MAILBOX_DOMAIN_RING, OC_MAILBOX_SET_COMMAND, payload, packet);
    if (status != STATUS_SUCCESS)
        return status;

    new response = 0;
    // Mutation is never retried silently: a busy timeout is returned as an
    // explicit failure. The raw response is returned for caller-side
    // verification; the module does not claim success from a status field.
    status = mailbox_send(OC_MAILBOX_MSR, packet, response);
    out[0] = response;
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    // Fail closed on any CPU outside the supported family/model/stepping.
    if (!cpu_gate_ok())
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
