//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  namazso <admin@namazso.eu>
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

#include <pawnio.inc>

// PawnIO Intel Client IMC Clock Driver
//
// This module reads the integrated memory controller (IMC) clock ratio that
// firmware programs during memory training on Intel client SoCs. The intent
// is to give monitoring tools a safe way to compute a "Memory Clock" sensor 
// on platforms where the legacy MSR_UNCORE_PERF_STATUS based formula no 
// longer applies because uncore and the IMC are decoupled.
//
// Two read sources are implemented and selected per CPUID model:
//
//   * MEMSS_PMA_CR_BIOS_DATA at MCHBAR + 0x13D10 - used on Core Ultra
//     (Meteor Lake, Arrow Lake, Lunar Lake, Panther Lake). The locked Qclk
//     ratio is referenced to BCLK/3. Static after MRC.
//
//   * SA_PERF_STATUS at MCHBAR + 0x5918 - used on Alder Lake / Raptor Lake.
//     Live workpoint, with a per-platform reference clock bit selecting
//     between BCLK and BCLK*4/3.
//
// Both registers are read-only and at fixed compile-time offsets. The
// platform tag derived from CPUID determines which register is used; one
// is never read on a CPU it doesn't apply to.
//
// Validation status: as of this revision, no allowlisted platform has had
// real-hardware validation of the returned ratio against a reference such
// as HWiNFO/CPU-Z DRAM Frequency. Every successful return therefore sets
// the EXPERIMENTAL flag in out[6]. Consumers MUST treat EXPERIMENTAL as
// "do not expose as a primary sensor by default" - either keep the sensor
// hidden, place it behind a debug toggle, or label it preview/experimental
// in the UI. Per-platform "validated" flags will be added (and EXPERIMENTAL
// cleared) as validation results land.
//
// Design constraints, kept deliberately tight so a security review is easy:
//
//   * Read-only. No writes to PCI config, MMIO, MSR, or IO ports.
//   * No user-controlled physical addresses or PCI BDF reach the kernel.
//   * One IOCTL only. Every register read is at a compile-time constant
//     offset against the firmware-published MCHBAR base.
//   * Strict CPUID allowlist. Unknown models return STATUS_NOT_SUPPORTED.
//   * Reserved bits and out-of-range ratios are rejected to avoid reporting
//     a wrong-but-confident value if a future stepping revises the layout.
//   * MCHBAR enable bit is observed but never modified.
//
// Public references used while writing this module:
//   - Intel Core Ultra 200H/200U CFG/MEM register reference, MEMSS_PMA_CR_BIOS_DATA
//   - Intel Core Ultra 200H/200U CFG/MEM register reference, MCHBAR base PCI 0/0/0 offset 0x48
//   - Intel 14th-gen client CFG/MEM register reference, SA_PERF_STATUS at MCHBAR + 0x5918
//   - Intel perfmon mapfile.csv (V1.05 PTL, V1.17 ARL, V1.21 MTL, V1.22 LNL, V1.39 ADL/RPL)

// === Module ABI ===
// Bumped only on incompatible output buffer layout changes.
#define IMC_CLOCK_ABI_VERSION       1

// Identifies which on-die source produced the ratio. Returned as out[1] so
// downstream code can adapt its conversion if a future revision of this
// module adds a different source register.
#define IMC_SRC_NONE                0
#define IMC_SRC_MCHBAR_MEMSS_PMA    1   // MCHBAR + 0x13D10 (MEMSS_PMA_CR_BIOS_DATA)
#define IMC_SRC_MCHBAR_SA_PERF      2   // MCHBAR + 0x5918  (SA_PERF_STATUS)
#define IMC_SRC_PMT_QCLK_STATUS     3   // PMBAR-based      (reserved, not used yet)

// What the ratio is multiplied with on this hardware. The consumer measures
// BCLK separately and computes MHz with whatever BCLK it sees; returning the
// mode keeps that conversion correct across BCLK variants.
#define IMC_REF_UNKNOWN             0
#define IMC_REF_BCLK_DIV_3          1   // 33.333 MHz when BCLK is 100 MHz
#define IMC_REF_BCLK                2   // 100 MHz when BCLK is 100 MHz
#define IMC_REF_BCLK_MUL_4_DIV_3    3   // 133.333 MHz when BCLK is 100 MHz

// DDR controller "gear". 0 means the source register does not encode a gear.
#define IMC_GEAR_UNKNOWN            0
#define IMC_GEAR_1                  1
#define IMC_GEAR_2                  2
#define IMC_GEAR_4                  4

// Hints to the consumer about how to interpret the ratio. Multiple bits
// may be set. While EXPERIMENTAL is set, the value should be treated as
// best-effort: the consumer is expected to keep the sensor hidden by
// default or label it accordingly. Per-platform "validated" bits are
// intentionally not part of this ABI yet; they will be introduced once
// real-hardware validation produces a pass criterion, at which point
// EXPERIMENTAL will be cleared on those platforms.
#define IMC_FLAG_STATIC_LOCKED      (1 << 0)    // value latched after MRC, won't track SAGV
#define IMC_FLAG_LIVE_CURRENT       (1 << 1)    // value reflects the live workpoint
#define IMC_FLAG_EXPERIMENTAL       (1 << 2)    // best-effort, treat with care
// Bits 3 and above are reserved for future use.

// === Host bridge / MCHBAR layout ===
// Host bridge is always at bus 0, device 0, function 0 on Intel client SoCs.
#define HOSTBRIDGE_BUS              0
#define HOSTBRIDGE_DEV              0
#define HOSTBRIDGE_FUNC             0
// PCI config offsets that publish MCHBAR base on Core Ultra processors.
#define HOSTBRIDGE_MCHBAR_LO        0x48
#define HOSTBRIDGE_MCHBAR_HI        0x4C
// Bit 0 of the low dword is MCHBAREN. We refuse to touch MCHBAR if firmware
// has not already enabled it; we never set this bit ourselves.
#define MCHBAR_ENABLE_BIT           0x1
// MCHBAR base spans bits 41:17 of the combined 64-bit value.
#define MCHBAR_BASE_MASK            0x000003FFFFFE0000

// === Register offsets inside MCHBAR ===
// MEMSS_PMA_CR_BIOS_DATA: the locked Qclk ratio that firmware programs after
// memory training. Public Intel docs cover this register on Core Ultra
// 200H/200U; the same register layout is used on Meteor Lake (Core Ultra
// 100), Lunar Lake (Core Ultra 200V), and Panther Lake (Core Ultra series
// 3). All four Core Ultra families read the same way.
//   bits 7:0  QCLK_RATIO   - controller QCLK multiplier of BCLK/3
//   bit 8     GEAR_TYPE    - 0 = Gear2, 1 = Gear4
#define MEMSS_PMA_CR_BIOS_DATA      0x13D10

// SA_PERF_STATUS: the older System-Agent performance status register used
// on Alder Lake / Raptor Lake clients. Intel's Core Ultra docs warn that
// this register's QCLK_RATIO field is "not defined properly" on Core Ultra
// silicon and route the equivalent data through MEMSS_PMA instead, so this
// register is only used on ADL/RPL.
//   bits 9:2  QCLK_RATIO     - controller QCLK multiplier of the reference
//   bit 10    QCLK_REFERENCE - 0 = BCLK*4/3 (133.33 MHz), 1 = BCLK (100 MHz)
#define SA_PERF_STATUS              0x5918

// Range a freshly trained QCLK ratio is expected to fall in. The lower
// bound rejects 0/very-low values that would mean "register not populated"
// or "wrong source". The upper bound is generous enough to cover heavily
// overclocked DDR5-12000 (ratio ~180 against BCLK/3) without rejecting
// future SKUs. Anything outside this range we treat as "wrong register"
// and refuse rather than report.
#define IMC_RATIO_MIN               16
#define IMC_RATIO_MAX               220

// Mask of reserved bits in MEMSS_PMA_CR_BIOS_DATA (bits 31:9). A nonzero
// value here means the read returned something this module does not
// understand - either the register layout shifted on a future stepping or
// we are on a CPU that mapped a different register at this offset.
#define MEMSS_PMA_RESERVED_MASK     0xFFFFFE00

// === Platform tags ===
// A platform tag identifies the family of registers this module knows how
// to interpret on a given CPU. We keep MTL/ARL/LNL/PTL distinct from each
// other (and from ADL/RPL) so a future revision can light up per-platform
// validated flags without affecting the others.
#define PLAT_NONE                   0
#define PLAT_MTL                    1   // Intel Core Ultra 100, Meteor Lake
#define PLAT_ARL                    2   // Intel Core Ultra 200, Arrow Lake
#define PLAT_LNL                    3   // Intel Core Ultra 200V, Lunar Lake
#define PLAT_PTL                    4   // Intel Core Ultra series 3, Panther Lake
#define PLAT_ADL                    5   // 12th Gen Core, Alder Lake
#define PLAT_RPL                    6   // 13th/14th Gen Core, Raptor Lake

// Map a CPUID model byte to a platform tag. Anything not listed here is
// rejected up front so that no MMIO is touched on unfamiliar hardware.
//
// Coverage is taken straight from intel-perfmon mapfile.csv:
//   MTL: 0xAA, 0xAC, 0xB5  (V1.21)
//   ARL: 0xC5, 0xC6        (V1.17)
//   LNL: 0xBD              (V1.22)
//   PTL: 0xCC, 0xD5        (V1.05, published 2026-02-26)
//   ADL: 0x97, 0x9A, 0xBE  (V1.39)
//   RPL: 0xB7, 0xBA, 0xBF  (V1.39, listed under /ADL/ in mapfile)
//
// Notably *not* included:
//   0xCD, 0xCE - not currently mapped to any Intel platform in mapfile.csv
//   0xCF       - mapped to Emerald Rapids (server), must not be treated as PTL
//   ICL/TGL/RKL (0x7D/0x7E/0x8C/0x8D/0xA7) - their IMC publishing register
//                is not yet validated for this module
stock get_platform(model) {
    switch (model) {
        case 0xAA, 0xAC, 0xB5:
            return PLAT_MTL;
        case 0xC5, 0xC6:
            return PLAT_ARL;
        case 0xBD:
            return PLAT_LNL;
        case 0xCC, 0xD5:
            return PLAT_PTL;
        case 0x97, 0x9A, 0xBE:
            return PLAT_ADL;
        case 0xB7, 0xBA, 0xBF:
            return PLAT_RPL;
    }
    return PLAT_NONE;
}

// Choose which on-die register exposes the IMC ratio for a given platform.
// All Core Ultra family members route through MEMSS_PMA_CR_BIOS_DATA;
// Alder/Raptor Lake route through SA_PERF_STATUS. There is no fall-through
// between the two sources: the wrong register on the wrong platform would
// either be reserved or "not defined properly" per Intel's own docs.
stock get_platform_source(platform) {
    switch (platform) {
        case PLAT_MTL, PLAT_ARL, PLAT_LNL, PLAT_PTL:
            return IMC_SRC_MCHBAR_MEMSS_PMA;
        case PLAT_ADL, PLAT_RPL:
            return IMC_SRC_MCHBAR_SA_PERF;
    }
    return IMC_SRC_NONE;
}

// Read the firmware-published MCHBAR base from the host bridge. We treat a
// disabled or zero base as "not supported" rather than trying to bring it
// up; the running OS would normally have already configured this.
stock NTSTATUS:read_mchbar_base(&base) {
    new lo = 0;
    new hi = 0;

    new NTSTATUS:status = pci_config_read_dword(
        HOSTBRIDGE_BUS, HOSTBRIDGE_DEV, HOSTBRIDGE_FUNC,
        HOSTBRIDGE_MCHBAR_LO, lo);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(
        HOSTBRIDGE_BUS, HOSTBRIDGE_DEV, HOSTBRIDGE_FUNC,
        HOSTBRIDGE_MCHBAR_HI, hi);
    if (!NT_SUCCESS(status))
        return status;

    // Refuse if firmware has not enabled MCHBAR. This module never enables it.
    if ((lo & MCHBAR_ENABLE_BIT) == 0)
        return STATUS_NOT_SUPPORTED;

    // Combine the two dwords. We mask each to 32 bits first so a high bit
    // in lo can't sign-extend into hi when the cell is interpreted as
    // signed 64-bit.
    base = ((hi & 0xFFFFFFFF) << 32) | (lo & 0xFFFFFFFF);
    base = base & MCHBAR_BASE_MASK;

    if (base == 0)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// Read a single 32-bit MMIO register at MCHBAR + offset. The offset comes
// only from compile-time constants we control, never from the IOCTL
// caller, so the address handed to io_space_map is always pinned to a
// register this module documents.
stock NTSTATUS:read_mchbar_dword(mchbar_base, offset, &value) {
    new VA:va = io_space_map(mchbar_base + offset, 4);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    new NTSTATUS:status = virtual_read_dword(va, value);

    io_space_unmap(va, 4);
    return status;
}

// Read MEMSS_PMA_CR_BIOS_DATA and split it into ratio + gear. We refuse
// to report a value if any of the reserved bits Intel documents as 0 came
// back set, or if the ratio is outside the plausible DDR5/LPDDR5 range -
// either case usually means we are reading the wrong register.
stock NTSTATUS:read_memss_pma(mchbar_base, &raw, &ratio, &gear) {
    new NTSTATUS:status = read_mchbar_dword(mchbar_base, MEMSS_PMA_CR_BIOS_DATA, raw);
    if (!NT_SUCCESS(status))
        return status;

    // Reserved bits must be zero on documented platforms. If they are not,
    // the register has either been repurposed or the read returned junk.
    if ((raw & MEMSS_PMA_RESERVED_MASK) != 0)
        return STATUS_NOT_SUPPORTED;

    ratio = raw & 0xFF;
    gear = ((raw >> 8) & 0x1) ? IMC_GEAR_4 : IMC_GEAR_2;

    if (ratio < IMC_RATIO_MIN || ratio > IMC_RATIO_MAX)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// Read SA_PERF_STATUS and split it into ratio + reference clock mode. The
// register does not encode a gear field on ADL/RPL, so gear stays Unknown
// and the consumer can derive Gear1/Gear2 from configured DRAM rate if it
// needs to. The reference bit selects between BCLK and BCLK*4/3 - the
// latter is the common configuration for DDR5 on these platforms.
//
// We do not validate reserved bits here because Intel's older client docs
// describe several other fields in SA_PERF_STATUS that this module does
// not consume; a "reserved" mask is therefore not safe to assume. Range
// checking the ratio still catches the wrong-register / wrong-platform
// cases without rejecting valid bits we do not interpret.
stock NTSTATUS:read_sa_perf_status(mchbar_base, &raw, &ratio, &refMode) {
    new NTSTATUS:status = read_mchbar_dword(mchbar_base, SA_PERF_STATUS, raw);
    if (!NT_SUCCESS(status))
        return status;

    ratio = (raw >> 2) & 0xFF;
    refMode = ((raw >> 10) & 0x1) ? IMC_REF_BCLK : IMC_REF_BCLK_MUL_4_DIV_3;

    if (ratio < IMC_RATIO_MIN || ratio > IMC_RATIO_MAX)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

/// Read Intel client IMC/QCLK clock-ratio information.
///
/// This is the only IOCTL the module exposes. There is no generic PCI,
/// MMIO, or MSR access. All addresses are hardcoded inside the module
/// and gated by a strict CPUID allowlist covering Alder Lake, Raptor Lake,
/// Meteor Lake, Arrow Lake, Lunar Lake, and Panther Lake. Older client
/// platforms whose IMC ratio is still tied to MSR_UNCORE_PERF_STATUS
/// remain on the existing IntelMSR module.
///
/// The ratio is returned together with its reference clock mode and gear
/// so the consumer can compute a memory clock with whatever BCLK it has
/// already measured. Doing the multiplication on the consumer side keeps
/// this module free of floating point and of any UI semantics.
///
/// While IMC_FLAG_EXPERIMENTAL is set in out[6] the value is best-effort
/// and the consumer must keep the resulting sensor hidden by default or
/// label it as experimental.
///
/// @param in_size Must be 0
/// @param out [0] = ABI version (currently 1)
/// @param out [1] = source enum (see IMC_SRC_*)
/// @param out [2] = ratio (controller QCLK multiplier of the reference clock)
/// @param out [3] = reference clock mode enum (see IMC_REF_*)
/// @param out [4] = gear enum (see IMC_GEAR_*, 0 if not applicable)
/// @param out [5] = raw register dword the ratio was decoded from
/// @param out [6] = flags (see IMC_FLAG_*)
/// @param out_size Must be 7
/// @return STATUS_SUCCESS on a supported source whose register passed all
///         consistency checks (reserved bits zero where documented, ratio
///         in IMC_RATIO_MIN..IMC_RATIO_MAX).
///         STATUS_NOT_SUPPORTED if the CPU is not in the allowlist, MCHBAR
///         is disabled, or the register's reserved bits / ratio range fail
///         the consistency checks.
///         Other NTSTATUS on PCI/MMIO read failure.
DEFINE_IOCTL_SIZED(ioctl_read_imc_clock, 0, 7) {
    // Pre-fill the buffer so the caller can rely on a well-defined layout
    // even if we end up returning an error before reading any register.
    out[0] = IMC_CLOCK_ABI_VERSION;
    out[1] = IMC_SRC_NONE;
    out[2] = 0;
    out[3] = IMC_REF_UNKNOWN;
    out[4] = IMC_GEAR_UNKNOWN;
    out[5] = 0;
    out[6] = 0;

    // Resolve the running CPU into a platform tag, then a register source.
    // Anything outside the allowlist is rejected before we touch any bus
    // or memory.
    new fms = get_cpu_fms();
    if (cpu_fms_family(fms) != 0x6)
        return STATUS_NOT_SUPPORTED;

    new platform = get_platform(cpu_fms_model(fms));
    if (platform == PLAT_NONE)
        return STATUS_NOT_SUPPORTED;

    new source = get_platform_source(platform);
    if (source == IMC_SRC_NONE)
        return STATUS_NOT_SUPPORTED;

    // Resolve MCHBAR. If firmware has not enabled it we refuse rather
    // than trying to bring it up ourselves.
    new mchbar = 0;
    new NTSTATUS:status = read_mchbar_base(mchbar);
    if (!NT_SUCCESS(status))
        return status;

    new raw = 0;
    new ratio = 0;
    new gear = IMC_GEAR_UNKNOWN;
    new refMode = IMC_REF_UNKNOWN;
    new flags = IMC_FLAG_EXPERIMENTAL;

    // Dispatch to the source helper that matches the platform. Each helper
    // owns the register's specific decoding and never reads the other
    // platform's register, so a wrong source can't sneak in via a code path
    // we forgot to update.
    switch (source) {
        case IMC_SRC_MCHBAR_MEMSS_PMA: {
            status = read_memss_pma(mchbar, raw, ratio, gear);
            if (!NT_SUCCESS(status))
                return status;
            // Core Ultra MEMSS_PMA is referenced to BCLK/3 and is the
            // value firmware writes after MRC, so it is effectively
            // static at runtime.
            refMode = IMC_REF_BCLK_DIV_3;
            flags |= IMC_FLAG_STATIC_LOCKED;
        }
        case IMC_SRC_MCHBAR_SA_PERF: {
            status = read_sa_perf_status(mchbar, raw, ratio, refMode);
            if (!NT_SUCCESS(status))
                return status;
            // SA_PERF_STATUS reflects the live System-Agent workpoint, so
            // the consumer can re-read periodically to track changes.
            flags |= IMC_FLAG_LIVE_CURRENT;
        }
        default:
            return STATUS_NOT_SUPPORTED;
    }

    out[1] = source;
    out[2] = ratio;
    out[3] = refMode;
    out[4] = gear;
    out[5] = raw & 0xFFFFFFFF;
    out[6] = flags;

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    // Intentionally do not gate on CPU model here. Returning success on
    // any Intel x64 lets users on unsupported models still load the
    // module and observe STATUS_NOT_SUPPORTED from the IOCTL itself,
    // which makes "module didn't load" easy to tell apart from "this CPU
    // is not on the allowlist".
    return STATUS_SUCCESS;
}
