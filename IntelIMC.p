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
// Reads the IMC QCLK ratio firmware programs during memory training, on
// platforms where the legacy MSR_UNCORE_PERF_STATUS path no longer applies
// because uncore and the IMC are decoupled.
//
// Read sources, selected per CPUID:
//   * MEMSS_PMA_CR_BIOS_DATA @ MCHBAR + 0x13D10 - Core Ultra (MTL/ARL/LNL/PTL,
//     experimental: NVL). Locked QCLK ratio against BCLK/3, static after MRC.
//   * SA_PERF_STATUS @ MCHBAR + 0x5918 - ADL/RPL. Live workpoint with per-
//     platform BCLK / BCLK*4/3 reference selector.
//   * IMC_LIVE_GV_STATUS_ARL @ MCHBAR + 0xE448 - ARL live workpoint.
//
// Security envelope:
//   * Read-only (no writes to PCI cfg / MMIO / MSR / IO).
//   * No caller-controlled physical addresses or PCI BDF.
//   * Fixed compile-time MCHBAR offsets only.
//   * Strict CPUID allowlist; unknown models return STATUS_NOT_SUPPORTED.
//   * Reserved-bits and ratio-range checks reject wrong-register reads.
//   * MCHBAR enable bit is observed, never set.
//
// Public references:
//   - Intel Core Ultra 200H/200U CFG/MEM reference (MEMSS_PMA, MCHBAR @ PCI 0/0/0 0x48)
//   - Intel 14th-gen client CFG/MEM reference (SA_PERF_STATUS @ MCHBAR + 0x5918)
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
#define IMC_SRC_MCHBAR_LIVE_GV_ARL  4   // MCHBAR + 0x5C    (ARL live SAGV workpoint)

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
// may be set. While EXPERIMENTAL is set, the value is best-effort and the
// consumer is expected to keep the sensor hidden or labelled accordingly.
#define IMC_FLAG_STATIC_LOCKED      (1 << 0)    // value latched after MRC, won't track SAGV
#define IMC_FLAG_LIVE_CURRENT       (1 << 1)    // value reflects the live workpoint
#define IMC_FLAG_EXPERIMENTAL       (1 << 2)    // best-effort, treat with care
// Bits 3 and above are reserved.

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
// MEMSS_PMA_CR_BIOS_DATA: locked Qclk ratio firmware programs after memory
// training. Same layout on MTL / ARL / LNL / PTL (and assumed on NVL).
//   bits 7:0  QCLK_RATIO   - controller QCLK multiplier of BCLK/3
//   bit 8     GEAR_TYPE    - 0 = Gear2, 1 = Gear4
#define MEMSS_PMA_CR_BIOS_DATA      0x13D10

// SA_PERF_STATUS: ADL/RPL System-Agent perf status, documented encoding.
//   bits 9:2  QCLK_RATIO     - controller QCLK multiplier of the reference
//   bit 10    QCLK_REFERENCE - 0 = BCLK*4/3 (133.33 MHz), 1 = BCLK (100 MHz)
// On Core Ultra Intel marks this field "not defined properly", but bits 9:2
// empirically track the live QCLK workpoint against BCLK/3 (PTL: 18..64
// across SAGV states, matches MEMSS_PMA scale). Bit 10 is informational on
// Core Ultra. This module uses SA_PERF_STATUS as a live signal on MTL/LNL/
// PTL and keeps the ADL/RPL decode path separate.
#define SA_PERF_STATUS              0x5918

// IMC_LIVE_GV_STATUS_ARL: ARL-S live workpoint register (the inherited 0x5918
// reads zero on ARL).
//   bits 7:0   QCLK_RATIO  - controller QCLK multiplier of BCLK/3
//   bits 31:8  unknown     - not used
// High-state value is `trained_max + 1` (consistent +1 PLL overshoot,
// cross-validated DDR5-7800 vs DDR5-7000); the live IOCTL clamps to
// MEMSS_PMA's trained max so output matches HWiNFO/CPU-Z. Mirrored at
// 0xEC48 / 0xF448 / 0x1E448 / 0x1EC48 / 0x1F448. Discovery + cross-BIOS
// evidence: Deploy/DETECT-ARL-LiveIMC{,-DDR7000}.md,
// Deploy/VERIFY-ARL-LoadCorrelation.md.
//
// Phase 2: migrate Core Ultra live reads to Intel PMT/CTL via PCIe DVSEC.
// PMT exposes canonical {floor, trained_max} without the +1 quirk. See
// PR_discussion.md for the migration trigger conditions.
#define IMC_LIVE_GV_STATUS_ARL      0xE448

// Range a freshly trained QCLK ratio is expected to fall in. The lower
// bound rejects 0/very-low values that would mean "register not populated"
// or "wrong source". The upper bound is generous enough to cover heavily
// overclocked DDR5-12000 (ratio ~180 against BCLK/3) without rejecting
// future SKUs. Anything outside this range we treat as "wrong register"
// and refuse rather than report.
#define IMC_RATIO_MIN               16
#define IMC_RATIO_MAX               220

// Reserved-bits mask for MEMSS_PMA. Bits 31:9 are reserved-zero on the
// 200H/200U layout (MTL, ARL). A nonzero value means a future-stepping
// repurposing or wrong-register read.
#define MEMSS_PMA_RESERVED_MASK_STRICT  0xFFFFFE00
// PTL (0xCC: 0x1E8B0000) and LNL (0xBD: 0x1CCC0000, on-package LPDDR5x)
// set bits in the 200H/200U-era reserved range; bits 7:0 still decode
// correctly (cross-checked against HWiNFO + SMBIOS). NVL is treated the
// same way until silicon validation lands.
#define MEMSS_PMA_RESERVED_MASK_NONE    0

// === Platform tags ===
// One tag per register-layout family this module decodes. Kept distinct so
// per-platform validated flags can be flipped independently.
#define PLAT_NONE                   0
#define PLAT_MTL                    1   // Core Ultra 100, Meteor Lake
#define PLAT_ARL                    2   // Core Ultra 200, Arrow Lake
#define PLAT_LNL                    3   // Core Ultra 200V, Lunar Lake
#define PLAT_PTL                    4   // Core Ultra series 3, Panther Lake
#define PLAT_ADL                    5   // 12th Gen, Alder Lake
#define PLAT_RPL                    6   // 13th/14th Gen, Raptor Lake
#define PLAT_NVL                    7   // Nova Lake (Family 18) - EXPERIMENTAL

// Map CPUID (family, model) to a platform tag. Models per intel-perfmon
// mapfile.csv (MTL V1.21, ARL V1.17, LNL V1.22, PTL V1.05, ADL/RPL V1.39).
// 0xCF is Emerald Rapids (server) - intentionally excluded. 0xD7 is Bartlett
// Lake (Raptor-Cove client core) - to be added with hardware validation.
// NVL Family-18 mapping (0x01, 0x03) per Linux arch/x86/include/asm/intel-
// family.h; routed as EXPERIMENTAL until silicon validation lands.
stock get_platform(family, model) {
    switch (family) {
        case 0x6: {
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
        }
        case 0x12: {
            // Nova Lake - INTEL_NOVALAKE = IFM(18, 0x01),
            //             INTEL_NOVALAKE_L = IFM(18, 0x03).
            switch (model) {
                case 0x01, 0x03:
                    return PLAT_NVL;
            }
        }
    }
    return PLAT_NONE;
}

// Pick the IMC source register for a platform. Core Ultra (incl. NVL,
// experimental) -> MEMSS_PMA. ADL/RPL -> SA_PERF_STATUS. No fall-through
// between sources.
stock get_platform_source(platform) {
    switch (platform) {
        case PLAT_MTL, PLAT_ARL, PLAT_LNL, PLAT_PTL, PLAT_NVL:
            return IMC_SRC_MCHBAR_MEMSS_PMA;
        case PLAT_ADL, PLAT_RPL:
            return IMC_SRC_MCHBAR_SA_PERF;
    }
    return IMC_SRC_NONE;
}

// Reserved-bits mask for MEMSS_PMA. MTL/ARL match the 200H/200U layout
// (strict). PTL/LNL/NVL set bits in the documented reserved range, so the
// gate is disabled and ratio-range carries the consistency burden.
stock get_platform_pma_reserved_mask(platform) {
    switch (platform) {
        case PLAT_MTL, PLAT_ARL:
            return MEMSS_PMA_RESERVED_MASK_STRICT;
        case PLAT_PTL, PLAT_LNL, PLAT_NVL:
            return MEMSS_PMA_RESERVED_MASK_NONE;
    }
    return MEMSS_PMA_RESERVED_MASK_STRICT;
}

// True when this platform's output has been cross-validated against
// HWiNFO / CPU-Z. False -> IMC_FLAG_EXPERIMENTAL is set on every return.
// Validation evidence per platform: Deploy/VALIDATION-{ARL,LNL,PTL}.md.
// MTL and NVL remain unvalidated.
stock bool:is_platform_validated(platform) {
    switch (platform) {
        case PLAT_ARL, PLAT_PTL, PLAT_LNL:
            return true;
    }
    return false;
}

// Read the firmware-published MCHBAR base. Disabled/zero -> NOT_SUPPORTED;
// this module never enables MCHBAR.
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

    // Mask each dword to 32 bits before combining so a high bit in lo
    // can't sign-extend into hi (cells are signed 64-bit).
    base = ((hi & 0xFFFFFFFF) << 32) | (lo & 0xFFFFFFFF);
    base = base & MCHBAR_BASE_MASK;

    if (base == 0)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// Read one 32-bit MMIO register at MCHBAR + offset. Offsets are compile-
// time constants only, never caller-controlled.
stock NTSTATUS:read_mchbar_dword(mchbar_base, offset, &value) {
    new VA:va = io_space_map(mchbar_base + offset, 4);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    new NTSTATUS:status = virtual_read_dword(va, value);

    io_space_unmap(va, 4);
    return status;
}

// Read MEMSS_PMA and split into ratio + gear. Reserved-bits check is
// platform-specific (skipped when reserved_mask == 0). Ratio out of the
// DDR5/LPDDR5 range -> wrong-register/wrong-platform.
stock NTSTATUS:read_memss_pma(mchbar_base, reserved_mask, &raw, &ratio, &gear) {
    new NTSTATUS:status = read_mchbar_dword(mchbar_base, MEMSS_PMA_CR_BIOS_DATA, raw);
    if (!NT_SUCCESS(status))
        return status;

    if (reserved_mask != 0 && (raw & reserved_mask) != 0)
        return STATUS_NOT_SUPPORTED;

    ratio = raw & 0xFF;
    gear = ((raw >> 8) & 0x1) ? IMC_GEAR_4 : IMC_GEAR_2;

    if (ratio < IMC_RATIO_MIN || ratio > IMC_RATIO_MAX)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// Read SA_PERF_STATUS on ADL/RPL (documented encoding). bit 10 picks BCLK
// vs BCLK*4/3 as reference. No gear field. Reserved bits not validated -
// the older client docs leave several fields here this module doesn't
// consume; ratio range carries the consistency check.
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

// Read SA_PERF_STATUS on Core Ultra: live workpoint, bits 9:2 against
// BCLK/3. bit 10 is informational on Core Ultra.
stock NTSTATUS:read_sa_perf_status_core_ultra(mchbar_base, &raw, &ratio) {
    new NTSTATUS:status = read_mchbar_dword(mchbar_base, SA_PERF_STATUS, raw);
    if (!NT_SUCCESS(status))
        return status;

    ratio = (raw >> 2) & 0xFF;

    if (ratio < IMC_RATIO_MIN || ratio > IMC_RATIO_MAX)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// Read the ARL live-workpoint register at MCHBAR + 0xE448 (bits 7:0 against
// BCLK/3). High state is `trained_max + 1` (PLL overshoot); caller clamps
// to MEMSS_PMA's trained max.
stock NTSTATUS:read_arl_live_gv_status(mchbar_base, &raw, &ratio) {
    new NTSTATUS:status = read_mchbar_dword(mchbar_base, IMC_LIVE_GV_STATUS_ARL, raw);
    if (!NT_SUCCESS(status))
        return status;

    ratio = raw & 0xFF;

    if (ratio < IMC_RATIO_MIN || ratio > IMC_RATIO_MAX)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

// === DEBUG / VALIDATION ONLY ===
// Bring-up diagnostic IOCTL: returns intermediate values + a step code for
// which check tripped. Same security envelope as production IOCTLs. To be
// removed once every allowlisted platform clears EXPERIMENTAL.
#define IMC_DBG_OK              0
#define IMC_DBG_FAIL_FAMILY     1
#define IMC_DBG_FAIL_PLATFORM   2
#define IMC_DBG_FAIL_SOURCE     3
#define IMC_DBG_FAIL_MCHBAR_OFF 4
#define IMC_DBG_FAIL_BASE_ZERO  5
#define IMC_DBG_FAIL_PMA_RSVD   6
#define IMC_DBG_FAIL_PMA_RANGE  7
#define IMC_DBG_FAIL_SA_RANGE   8

/// Diagnostic dump of the live IOCTL's intermediate values.
///
/// @param in_size Must be 0
/// @param out [0] = ABI version
/// @param out [1] = CPUID FMS (family<<16 | model<<8 | stepping)
/// @param out [2] = platform tag (PLAT_*)
/// @param out [3] = source tag  (IMC_SRC_*)
/// @param out [4] = MCHBAR low dword (raw, includes enable bit)
/// @param out [5] = MCHBAR high dword (raw)
/// @param out [6] = MCHBAR base after mask (0 if disabled)
/// @param out [7] = raw MEMSS_PMA_CR_BIOS_DATA dword (0 if not read)
/// @param out [8] = raw SA_PERF_STATUS dword         (0 if not read)
/// @param out [9] = step that would have failed live IOCTL (IMC_DBG_*)
/// @param out_size Must be 10
/// @return STATUS_SUCCESS even on a "would have failed" run; the step code
///         in out[9] tells you why.
DEFINE_IOCTL_SIZED(ioctl_read_imc_clock_dbg, 0, 10) {
    out[0] = IMC_CLOCK_ABI_VERSION;
    out[1] = 0; out[2] = 0; out[3] = 0;
    out[4] = 0; out[5] = 0; out[6] = 0;
    out[7] = 0; out[8] = 0; out[9] = IMC_DBG_OK;

    new fms = get_cpu_fms();
    out[1] = fms;
    new family = cpu_fms_family(fms);
    if (family != 0x6 && family != 0x12) {
        out[9] = IMC_DBG_FAIL_FAMILY;
        return STATUS_SUCCESS;
    }

    new platform = get_platform(family, cpu_fms_model(fms));
    out[2] = platform;
    if (platform == PLAT_NONE) {
        out[9] = IMC_DBG_FAIL_PLATFORM;
        return STATUS_SUCCESS;
    }

    new source = get_platform_source(platform);
    out[3] = source;
    if (source == IMC_SRC_NONE) {
        out[9] = IMC_DBG_FAIL_SOURCE;
        return STATUS_SUCCESS;
    }

    new lo = 0;
    new hi = 0;
    pci_config_read_dword(HOSTBRIDGE_BUS, HOSTBRIDGE_DEV, HOSTBRIDGE_FUNC,
        HOSTBRIDGE_MCHBAR_LO, lo);
    pci_config_read_dword(HOSTBRIDGE_BUS, HOSTBRIDGE_DEV, HOSTBRIDGE_FUNC,
        HOSTBRIDGE_MCHBAR_HI, hi);
    out[4] = lo & 0xFFFFFFFF;
    out[5] = hi & 0xFFFFFFFF;

    if ((lo & MCHBAR_ENABLE_BIT) == 0) {
        out[9] = IMC_DBG_FAIL_MCHBAR_OFF;
        return STATUS_SUCCESS;
    }

    new base = ((hi & 0xFFFFFFFF) << 32) | (lo & 0xFFFFFFFF);
    base = base & MCHBAR_BASE_MASK;
    out[6] = base;
    if (base == 0) {
        out[9] = IMC_DBG_FAIL_BASE_ZERO;
        return STATUS_SUCCESS;
    }

    new raw_pma = 0;
    new raw_sa  = 0;
    read_mchbar_dword(base, MEMSS_PMA_CR_BIOS_DATA, raw_pma);
    read_mchbar_dword(base, SA_PERF_STATUS,         raw_sa);
    out[7] = raw_pma & 0xFFFFFFFF;
    out[8] = raw_sa  & 0xFFFFFFFF;

    if (source == IMC_SRC_MCHBAR_MEMSS_PMA) {
        new pma_mask = get_platform_pma_reserved_mask(platform);
        if (pma_mask != 0 && (raw_pma & pma_mask) != 0) {
            out[9] = IMC_DBG_FAIL_PMA_RSVD;
        } else {
            new r = raw_pma & 0xFF;
            if (r < IMC_RATIO_MIN || r > IMC_RATIO_MAX)
                out[9] = IMC_DBG_FAIL_PMA_RANGE;
        }
    } else if (source == IMC_SRC_MCHBAR_SA_PERF) {
        new r = (raw_sa >> 2) & 0xFF;
        if (r < IMC_RATIO_MIN || r > IMC_RATIO_MAX)
            out[9] = IMC_DBG_FAIL_SA_RANGE;
    }

    return STATUS_SUCCESS;
}

// === MCHBAR window dump (diagnostic) ===
// Returns 64 dwords starting at a caller-supplied byte offset into MCHBAR.
// Used during platform bring-up to locate new registers without rebuilding.
// Security envelope: offset clamped to [0, 0x1FF00] and dword-aligned,
// same CPUID allowlist as production IOCTLs, MCHBAR enable-bit checked,
// read-only, no caller-supplied absolute address.
#define MCHBAR_WINDOW_DWORDS    64
#define MCHBAR_WINDOW_BYTES     (MCHBAR_WINDOW_DWORDS * 4)
#define MCHBAR_REGION_BYTES     0x20000      // 128 kB documented MCHBAR size
#define MCHBAR_OFFSET_MAX       (MCHBAR_REGION_BYTES - MCHBAR_WINDOW_BYTES)

/// Dump a 64-dword (256-byte) window of MCHBAR.
///
/// @param in [0]  = byte offset into MCHBAR. Must be dword-aligned and
///                  satisfy 0 <= offset <= 0x1FF00 so the 256-byte window
///                  stays inside the documented 128 kB MCHBAR region.
/// @param out [0..63] = 64 consecutive dwords starting at MCHBAR + offset.
/// @return STATUS_SUCCESS on success.
///         STATUS_INVALID_PARAMETER if the offset is misaligned or out of
///         range. STATUS_NOT_SUPPORTED if the CPU is not on the allowlist
///         or MCHBAR is firmware-disabled. Other NTSTATUS on PCI/MMIO read
///         failure.
DEFINE_IOCTL_SIZED(ioctl_dump_mchbar_window, 1, MCHBAR_WINDOW_DWORDS) {
    new offset = in[0];
    if (offset < 0 || offset > MCHBAR_OFFSET_MAX)
        return STATUS_INVALID_PARAMETER;
    if ((offset & 0x3) != 0)
        return STATUS_INVALID_PARAMETER;

    // Same CPUID gate as the production IOCTLs.
    new fms = get_cpu_fms();
    new family = cpu_fms_family(fms);
    if (family != 0x6 && family != 0x12)
        return STATUS_NOT_SUPPORTED;
    new platform = get_platform(family, cpu_fms_model(fms));
    if (platform == PLAT_NONE)
        return STATUS_NOT_SUPPORTED;

    new mchbar = 0;
    new NTSTATUS:status = read_mchbar_base(mchbar);
    if (!NT_SUCCESS(status))
        return status;

    // Per-dword map/unmap. A single 256-byte mapping with va+i*4 silently
    // aliased to the first dword on this runtime; the per-dword pattern is
    // the proven workaround.
    for (new i = 0; i < MCHBAR_WINDOW_DWORDS; i++) {
        new value = 0;
        new NTSTATUS:s = read_mchbar_dword(mchbar, offset + i * 4, value);
        if (!NT_SUCCESS(s))
            return s;
        out[i] = value & 0xFFFFFFFF;
    }

    return STATUS_SUCCESS;
}

/// Read Intel client IMC/QCLK trained-max ratio.
///
/// CPUID allowlist: ADL/RPL/MTL/ARL/LNL/PTL (and NVL, EXPERIMENTAL).
/// Older clients with MSR_UNCORE_PERF_STATUS remain on IntelMSR.
///
/// Returns ratio + reference clock mode + gear so the consumer can compute
/// memory clock with its measured BCLK; the multiplication stays on the
/// consumer side to keep this module free of floating point and UI
/// semantics. While IMC_FLAG_EXPERIMENTAL is set, the consumer must keep
/// the resulting sensor hidden or labelled experimental.
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

    // Resolve CPU -> platform tag -> register source. Anything outside the
    // allowlist is rejected before we touch any bus or memory.
    new fms = get_cpu_fms();
    new family = cpu_fms_family(fms);
    if (family != 0x6 && family != 0x12)
        return STATUS_NOT_SUPPORTED;

    new platform = get_platform(family, cpu_fms_model(fms));
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
    new flags = is_platform_validated(platform) ? 0 : IMC_FLAG_EXPERIMENTAL;

    // Dispatch to the source helper that matches the platform. Each helper
    // owns the register's specific decoding and never reads the other
    // platform's register, so a wrong source can't sneak in via a code path
    // we forgot to update.
    switch (source) {
        case IMC_SRC_MCHBAR_MEMSS_PMA: {
            status = read_memss_pma(mchbar, get_platform_pma_reserved_mask(platform), raw, ratio, gear);
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

/// Read the live IMC/QCLK workpoint on Core Ultra (MTL/ARL/LNL/PTL, NVL).
///
/// `ioctl_read_imc_clock` returns the static trained-max ratio MEMSS_PMA
/// holds after MRC; this IOCTL returns the LIVE ratio the controller runs
/// at. On ADL/RPL it returns STATUS_NOT_SUPPORTED (use the static IOCTL -
/// it is already live there via SA_PERF).
///
/// Source register depends on platform:
///   * MTL/LNL/PTL/NVL: MCHBAR + 0x5918 (SA_PERF_STATUS, bits 9:2, BCLK/3)
///   * ARL:             MCHBAR + 0xE448 (IMC_LIVE_GV_STATUS_ARL, bits 7:0,
///                       BCLK/3, clamped to MEMSS_PMA's trained max)
///
/// Caller should poll at ~1 Hz; the live ratio drops well below trained
/// max during low memory activity.
///
/// @param in_size Must be 0
/// @param out [0] = ABI version (currently 1)
/// @param out [1] = source enum (IMC_SRC_MCHBAR_SA_PERF on MTL/LNL/PTL,
///                   IMC_SRC_MCHBAR_LIVE_GV_ARL on ARL)
/// @param out [2] = ratio (live controller QCLK multiplier of BCLK/3)
/// @param out [3] = reference clock mode (IMC_REF_BCLK_DIV_3)
/// @param out [4] = gear (carried from MEMSS_PMA, since neither live
///                   register encodes gear; 0 if MEMSS_PMA refused)
/// @param out [5] = raw register dword the ratio was decoded from
/// @param out [6] = flags (always sets LIVE_CURRENT; sets EXPERIMENTAL
///                   when the running platform is not yet validated)
/// @param out_size Must be 7
/// @return STATUS_SUCCESS on a Core Ultra platform whose live ratio is in
///         range. STATUS_NOT_SUPPORTED on ADL/RPL or unknown CPUs. Other
///         NTSTATUS on PCI/MMIO read failure.
DEFINE_IOCTL_SIZED(ioctl_read_imc_clock_live, 0, 7) {
    out[0] = IMC_CLOCK_ABI_VERSION;
    out[1] = IMC_SRC_NONE;
    out[2] = 0;
    out[3] = IMC_REF_UNKNOWN;
    out[4] = IMC_GEAR_UNKNOWN;
    out[5] = 0;
    out[6] = 0;

    new fms = get_cpu_fms();
    new family = cpu_fms_family(fms);
    if (family != 0x6 && family != 0x12)
        return STATUS_NOT_SUPPORTED;

    new platform = get_platform(family, cpu_fms_model(fms));
    if (platform == PLAT_NONE)
        return STATUS_NOT_SUPPORTED;

    // Only Core Ultra parts use this live path. ADL/RPL get the documented
    // ADL encoding via the existing ioctl_read_imc_clock.
    new source = get_platform_source(platform);
    if (source != IMC_SRC_MCHBAR_MEMSS_PMA)
        return STATUS_NOT_SUPPORTED;

    new mchbar = 0;
    new NTSTATUS:status = read_mchbar_base(mchbar);
    if (!NT_SUCCESS(status))
        return status;

    // Pull gear + trained_max from MEMSS_PMA first; trained_max feeds the
    // ARL clamp. On reserved-bits/range failure we fall back to Unknown
    // gear and skip the clamp rather than failing the IOCTL.
    new pma_raw = 0;
    new pma_ratio = 0;
    new gear = IMC_GEAR_UNKNOWN;
    new NTSTATUS:pma_status = read_memss_pma(mchbar, get_platform_pma_reserved_mask(platform), pma_raw, pma_ratio, gear);
    if (!NT_SUCCESS(pma_status)) {
        gear = IMC_GEAR_UNKNOWN;
        pma_ratio = 0;
    }

    new raw = 0;
    new ratio = 0;
    new live_source = IMC_SRC_NONE;
    if (platform == PLAT_ARL) {
        status = read_arl_live_gv_status(mchbar, raw, ratio);
        live_source = IMC_SRC_MCHBAR_LIVE_GV_ARL;
        // ARL clamp: high state encodes trained_max + 1 (PLL overshoot);
        // clamp to MEMSS_PMA so output matches HWiNFO/CPU-Z. Skip if
        // pma_ratio == 0 (MEMSS_PMA read failed) - reporting the raw +1
        // beats silently zeroing a valid signal.
        if (NT_SUCCESS(status) && pma_ratio > 0 && ratio > pma_ratio)
            ratio = pma_ratio;
    } else {
        status = read_sa_perf_status_core_ultra(mchbar, raw, ratio);
        live_source = IMC_SRC_MCHBAR_SA_PERF;
    }
    if (!NT_SUCCESS(status))
        return status;

    new live_flags = IMC_FLAG_LIVE_CURRENT;
    if (!is_platform_validated(platform))
        live_flags |= IMC_FLAG_EXPERIMENTAL;

    out[1] = live_source;
    out[2] = ratio;
    out[3] = IMC_REF_BCLK_DIV_3;
    out[4] = gear;
    out[5] = raw & 0xFFFFFFFF;
    out[6] = live_flags;

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    // No model gate here; per-IOCTL allowlist returns STATUS_NOT_SUPPORTED
    // so callers can distinguish "didn't load" from "model not allowlisted".
    return STATUS_SUCCESS;
}
