//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Adrenalift, CapFrameX, and ARX HUD contributors
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

/*
 * RadeonSMU - SMU mailbox and bounded telemetry access for AMD Radeon
 * RDNA 3 (Navi 3x / RX 7000) and RDNA 4 (Navi 4x / RX 9000) GPUs.
 *
 * Replaces WinRing0/InpOut32-style raw physical MMIO for AMD GPU monitoring
 * and tuning software. Rather than handing out a physical read/write
 * primitive, this module owns the SMN index/data sequence and allowlists
 * the exact SMN addresses and fixed telemetry transactions a caller may
 * reach.
 *
 * Discovery
 * ---------
 * The module finds the AMD display device itself by PCI scan and reads its
 * own BAR bases and sizes; the caller never supplies a bus/device or a
 * physical address for register access. Selection is by LARGEST VRAM
 * aperture rather than enumeration order, so an AMD iGPU that enumerates
 * first (e.g. Raphael, 256 MB carved from system RAM) cannot shadow a
 * discrete card. `ioctl_get_bounds` and `ioctl_get_device_info` let the
 * caller confirm which device was chosen.
 *
 * Discovery is strictly read-only: the VRAM aperture size comes from the
 * PCIe Resizable BAR capability (`rebar_current_size`), never from a
 * write-all-ones BAR probe. Only discrete Navi 3x and Navi 4x PCI device IDs
 * are admitted (`is_supported_gpu`); APUs and CDNA parts are excluded.
 *
 * Exposed surface
 * ---------------
 *   - SMN reads/writes restricted to the MP1 C2PMSG register file
 *     (0x03B10900..0x03B10AFF, 128 registers). That window is the SMU
 *     message mailbox: MSG (C2PMSG_66), PARAM (C2PMSG_82), RESP
 *     (C2PMSG_90), and the addr_hi/lo pair (C2PMSG_80/81) that PMFW
 *     populates for TransferTableSmu2DramWithAddr.
 *   - Fixed-size read of the SmuMetrics_t DMA buffer (`ioctl_read_metrics`),
 *     bounded to the GPU's own VRAM aperture.
 *   - Fixed private monitoring-table transaction (`ioctl_read_rdna_tool_table`)
 *     for allowlisted RDNA 3 and RDNA 4 GPUs. All register offsets, mailbox
 *     command IDs, selector, and read length (8 KiB) are compile-time fixed;
 *     the firmware-reported GPU/MC address is validated against both the
 *     hardware framebuffer interval and the PCIe BAR0 ReBAR aperture.
 *   - All `io_space_map` mappings (SMN window, BAR5 tool mailbox window,
 *     and resolved VRAM table buffers) are lazily cached and released in
 *     `unload()` to avoid per-poll cross-CPU TLB shootdown IPIs.
 *   - Nothing else. The module touches no system RAM, no other device, and
 *     no SMN address outside the mailbox window, and it never writes to the
 *     framebuffer.
 *
 * Reference
 * ---------
 *   - Linux amdgpu: `drivers/gpu/drm/amd/pm/swsmu/smu_cmn.c` (mailbox
 *     protocol) and `amdgpu_device.c` (PCIE_INDEX2/PCIE_DATA2 indirect SMN).
 *   - MP1 C2PMSG register numbering:
 *     `drivers/gpu/drm/amd/include/asic_reg/mp/mp_13_0_0_offset.h`
 *     (`regMP1_SMN_C2PMSG_66` = 0x0282, `_72` = 0x0288, `_80` = 0x0290,
 *     `_81` = 0x0291, `_82` = 0x0292, `_90` = 0x029A, `_96` = 0x02A0,
 *     `_98` = 0x02A2, `_109` = 0x02AD) and `navi10_ip_offset.h`
 *     (`MP1_BASE` seg0 = 0x16000 dwords -> BAR5 byte base 0x58000, so
 *     C2PMSG_72 = 0x58A20, C2PMSG_96 = 0x58A80, C2PMSG_98 = 0x58A88,
 *     C2PMSG_109 = 0x58AB4).
 *   - SMU metrics headers: `smu13_driver_if_v13_0_0.h` (Navi 31/32),
 *     `smu13_driver_if_v13_0_7.h` (Navi 33), `smu14_driver_if_v14_0.h`
 *     (Navi 4x).
 *   - Resizable BAR capability: PCI Express Base Specification, "Resizable
 *     BAR Extended Capability"; Linux `drivers/pci/pci.c`
 *     `pci_rebar_find_pos` / `pci_rebar_get_current_size`.
 */

#include <pawnio.inc>

/// Module ABI version reported by `ioctl_get_device_info`.
const MODULE_ABI_VERSION = 2;

/// PCI vendor id for AMD/ATI.
const AMD_VENDOR_ID = 0x1002;

/// PCI COMMAND register offset, and its memory-space decode bit.
const PCI_CFG_COMMAND = 0x04;
const PCI_COMMAND_MEMORY = 0x02;

/* PCIe extended config space: 4 KB, capability chain starts at 0x100. */
const PCI_EXT_CFG_BASE = 0x100;
const PCI_EXT_CFG_SIZE = 0x1000;
/// Extended capability id of the Resizable BAR capability.
const PCI_EXT_CAP_ID_REBAR = 0x0015;
/// Bound on the capability chain walk.
const PCI_EXT_CAP_MAX_HOPS = 64;

/* Resizable BAR capability layout, relative to its header. */
const PCI_REBAR_CTRL = 0x08;
const PCI_REBAR_ENTRY_STRIDE = 0x08;
const PCI_REBAR_CTRL_BAR_IDX = 0x07;
const PCI_REBAR_CTRL_NBAR_SHIFT = 5;
const PCI_REBAR_CTRL_NBAR_MASK = 0x07;
const PCI_REBAR_CTRL_SIZE_SHIFT = 8;
const PCI_REBAR_CTRL_SIZE_MASK = 0x3F;
/// Largest size encoding acted on; 2^(42+20) stays clear of the sign bit.
const PCI_REBAR_SIZE_ENCODING_MAX = 42;

/* Navi 3x (RDNA 3 / RX 7000) PCI device ID ranges from Linux amdgpu_drv.c
 * and Adrenalin INF (32.0.31041):
 *   - 0x7448..0x745E: Navi 31 (RX 7900 XTX / XT / GRE, PRO W7900 / W7800)
 *   - 0x7470..0x747F: Navi 32 (RX 7800 XT 0x747E, RX 7700 XT, PRO W7700)
 *   - 0x7480..0x7499: Navi 33 (RX 7600 / 7600 XT / 7700S, PRO W7600 / W7500)
 * CDNA Aldebaran (0x7408..0x7410) and all AMD APUs remain strictly excluded. */
const NAVI3X_DEVICE_ID_MIN_0 = 0x7448;
const NAVI3X_DEVICE_ID_MAX_0 = 0x745E;
const NAVI3X_DEVICE_ID_MIN_1 = 0x7470;
const NAVI3X_DEVICE_ID_MAX_1 = 0x747F;
const NAVI3X_DEVICE_ID_MIN_2 = 0x7480;
const NAVI3X_DEVICE_ID_MAX_2 = 0x7499;

/* Navi 4x (RDNA 4 / RX 9000) PCI device ID ranges from Adrenalin INF:
 *   - 0x7550..0x756F: Navi 48 (RX 9070 / 9070 XT)
 *   - 0x7590..0x75AF: Navi 44 (RX 9060 / 9060 XT) */
const NAVI4X_DEVICE_ID_MIN_0 = 0x7550;
const NAVI4X_DEVICE_ID_MAX_0 = 0x756F;
const NAVI4X_DEVICE_ID_MIN_1 = 0x7590;
const NAVI4X_DEVICE_ID_MAX_1 = 0x75AF;

/// BAR5 offset of PCIE_INDEX2 - the SMN address (index) register.
const SMN_INDEX_OFFSET = 0x38;
/// BAR5 offset of PCIE_DATA2 - the SMN data register.
const SMN_DATA_OFFSET = 0x3C;

/// First SMN address of the MP1 C2PMSG register file (C2PMSG_0).
const MP1_C2PMSG_BASE = 0x03B10900;
/// Byte span of the C2PMSG register file: 128 registers of 4 bytes.
const MP1_C2PMSG_SPAN = 0x200;

/// Dword count of `SmuMetrics_t` on Navi 4x (260 bytes).
const SMU_METRICS_DWORDS = 65;

/* Fixed BAR5 offsets for the RDNA 3 / RDNA 4 monitoring-tool SMU mailbox.
 * Derived from MP1_BASE (0x16000 dwords = 0x58000 bytes) plus mp_13_0_0_offset.h:
 *   - C2PMSG_72  (0x0288 * 4 + 0x58000) = 0x58A20 (message ID)
 *   - C2PMSG_96  (0x02A0 * 4 + 0x58000) = 0x58A80 (response)
 *   - C2PMSG_98  (0x02A2 * 4 + 0x58000) = 0x58A88 (argument/result)
 *   - C2PMSG_109 (0x02AD * 4 + 0x58000) = 0x58AB4 (layout-10 argument) */
const RDNA_TOOL_MESSAGE_OFFSET = 0x58A20;
const RDNA_TOOL_RESPONSE_OFFSET = 0x58A80;
const RDNA_TOOL_ARGUMENT_OFFSET = 0x58A88;
const RDNA_TOOL_ARG_V10_OFFSET = 0x58AB4;
const RDNA_TOOL_GET_VERSION = 0x14;
const RDNA_TOOL_GET_ADDRESS_HIGH = 0x07;
const RDNA_TOOL_GET_ADDRESS_LOW = 0x08;
const RDNA_TOOL_REFRESH_TABLE = 0x09;
const RDNA_TOOL_REFRESH_SELECTOR = 4;
const RDNA_TOOL_RESPONSE_OK = 1;
const RDNA_TOOL_RESPONSE_BUSY = 0xFC;
const RDNA_TOOL_RSP_PREREQ = 0xFD;
const RDNA_TOOL_RESPONSE_UNKNOWN = 0xFE;

/// Spin retry limit for SMU mailbox polling (matches RyzenSMU's SMU_RETRIES_MAX;
/// no `microsleep` so a timeout never holds the ioctl lock for timer ticks).
const RDNA_TOOL_POLL_RETRIES = 8192;
const RDNA_TOOL_READ_ATTEMPTS = 5;

/* RDNA 3 / RDNA 4 hardware framebuffer location bounds in BAR5 (bits [23:0] << 24). */
const RDNA3_FB_BASE_OFFSET = 0xE4D4;
const RDNA3_FB_TOP_OFFSET = 0xE4D8;

/// Covers all fixed tool mailbox & FB registers and fits RDNA 3/4 BAR5 (>= 512 KiB).
const RDNA_TOOL_MMIO_MAP_SIZE = 0x80000;
const RDNA_TOOL_TABLE_BYTES = 0x2000;
const RDNA_TOOL_TABLE_QWORDS = RDNA_TOOL_TABLE_BYTES / 8;
const RDNA_TOOL_METADATA_QWORDS = 4;
const RDNA_TOOL_OUTPUT_QWORDS = RDNA_TOOL_METADATA_QWORDS + RDNA_TOOL_TABLE_QWORDS;

/// Declared register-BAR map bound (RDNA 3/4 BAR5 is at least 512 KiB / 3 MiB).
const REG_SPAN = 0x80000;
/// Sanity bound on the ReBAR-reported BAR0 size; larger is a bad read.
const VRAM_SIZE_MAX = 0x1000000000;  /* 64 GB */

/* Discovered at load (main()). */
new g_ready = 0;
new g_pci_bus = 0;
new g_pci_dev = 0;
new g_device_id = 0;
new g_revision_id = 0;
new g_subsys_vid = 0;
new g_subsys_did = 0;
new g_reg_bar = 0;    /* BAR5 base - MMIO register aperture   */
new g_reg_size = 0;   /* declared register map bound          */
new g_vram_bar = 0;   /* BAR0 base - VRAM aperture            */
new g_vram_size = 0;  /* BAR0 size, from the ReBAR capability */

/* Lazily cached IO space mappings (released in unload()) to prevent per-poll
 * cross-CPU TLB shootdown IPIs. */
new VA:g_smn_va = NULL;
new VA:g_tool_mmio_va = NULL;
new VA:g_metrics_va = NULL;
new g_metrics_pa = 0;
new g_metrics_len = 0;
new VA:g_tool_table_va = NULL;
new g_tool_table_pa = 0;

/* ---------------------------------------------------------------------
 * Discovery
 * ------------------------------------------------------------------- */

/// True iff `device_id` is an allowlisted discrete Navi 3x (RDNA 3) GPU.
bool: is_navi3x_gpu(device_id) {
    if (device_id >= NAVI3X_DEVICE_ID_MIN_0 && device_id <= NAVI3X_DEVICE_ID_MAX_0) return true;
    if (device_id >= NAVI3X_DEVICE_ID_MIN_1 && device_id <= NAVI3X_DEVICE_ID_MAX_1) return true;
    if (device_id >= NAVI3X_DEVICE_ID_MIN_2 && device_id <= NAVI3X_DEVICE_ID_MAX_2) return true;
    return false;
}

/// True iff `device_id` is an allowlisted discrete Navi 4x (RDNA 4) GPU.
bool: is_navi4x_gpu(device_id) {
    if (device_id >= NAVI4X_DEVICE_ID_MIN_0 && device_id <= NAVI4X_DEVICE_ID_MAX_0) return true;
    if (device_id >= NAVI4X_DEVICE_ID_MIN_1 && device_id <= NAVI4X_DEVICE_ID_MAX_1) return true;
    return false;
}

/// True iff `device_id` is an allowlisted discrete Navi 3x or Navi 4x GPU.
/// Security gate: rejects AMD APUs (whose MP1 C2PMSG window is the CPU SMU)
/// and CDNA accelerators.
bool: is_supported_gpu(device_id) {
    return is_navi3x_gpu(device_id) || is_navi4x_gpu(device_id);
}

/// Current size in bytes of `bar_index`, from the Resizable BAR capability.
NTSTATUS:rebar_current_size(bus, dev, bar_index, &size) {
    size = 0;

    new off = PCI_EXT_CFG_BASE;
    for (new hop = 0; hop < PCI_EXT_CAP_MAX_HOPS; hop++) {
        if (off < PCI_EXT_CFG_BASE || off > PCI_EXT_CFG_SIZE - 4) return STATUS_NOT_FOUND;

        new hdr = 0;
        new NTSTATUS:s = pci_config_read_dword(bus, dev, 0, off, hdr);
        if (s != STATUS_SUCCESS) return s;
        if (hdr == 0 || (hdr & 0xFFFFFFFF) == 0xFFFFFFFF) return STATUS_NOT_FOUND;

        if ((hdr & 0xFFFF) == PCI_EXT_CAP_ID_REBAR) {
            new ctrl0 = 0;
            s = pci_config_read_dword(bus, dev, 0, off + PCI_REBAR_CTRL, ctrl0);
            if (s != STATUS_SUCCESS) return s;
            new entries = (ctrl0 >>> PCI_REBAR_CTRL_NBAR_SHIFT) & PCI_REBAR_CTRL_NBAR_MASK;
            if (entries < 1 || entries > 6) return STATUS_NOT_FOUND;

            for (new i = 0; i < entries; i++) {
                new slot = off + PCI_REBAR_CTRL + i * PCI_REBAR_ENTRY_STRIDE;
                if (slot > PCI_EXT_CFG_SIZE - 4) return STATUS_NOT_FOUND;

                new ctrl = 0;
                s = pci_config_read_dword(bus, dev, 0, slot, ctrl);
                if (s != STATUS_SUCCESS) return s;
                if ((ctrl & PCI_REBAR_CTRL_BAR_IDX) != bar_index) continue;

                new enc = (ctrl >>> PCI_REBAR_CTRL_SIZE_SHIFT) & PCI_REBAR_CTRL_SIZE_MASK;
                if (enc > PCI_REBAR_SIZE_ENCODING_MAX) return STATUS_NOT_SUPPORTED;
                size = 1 << (enc + 20);
                return STATUS_SUCCESS;
            }
            return STATUS_NOT_FOUND;
        }

        off = (hdr >>> 20) & 0xFFF;
    }
    return STATUS_NOT_FOUND;
}

/// Find the supported AMD VGA controller with the largest VRAM aperture
/// and read its register and VRAM BAR bases. Strictly read-only.
find_gpu_and_probe() {
    new best_bus = -1, best_dev = -1;
    new best_vram_bar = 0, best_vram_size = 0;
    new best_did = 0, best_rev = 0, best_subsys = 0;

    for (new bus = 0; bus <= 255; bus++) {
        for (new dev = 0; dev < 32; dev++) {
            new vd = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x00, vd) != STATUS_SUCCESS) continue;
            if ((vd & 0xFFFF) != AMD_VENDOR_ID) continue;
            new did = (vd >>> 16) & 0xFFFF;
            if (!is_supported_gpu(did)) continue;

            new cls = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x08, cls) != STATUS_SUCCESS) continue;
            if (((cls >>> 24) & 0xFF) != 0x03) continue;   /* base class: display */
            if (((cls >>> 16) & 0xFF) != 0x00) continue;   /* sub class:  VGA     */

            new cmd = 0;
            if (pci_config_read_word(bus, dev, 0, PCI_CFG_COMMAND, cmd) != STATUS_SUCCESS) continue;
            if ((cmd & PCI_COMMAND_MEMORY) == 0) continue;

            new b0lo = 0, b0hi = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x10, b0lo) != STATUS_SUCCESS) continue;
            if ((b0lo & 0x1) != 0) continue;            /* I/O space  */
            if (((b0lo >>> 1) & 0x3) != 0x2) continue;  /* not 64-bit */
            if (pci_config_read_dword(bus, dev, 0, 0x14, b0hi) != STATUS_SUCCESS) continue;
            new vram_bar = ((b0hi & 0xFFFFFFFF) << 32) | (b0lo & 0xFFFFFFF0);
            if (vram_bar <= 0) continue;

            new vram_size = 0;
            if (rebar_current_size(bus, dev, 0, vram_size) != STATUS_SUCCESS) continue;
            if (vram_size <= 0 || vram_size > VRAM_SIZE_MAX) continue;

            if (vram_size > best_vram_size ||
                (vram_size == best_vram_size && bus > best_bus)) {
                new subsys = 0;
                pci_config_read_dword(bus, dev, 0, 0x2C, subsys);
                best_vram_size = vram_size;
                best_vram_bar = vram_bar;
                best_bus = bus;
                best_dev = dev;
                best_did = did;
                best_rev = cls & 0xFF;
                best_subsys = subsys;
            }
        }
    }

    if (best_bus < 0) return;

    new b5 = 0;
    if (pci_config_read_dword(best_bus, best_dev, 0, 0x24, b5) != STATUS_SUCCESS)
        return;
    if ((b5 & 0x1) != 0) return;                /* must be memory space */
    if (((b5 >>> 1) & 0x3) != 0x0) return;      /* BAR5 must be 32-bit  */
    new reg_bar = b5 & 0xFFFFFFF0;
    if (reg_bar <= 0)
        return;

    g_pci_bus = best_bus;
    g_pci_dev = best_dev;
    g_device_id = best_did;
    g_revision_id = best_rev;
    g_subsys_vid = best_subsys & 0xFFFF;
    g_subsys_did = (best_subsys >>> 16) & 0xFFFF;
    g_reg_bar = reg_bar;
    g_reg_size = REG_SPAN;
    g_vram_bar = best_vram_bar;
    g_vram_size = best_vram_size;
    g_ready = 1;
}

/* ---------------------------------------------------------------------
 * Bounds & Cached IO Space Helpers
 * ------------------------------------------------------------------- */

/// True iff [pa, pa+len) lies entirely within [base, base+size).
/// Overflow-safe for signed 64-bit Pawn cells.
bool: in_window(pa, len, base, size) {
    if (size <= 0) return false;
    if (len <= 0) return false;
    if (pa < base) return false;
    new off = pa - base;
    if (off > size) return false;
    if (len > size - off) return false;
    return true;
}

/// True iff `smn_addr` is a dword-aligned address inside the MP1 C2PMSG file.
bool: smn_allowed(smn_addr) {
    if ((smn_addr & 0x3) != 0) return false;
    return in_window(smn_addr, 4, MP1_C2PMSG_BASE, MP1_C2PMSG_SPAN);
}

unmap_all_cached() {
    if (g_smn_va != NULL) {
        io_space_unmap(g_smn_va, 8);
        g_smn_va = NULL;
    }
    if (g_tool_mmio_va != NULL) {
        io_space_unmap(g_tool_mmio_va, RDNA_TOOL_MMIO_MAP_SIZE);
        g_tool_mmio_va = NULL;
    }
    if (g_metrics_va != NULL) {
        io_space_unmap(g_metrics_va, g_metrics_len);
        g_metrics_va = NULL;
    }
    g_metrics_pa = 0;
    g_metrics_len = 0;
    if (g_tool_table_va != NULL) {
        io_space_unmap(g_tool_table_va, RDNA_TOOL_TABLE_BYTES);
        g_tool_table_va = NULL;
    }
    g_tool_table_pa = 0;
}

NTSTATUS:ensure_smn_mapped() {
    if (g_smn_va != NULL) return STATUS_SUCCESS;
    g_smn_va = io_space_map(g_reg_bar + SMN_INDEX_OFFSET, 8);
    if (g_smn_va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    return STATUS_SUCCESS;
}

NTSTATUS:ensure_tool_mmio_mapped() {
    if (g_tool_mmio_va != NULL) return STATUS_SUCCESS;
    g_tool_mmio_va = io_space_map(g_reg_bar, RDNA_TOOL_MMIO_MAP_SIZE);
    if (g_tool_mmio_va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    return STATUS_SUCCESS;
}

NTSTATUS:ensure_metrics_mapped(pa, len) {
    if (g_metrics_va != NULL && g_metrics_pa == pa && g_metrics_len == len)
        return STATUS_SUCCESS;
    if (g_metrics_va != NULL) {
        io_space_unmap(g_metrics_va, g_metrics_len);
        g_metrics_va = NULL;
        g_metrics_pa = 0;
        g_metrics_len = 0;
    }
    new VA:va = io_space_map(pa, len);
    if (va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    g_metrics_va = va;
    g_metrics_pa = pa;
    g_metrics_len = len;
    return STATUS_SUCCESS;
}

NTSTATUS:ensure_tool_table_mapped(pa) {
    if (g_tool_table_va != NULL && g_tool_table_pa == pa)
        return STATUS_SUCCESS;
    if (g_tool_table_va != NULL) {
        io_space_unmap(g_tool_table_va, RDNA_TOOL_TABLE_BYTES);
        g_tool_table_va = NULL;
        g_tool_table_pa = 0;
    }
    new VA:va = io_space_map(pa, RDNA_TOOL_TABLE_BYTES);
    if (va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    g_tool_table_va = va;
    g_tool_table_pa = pa;
    return STATUS_SUCCESS;
}

/* ---------------------------------------------------------------------
 * SMN access
 * ------------------------------------------------------------------- */

/// Perform the SMN index/data sequence for a read using the cached mapping.
NTSTATUS:smn_read(smn_addr, &value) {
    new NTSTATUS:s = ensure_smn_mapped();
    if (s != STATUS_SUCCESS) return s;
    s = virtual_write_dword(g_smn_va, smn_addr);
    if (s == STATUS_SUCCESS) s = virtual_read_dword(g_smn_va + 4, value);
    return s;
}

/// Perform the SMN index/data sequence for a write using the cached mapping.
NTSTATUS:smn_write(smn_addr, value) {
    new NTSTATUS:s = ensure_smn_mapped();
    if (s != STATUS_SUCCESS) return s;
    s = virtual_write_dword(g_smn_va, smn_addr);
    if (s == STATUS_SUCCESS) s = virtual_write_dword(g_smn_va + 4, value);
    return s;
}

/* ---------------------------------------------------------------------
 * Bounded RDNA 3 / RDNA 4 Tool-Table Telemetry
 * ------------------------------------------------------------------- */

/// Map a firmware tool-table version major to its known mailbox layout.
rdna_tool_layout(version) {
    switch ((version >>> 16) & 0xFFFF) {
        case 0x004E: return 7;   /* Navi 31 / Navi 32 (SMU 13.0.0 / 13.0.10) */
        case 0x0055: return 10;  /* Navi 33 (SMU 13.0.7)                     */
        case 0x0056: return 11;  /* Navi 33 alt                              */
        case 0x0066: return 8;   /* Navi 48 / Navi 44 (SMU 14.0)             */
    }
    return 0;
}

NTSTATUS:rdna_tool_response_status(response) {
    response = response & 0xFFFFFFFF;
    if (response == RDNA_TOOL_RESPONSE_OK) return STATUS_SUCCESS;
    if (response == RDNA_TOOL_RESPONSE_BUSY) return STATUS_DEVICE_BUSY;
    if (response == RDNA_TOOL_RSP_PREREQ) return STATUS_INVALID_DEVICE_STATE;
    if (response == RDNA_TOOL_RESPONSE_UNKNOWN) return STATUS_NOT_SUPPORTED;
    return STATUS_UNSUCCESSFUL;
}

/// Spin-wait bounded until the tool mailbox response register becomes nonzero
/// (no `microsleep`, matching RyzenSMU's bounded spin idiom).
NTSTATUS:rdna_tool_wait_response(VA:registers, &response) {
    response = 0;
    for (new i = 0; i < RDNA_TOOL_POLL_RETRIES; i++) {
        new NTSTATUS:s = virtual_read_dword(registers + RDNA_TOOL_RESPONSE_OFFSET, response);
        if (s != STATUS_SUCCESS) return s;
        if ((response & 0xFFFFFFFF) != 0) return STATUS_SUCCESS;
    }
    return STATUS_IO_TIMEOUT;
}

/// Issue one compile-time-fixed tool mailbox command and read back its argument.
NTSTATUS:rdna_tool_send(VA:registers, argument_offset, message, bool:has_argument, &argument) {
    new response = 0;
    new NTSTATUS:s = rdna_tool_wait_response(registers, response);
    if (s != STATUS_SUCCESS) return s;

    s = virtual_write_dword(registers + RDNA_TOOL_RESPONSE_OFFSET, 0);
    if (s != STATUS_SUCCESS) return s;

    if (has_argument) {
        s = virtual_write_dword(registers + argument_offset, argument & 0xFFFFFFFF);
        if (s != STATUS_SUCCESS) return s;
    }

    s = virtual_write_dword(registers + RDNA_TOOL_MESSAGE_OFFSET, message & 0xFFFFFFFF);
    if (s != STATUS_SUCCESS) return s;

    s = rdna_tool_wait_response(registers, response);
    if (s != STATUS_SUCCESS) return s;

    s = rdna_tool_response_status(response);
    if (s != STATUS_SUCCESS) return s;

    s = virtual_read_dword(registers + argument_offset, argument);
    if (s != STATUS_SUCCESS) return s;
    argument = argument & 0xFFFFFFFF;
    return STATUS_SUCCESS;
}

/// Read the GPU hardware framebuffer [base, top) interval from BAR5.
NTSTATUS:rdna_tool_framebuffer_bounds(VA:registers, &fb_base, &fb_top) {
    new base_val = 0, top_val = 0;
    new NTSTATUS:s = virtual_read_dword(registers + RDNA3_FB_BASE_OFFSET, base_val);
    if (s != STATUS_SUCCESS) return s;
    s = virtual_read_dword(registers + RDNA3_FB_TOP_OFFSET, top_val);
    if (s != STATUS_SUCCESS) return s;

    fb_base = (base_val & 0x00FFFFFF) << 24;
    fb_top = (top_val & 0x00FFFFFF) << 24;
    if (fb_top <= fb_base) return STATUS_INVALID_ADDRESS;
    return STATUS_SUCCESS;
}

/// Query, refresh, validate, and copy one bounded 8-KiB RDNA 3/4 telemetry table.
NTSTATUS:read_rdna_tool_table(result[]) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    if (!in_window(0, RDNA_TOOL_MMIO_MAP_SIZE, 0, g_reg_size)) return STATUS_NOT_SUPPORTED;

    new NTSTATUS:s = ensure_tool_mmio_mapped();
    if (s != STATUS_SUCCESS) return s;

    new version = 0;
    s = rdna_tool_send(g_tool_mmio_va, RDNA_TOOL_ARGUMENT_OFFSET, RDNA_TOOL_GET_VERSION, false, version);
    if (s != STATUS_SUCCESS) return s;

    new layout = rdna_tool_layout(version);
    if (layout == 0) return STATUS_NOT_SUPPORTED;
    new arg_off = (layout == 10) ? RDNA_TOOL_ARG_V10_OFFSET : RDNA_TOOL_ARGUMENT_OFFSET;

    new addr_hi = 0, addr_lo = 0;
    s = rdna_tool_send(g_tool_mmio_va, arg_off, RDNA_TOOL_GET_ADDRESS_HIGH, false, addr_hi);
    if (s != STATUS_SUCCESS) return s;
    s = rdna_tool_send(g_tool_mmio_va, arg_off, RDNA_TOOL_GET_ADDRESS_LOW, false, addr_lo);
    if (s != STATUS_SUCCESS) return s;

    new gpu_addr = ((addr_hi & 0xFFFFFFFF) << 32) | (addr_lo & 0xFFFFFFFF);
    if ((gpu_addr & 0x3) != 0) return STATUS_INVALID_ADDRESS;

    new fb_base = 0, fb_top = 0;
    s = rdna_tool_framebuffer_bounds(g_tool_mmio_va, fb_base, fb_top);
    if (s != STATUS_SUCCESS) return s;
    if (!in_window(gpu_addr, RDNA_TOOL_TABLE_BYTES, fb_base, fb_top - fb_base))
        return STATUS_ACCESS_DENIED;

    new vram_off = gpu_addr - fb_base;
    if (!in_window(vram_off, RDNA_TOOL_TABLE_BYTES, 0, g_vram_size))
        return STATUS_ACCESS_DENIED;
    new phys_addr = g_vram_bar + vram_off;
    if (!in_window(phys_addr, RDNA_TOOL_TABLE_BYTES, g_vram_bar, g_vram_size))
        return STATUS_ACCESS_DENIED;

    s = ensure_tool_table_mapped(phys_addr);
    if (s != STATUS_SUCCESS) return s;

    new bool:valid_table = false;
    for (new attempt = 0; attempt < RDNA_TOOL_READ_ATTEMPTS; attempt++) {
        new sel = RDNA_TOOL_REFRESH_SELECTOR;
        s = rdna_tool_send(g_tool_mmio_va, arg_off, RDNA_TOOL_REFRESH_TABLE, true, sel);
        if (s != STATUS_SUCCESS) return s;

        new first_qword = 0;
        new bool:all_same = true;
        for (new i = 0; i < RDNA_TOOL_TABLE_QWORDS; i++) {
            new d_lo = 0, d_hi = 0;
            s = virtual_read_dword(g_tool_table_va + i * 8, d_lo);
            if (s == STATUS_SUCCESS) s = virtual_read_dword(g_tool_table_va + i * 8 + 4, d_hi);
            if (s != STATUS_SUCCESS) return s;

            new q = (d_lo & 0xFFFFFFFF) | ((d_hi & 0xFFFFFFFF) << 32);
            result[RDNA_TOOL_METADATA_QWORDS + i] = q;
            if (i == 0) first_qword = q;
            else if (q != first_qword) all_same = false;
        }
        if (!all_same) {
            valid_table = true;
            break;
        }
    }

    if (!valid_table) return STATUS_DATA_ERROR;

    result[0] = version & 0xFFFFFFFF;
    result[1] = gpu_addr;
    result[2] = fb_base;
    result[3] = fb_top;
    return STATUS_SUCCESS;
}

/* ---------------------------------------------------------------------
 * IOCTLs
 * ------------------------------------------------------------------- */

/// Read one dword from the SMU message mailbox.
///
/// @param in [0] = SMN address. Must be dword-aligned and inside the MP1
///           C2PMSG register file (0x03B10900..0x03B10AFF); any other
///           address returns STATUS_ACCESS_DENIED.
/// @param in_size Must be 1
/// @param out [0] = the register value, zero-extended to 64 bits
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_smn, 1, 1) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    new smn_addr = in[0];
    if (!smn_allowed(smn_addr)) return STATUS_ACCESS_DENIED;

    new value = 0;
    new NTSTATUS:s = smn_read(smn_addr, value);
    out[0] = value & 0xFFFFFFFF;
    return s;
}

/// Write one dword to the SMU message mailbox.
///
/// @param in [0] = SMN address. Must be dword-aligned and inside the MP1
///           C2PMSG register file (0x03B10900..0x03B10AFF).
///           [1] = value to write; only the low 32 bits are used.
/// @param in_size Must be 2
/// @param out [0] = the NTSTATUS of the underlying register write
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_write_smn, 2, 1) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    new smn_addr = in[0];
    if (!smn_allowed(smn_addr)) {
        out[0] = _:STATUS_ACCESS_DENIED & 0xFFFFFFFF;
        return STATUS_ACCESS_DENIED;
    }

    new NTSTATUS:s = smn_write(smn_addr, in[1] & 0xFFFFFFFF);
    out[0] = _:s & 0xFFFFFFFF;
    return s;
}

/// Read the SmuMetrics_t DMA buffer from the GPU's VRAM aperture.
///
/// @param in [0] = host physical address of the buffer. Must be dword-aligned
///           and lie entirely within the GPU's VRAM aperture.
/// @param in_size Must be 1
/// @param out 65 raw DWORDs - `SmuMetrics_t` (260 bytes) on Navi 4x
/// @param out_size Must be 65
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_metrics, 1, SMU_METRICS_DWORDS) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    new pa = in[0];
    new len = SMU_METRICS_DWORDS * 4;
    if ((pa & 0x3) != 0) return STATUS_INVALID_PARAMETER;
    if (!in_window(pa, len, g_vram_bar, g_vram_size)) return STATUS_ACCESS_DENIED;

    new NTSTATUS:s = ensure_metrics_mapped(pa, len);
    if (s != STATUS_SUCCESS) return s;
    for (new i = 0; i < SMU_METRICS_DWORDS; i++) {
        new v = 0;
        s = virtual_read_dword(g_metrics_va + i * 4, v);
        if (s != STATUS_SUCCESS) return s;
        out[i] = v & 0xFFFFFFFF;
    }
    return STATUS_SUCCESS;
}

/// Report which device apertures the module selected and enforces (legacy ABI 1).
///
/// @param in Ignored
/// @param in_size Must be 1
/// @param out [0] = ready; [1]/[2] = register BAR base/size;
///            [3]/[4] = VRAM BAR base/size
/// @param out_size Must be 5
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_bounds, 1, 5) {
    out[0] = g_ready;
    out[1] = g_reg_bar;
    out[2] = g_reg_size;
    out[3] = g_vram_bar;
    out[4] = g_vram_size;
    return STATUS_SUCCESS;
}

/// Report the selected GPU's PCI identity and apertures (ABI 2).
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = ABI version; [1..3] = PCI bus/dev/fn; [4..7] = device ID,
///            revision ID, subsystem vendor ID, subsystem device ID;
///            [8] = BAR5 register base; [9..10] = BAR0 VRAM base and size
/// @param out_size Must be 11
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_device_info, 0, 11) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    out[0] = MODULE_ABI_VERSION;
    out[1] = g_pci_bus;
    out[2] = g_pci_dev;
    out[3] = 0;
    out[4] = g_device_id;
    out[5] = g_revision_id;
    out[6] = g_subsys_vid;
    out[7] = g_subsys_did;
    out[8] = g_reg_bar;
    out[9] = g_vram_bar;
    out[10] = g_vram_size;
    return STATUS_SUCCESS;
}

/// Refresh and read the bounded 8-KiB RDNA 3 / RDNA 4 monitoring tool table.
///
/// All register offsets, mailbox message IDs, selector, and table size are
/// compile-time fixed. The firmware-reported GPU/MC address is validated
/// against both the hardware framebuffer interval and the BAR0 ReBAR aperture.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = table version; [1] = GPU MC address; [2]/[3] = FB base/top;
///            [4..1027] = 8192 raw table bytes packed as 1024 QWORDs
/// @param out_size Must be 1028
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_rdna_tool_table, 0, RDNA_TOOL_OUTPUT_QWORDS) {
    return read_rdna_tool_table(out);
}

/* --- Lifecycle --- */

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    g_ready = 0;
    g_pci_bus = 0;
    g_pci_dev = 0;
    g_device_id = 0;
    g_revision_id = 0;
    g_subsys_vid = 0;
    g_subsys_did = 0;
    g_reg_bar = 0;
    g_reg_size = 0;
    g_vram_bar = 0;
    g_vram_size = 0;
    g_smn_va = NULL;
    g_tool_mmio_va = NULL;
    g_metrics_va = NULL;
    g_metrics_pa = 0;
    g_metrics_len = 0;
    g_tool_table_va = NULL;
    g_tool_table_pa = 0;
    find_gpu_and_probe();

    if (!g_ready)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    unmap_all_cached();
    return STATUS_SUCCESS;
}
