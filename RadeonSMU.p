//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Adrenalift contributors
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
 * RadeonSMU - SMU mailbox access for AMD Radeon RDNA4 (Navi 4x) GPUs.
 *
 * Replaces WinRing0/InpOut32-style raw physical MMIO for AMD GPU tuning
 * software. Rather than handing out a physical read/write primitive, this
 * module owns the SMN index/data sequence itself and allowlists the SMN
 * addresses a caller may reach, so the exposed surface is the MP1 message
 * mailbox and nothing else.
 *
 * Discovery
 * ---------
 * The module finds the AMD display device itself by PCI scan and reads its
 * own BAR bases and sizes; the caller never supplies a bus/device or a
 * physical address for register access. Selection is by LARGEST VRAM
 * aperture rather than enumeration order, so an AMD iGPU that enumerates
 * first (e.g. Raphael, 256 MB carved from system RAM) cannot shadow a
 * discrete card. `ioctl_get_bounds` lets the caller confirm which device
 * was chosen.
 *
 * Discovery is read-only: the VRAM aperture size comes from the Resizable
 * BAR capability (`rebar_current_size`), not a write-all-ones probe. Only
 * Navi 4x is bound (`is_supported_gpu`), the one family this has run on.
 *
 * Exposed surface
 * ---------------
 *   - SMN reads/writes restricted to the MP1 C2PMSG register file
 *     (0x03B10900..0x03B10AFF, 128 registers). That window is the SMU
 *     message mailbox: MSG (C2PMSG_66), PARAM (C2PMSG_82), RESP
 *     (C2PMSG_90), and the addr_hi/lo pair (C2PMSG_80/81) that PMFW
 *     populates for TransferTableSmu2DramWithAddr.
 *   - One fixed-size read of the SmuMetrics_t DMA buffer, bounded to the
 *     GPU's own VRAM aperture.
 *   - Nothing else. The module itself touches no system RAM, no other
 *     device, and no SMN address outside the mailbox window, and it never
 *     writes to the framebuffer.
 *
 * Two caveats, both shared with RyzenSMU. C2PMSG_80/81 are writable, so a
 * caller can aim the firmware's own DMA anywhere via
 * TransferTableSmu2DramWithAddr; that transfer is the SMU's, not ours, and
 * no bound here applies to it. And PCIE_INDEX2/DATA2 is the same pair the
 * AMD display driver uses (Linux: `adev->pcie_idx_lock`); we cannot take
 * that lock, so callers should serialise via the
 * "\BaseNamedObjects\Access_PCI" mutant.
 *
 * The mailbox window is deliberately not gated on message id: every
 * PPSMC message is a write of an id to the same MSG register, so
 * allowlisting the registers rather than the messages keeps present and
 * future firmware messages working without widening the surface. This
 * mirrors RyzenSMU, which likewise bounds SMN ranges and accepts any
 * message id.
 *
 * Reference
 * ---------
 *   - Linux amdgpu: `drivers/gpu/drm/amd/pm/swsmu/smu_cmn.c` (mailbox
 *     protocol: write RESP=0, write PARAM, write MSG, poll RESP) and
 *     `amdgpu_device.c` (PCIE_INDEX2/PCIE_DATA2 indirect SMN access).
 *   - MP1 C2PMSG register numbering: `smu_v14_0.c` / `smu_v13_0.c`,
 *     `mmMP1_SMN_C2PMSG_66/82/90`.
 *   - Navi 4x SmuMetrics_t layout: `smu14_driver_if_v14_0.h`.
 *   - Resizable BAR capability: PCI Express Base Specification, "Resizable
 *     BAR Extended Capability"; Linux `drivers/pci/pci.c`
 *     `pci_rebar_find_pos` / `pci_rebar_get_current_size`.
 */

#include <pawnio.inc>

/// PCI vendor id for AMD/ATI.
const AMD_VENDOR_ID = 0x1002;

/// PCI COMMAND register offset, and its memory-space decode bit.
/// Mapping a BAR does not enable the endpoint's decoder, so a function
/// with decode disabled would map "successfully" onto nothing.
const PCI_CFG_COMMAND = 0x04;
const PCI_COMMAND_MEMORY = 0x02;

/* PCIe extended config space: 4 KB, capability chain starts at 0x100. A
 * platform that cannot reach it fails the read rather than returning
 * garbage (short HalGetBusDataByOffset -> STATUS_UNSUCCESSFUL). */
const PCI_EXT_CFG_BASE = 0x100;
const PCI_EXT_CFG_SIZE = 0x1000;
/// Extended capability id of the Resizable BAR capability.
const PCI_EXT_CAP_ID_REBAR = 0x0015;
/// Bound on the capability chain walk; only has to terminate a bad chain.
const PCI_EXT_CAP_MAX_HOPS = 64;

/* Resizable BAR capability layout, relative to its header: header at 0,
 * then one { capability, control } dword pair per resizable BAR. */
const PCI_REBAR_CTRL = 0x08;          /* first control register       */
const PCI_REBAR_ENTRY_STRIDE = 0x08;  /* bytes per { cap, ctrl } pair */
const PCI_REBAR_CTRL_BAR_IDX = 0x07;  /* ctrl[2:0]  - BAR this describes  */
const PCI_REBAR_CTRL_NBAR_SHIFT = 5;  /* ctrl[7:5]  - entry count (first) */
const PCI_REBAR_CTRL_NBAR_MASK = 0x07;
const PCI_REBAR_CTRL_SIZE_SHIFT = 8;  /* ctrl[13:8] - size, 2^(n+20) bytes */
const PCI_REBAR_CTRL_SIZE_MASK = 0x3F;
/// Largest size encoding acted on; 2^(42+20) stays clear of the sign bit.
const PCI_REBAR_SIZE_ENCODING_MAX = 42;

/* Navi 4x device id ranges, the only family this module has been run
 * on. Navi 4x loads via CHIP_IP_DISCOVERY and has no explicit Linux
 * entries; ids are from the Adrenalin INF (32.0.31041): Navi 48
 * 0x7550/0x7551, Navi 44 0x7590. Family ranges rather than a per-SKU
 * list, because the mailbox surface is identical within a family.
 *
 * Not admitted, untested here, for whoever widens this: Navi 2x from
 * Linux amdgpu_drv.c is 0x73A0-0x73FF (Navi 21/22/23) and 0x7420-0x743F
 * (Navi 24); 0x7408-0x7410 between them is Aldebaran (CDNA) and must
 * stay out. Navi 3x from the same INF is 0x7448-0x745E, 0x7470/0x747E
 * and 0x7480-0x7499. SMU_METRICS_DWORDS would also need to become
 * per-family (41 on SMU11, 60/61 on SMU13). */
const NAVI4X_DEVICE_ID_MIN_0 = 0x7550;
const NAVI4X_DEVICE_ID_MAX_0 = 0x756F;
const NAVI4X_DEVICE_ID_MIN_1 = 0x7590;
const NAVI4X_DEVICE_ID_MAX_1 = 0x75AF;

/// BAR5 offset of PCIE_INDEX2 - the SMN address (index) register.
const SMN_INDEX_OFFSET = 0x38;
/// BAR5 offset of PCIE_DATA2 - the SMN data register (the `va + 4` in
/// smn_read/smn_write, which map the pair as one 8-byte window).
const SMN_DATA_OFFSET = 0x3C;

/// First SMN address of the MP1 C2PMSG register file (C2PMSG_0).
/// C2PMSG_66 is 0x03B10A08, i.e. base + 66*4.
const MP1_C2PMSG_BASE = 0x03B10900;
/// Byte span of the C2PMSG register file: 128 registers of 4 bytes.
const MP1_C2PMSG_SPAN = 0x200;

/// Dword count of `SmuMetrics_t` on Navi 4x (260 bytes). Fixed rather
/// than caller-supplied so the output array is statically sized.
const SMU_METRICS_DWORDS = 65;

/* Declared register-BAR map bound, not a measured size: the only register
 * access is the 8-byte SMN pair at +0x38, so there is nothing to measure.
 * Nvidia.p and IntelOOBMSM.p bound their mappings the same way. */
const REG_SPAN = 0x100000;
/// Sanity bound on the ReBAR-reported BAR0 size; larger is a bad read.
const VRAM_SIZE_MAX = 0x1000000000;  /* 64 GB */

/* Discovered at load (main()). */
new g_ready = 0;
new g_reg_bar = 0;    /* BAR5 base - MMIO register aperture   */
new g_reg_size = 0;   /* declared register map bound          */
new g_vram_bar = 0;   /* BAR0 base - VRAM aperture            */
new g_vram_size = 0;  /* BAR0 size, from the ReBAR capability */

/* ---------------------------------------------------------------------
 * Discovery
 * ------------------------------------------------------------------- */

/// True iff `device_id` is a Navi 4x part. A security gate: on an AMD
/// APU the allowlisted C2PMSG window is the *CPU's* SMU mailbox
/// (RyzenSMU.p reaches 0x3B10A20/0x3B10A80/0x3B10A88 there, inside our
/// window), so binding an iGPU would hand a caller the processor's SMU.
bool: is_supported_gpu(device_id) {
    if (device_id >= NAVI4X_DEVICE_ID_MIN_0 && device_id <= NAVI4X_DEVICE_ID_MAX_0) return true;
    if (device_id >= NAVI4X_DEVICE_ID_MIN_1 && device_id <= NAVI4X_DEVICE_ID_MAX_1) return true;
    return false;
}

/// Current size in bytes of `bar_index`, from the Resizable BAR capability.
/// Replaces the write-all-ones probe, which relocates a live endpoint's
/// decoder mid-scanout; Linux only does that with decode disabled and
/// before a driver attaches (`__pci_read_base`). Fails with
/// STATUS_NOT_FOUND when absent - the caller must skip, not guess, since
/// this is the only bound on `ioctl_read_metrics`.
NTSTATUS:rebar_current_size(bus, dev, bar_index, &size) {
    size = 0;

    new off = PCI_EXT_CFG_BASE;
    for (new hop = 0; hop < PCI_EXT_CAP_MAX_HOPS; hop++) {
        if (off < PCI_EXT_CFG_BASE || off > PCI_EXT_CFG_SIZE - 4) return STATUS_NOT_FOUND;

        new hdr = 0;
        new NTSTATUS:s = pci_config_read_dword(bus, dev, 0, off, hdr);
        if (s != STATUS_SUCCESS) return s;
        /* No extended config space, or a hole in the chain. */
        if (hdr == 0 || (hdr & 0xFFFFFFFF) == 0xFFFFFFFF) return STATUS_NOT_FOUND;

        if ((hdr & 0xFFFF) == PCI_EXT_CAP_ID_REBAR) {
            /* The entry count lives in the first control register only. */
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
/// and read its register and VRAM BAR bases. Sets g_ready on success. If
/// it fails, `main()` refuses the load with STATUS_NOT_SUPPORTED.
///
/// Every config access here is a READ; nothing modifies the device.
find_gpu_and_probe() {
    new best_bus = -1, best_dev = -1;
    new best_vram_bar = 0, best_vram_size = 0;

    for (new bus = 0; bus <= 255; bus++) {
        for (new dev = 0; dev < 32; dev++) {
            new vd = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x00, vd) != STATUS_SUCCESS) continue;
            if ((vd & 0xFFFF) != AMD_VENDOR_ID) continue;
            if (!is_supported_gpu((vd >>> 16) & 0xFFFF)) continue;

            new cls = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x08, cls) != STATUS_SUCCESS) continue;
            if (((cls >> 24) & 0xFF) != 0x03) continue;   /* base class: display */
            if (((cls >> 16) & 0xFF) != 0x00) continue;   /* sub class:  VGA     */

            /* The function must already be decoding memory. Mapping a BAR
             * does not enable the endpoint decoder, and a function with
             * decode off has no assigned base worth reading. */
            new cmd = 0;
            if (pci_config_read_word(bus, dev, 0, PCI_CFG_COMMAND, cmd) != STATUS_SUCCESS) continue;
            if ((cmd & PCI_COMMAND_MEMORY) == 0) continue;

            /* VRAM BAR = BAR0. Validate the type bits before treating
             * 0x14 as the high half: on a 32-bit-BAR device that offset
             * is BAR1, and composing the two yields a garbage base. */
            new b0lo = 0, b0hi = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x10, b0lo) != STATUS_SUCCESS) continue;
            if ((b0lo & 0x1) != 0) continue;            /* I/O space  */
            if (((b0lo >> 1) & 0x3) != 0x2) continue;   /* not 64-bit */
            /* An unchecked high read would leave b0hi = 0 and truncate an
             * above-4G base to its low half. */
            if (pci_config_read_dword(bus, dev, 0, 0x14, b0hi) != STATUS_SUCCESS) continue;
            new vram_bar = ((b0hi & 0xFFFFFFFF) << 32) | (b0lo & 0xFFFFFFF0);

            /* An UNASSIGNED BAR still reads its hardwired type bits (raw
             * 0x0C) while the base masks to zero, which would aim every
             * bounds check at low RAM. `<= 0`: in_window() needs base >= 0. */
            if (vram_bar <= 0) continue;

            /* No ReBAR capability -> skip, never guess (sole metrics bound). */
            new vram_size = 0;
            if (rebar_current_size(bus, dev, 0, vram_size) != STATUS_SUCCESS) continue;
            if (vram_size <= 0 || vram_size > VRAM_SIZE_MAX) continue;

            /* Tie-break toward the higher PCI bus, where a discrete card
             * normally sits (an iGPU is typically at bus 00). */
            if (vram_size > best_vram_size ||
                (vram_size == best_vram_size && bus > best_bus)) {
                best_vram_size = vram_size;
                best_vram_bar = vram_bar;
                best_bus = bus;
                best_dev = dev;
            }
        }
    }

    if (best_bus < 0) return;   /* no AMD VGA device found */

    new b5 = 0;
    /* Check the status: a failed read leaves b5 = 0, which would otherwise
     * fall straight through the type checks below into a zero base. */
    if (pci_config_read_dword(best_bus, best_dev, 0, 0x24, b5) != STATUS_SUCCESS)
        return;
    if ((b5 & 0x1) != 0) return;                /* must be memory space */
    /* BAR5 must be 32-bit: 0x28 is the Cardbus CIS Pointer, not a BAR, so
     * a 64-bit encoding at 0x24 means it is the high half of a 64-bit BAR4
     * (whose address bits can read as 0b10), not BAR5. Refuse. */
    if (((b5 >> 1) & 0x3) != 0x0) return;
    new reg_bar = b5 & 0xFFFFFFF0;

    /* Same unassigned-BAR hazard as BAR0. A zero base would make
     * g_reg_bar + SMN_INDEX_OFFSET resolve to physical 0x38 — a WRITE
     * into low system RAM instead of the SMN index register. */
    if (reg_bar <= 0)
        return;

    g_reg_bar = reg_bar;
    g_reg_size = REG_SPAN;
    g_vram_bar = best_vram_bar;
    g_vram_size = best_vram_size;
    g_ready = 1;
}

/* ---------------------------------------------------------------------
 * Bounds
 * ------------------------------------------------------------------- */

/// True iff [pa, pa+len) lies entirely within [base, base+size).
///
/// Overflow-safe by construction. Cells are signed 64-bit (-C64), so the
/// naive `pa + len <= base + size` can wrap for a pa near 2^63 and wrongly
/// return true. This never adds to pa: it checks `pa >= base` (which also
/// rejects a negative pa), derives the offset by subtraction, and compares
/// len against the remaining window, which cannot overflow.
/// Requires base >= 0 (a negative base makes `pa - base` wrap), which is
/// why discovery rejects non-positive apertures.
bool: in_window(pa, len, base, size) {
    if (size <= 0) return false;
    if (len <= 0) return false;
    if (pa < base) return false;
    new off = pa - base;
    if (off > size) return false;
    if (len > size - off) return false;
    return true;
}

/// True iff `smn_addr` is a dword-aligned address inside the MP1 C2PMSG
/// register file. This is the entire SMN surface the module exposes.
bool: smn_allowed(smn_addr) {
    if ((smn_addr & 0x3) != 0) return false;
    return in_window(smn_addr, 4, MP1_C2PMSG_BASE, MP1_C2PMSG_SPAN);
}

/* ---------------------------------------------------------------------
 * SMN access
 * ------------------------------------------------------------------- */

/// Perform the SMN index/data sequence for a read. Both accesses happen
/// under a single mapping so the pair is not split across ioctl calls.
NTSTATUS:smn_read(smn_addr, &value) {
    new VA:va = io_space_map(g_reg_bar + SMN_INDEX_OFFSET, 8);
    if (va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    new NTSTATUS:s = virtual_write_dword(va, smn_addr);
    if (s == STATUS_SUCCESS) s = virtual_read_dword(va + 4, value);
    io_space_unmap(va, 8);
    return s;
}

/// Perform the SMN index/data sequence for a write.
NTSTATUS:smn_write(smn_addr, value) {
    new VA:va = io_space_map(g_reg_bar + SMN_INDEX_OFFSET, 8);
    if (va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    new NTSTATUS:s = virtual_write_dword(va, smn_addr);
    if (s == STATUS_SUCCESS) s = virtual_write_dword(va + 4, value);
    io_space_unmap(va, 8);
    return s;
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
/// Writing an id to the MSG register (C2PMSG_66) is how every PPSMC
/// message is issued; the module deliberately does not gate on message
/// id, because the register allowlist already confines the effect to this
/// GPU's mailbox.
///
/// @param in [0] = SMN address. Must be dword-aligned and inside the MP1
///           C2PMSG register file (0x03B10900..0x03B10AFF); any other
///           address returns STATUS_ACCESS_DENIED.
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
/// The buffer is allocated and programmed by the AMD display driver, not
/// by this module; its address is recovered by the caller from the
/// addr_hi/lo pair the firmware writes to C2PMSG_80/81. The address is
/// still bounds-checked here rather than trusted.
///
/// @param in [0] = host physical address of the buffer. Must be
///           dword-aligned and lie entirely within the GPU's VRAM
///           aperture; any other address returns STATUS_ACCESS_DENIED.
/// @param in_size Must be 1
/// @param out 65 raw DWORDs - `SmuMetrics_t` (260 bytes) on Navi 4x, each
///            zero-extended to 64 bits. Layout is firmware-defined; see
///            `smu14_driver_if_v14_0.h`.
/// @param out_size Must be 65
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_metrics, 1, SMU_METRICS_DWORDS) {
    if (!g_ready) return STATUS_DEVICE_NOT_READY;
    new pa = in[0];
    new len = SMU_METRICS_DWORDS * 4;
    if ((pa & 0x3) != 0) return STATUS_INVALID_PARAMETER;
    if (!in_window(pa, len, g_vram_bar, g_vram_size)) return STATUS_ACCESS_DENIED;

    new VA:va = io_space_map(pa, len);
    if (va == NULL) return STATUS_INSUFFICIENT_RESOURCES;
    for (new i = 0; i < SMU_METRICS_DWORDS; i++) {
        new v = 0;
        new NTSTATUS:s = virtual_read_dword(va + i * 4, v);
        if (s != STATUS_SUCCESS) {
            io_space_unmap(va, len);
            return s;
        }
        out[i] = v & 0xFFFFFFFF;
    }
    io_space_unmap(va, len);
    return STATUS_SUCCESS;
}

/// Report which device the module selected and the apertures it enforces.
///
/// Lets the caller confirm the module bound to the same GPU it intends to
/// drive, and observe the VRAM aperture size across ReBAR states.
///
/// @param in Ignored
/// @param in_size Must be 1
/// @param out [0] = always 1; the module refuses to load at all when no
///            supported GPU is found, so a loaded module is a bound one
///            [1] = register BAR base, [2] = register BAR size
///            [3] = VRAM aperture base, [4] = VRAM aperture size
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

/* --- Lifecycle --- */

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    g_ready = 0;
    g_reg_bar = 0;
    g_reg_size = 0;
    g_vram_bar = 0;
    g_vram_size = 0;
    find_gpu_and_probe();

    /* Refuse to load rather than loading and denying every ioctl: a
     * caller on a machine with no supported AMD GPU learns at load time
     * instead of from a string of STATUS_ACCESS_DENIED results. */
    if (!g_ready)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    return STATUS_SUCCESS;
}
