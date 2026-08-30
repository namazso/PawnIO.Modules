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
 * RadeonSMU - SMU mailbox access for AMD Radeon (RDNA) GPUs.
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
 * Exposed surface
 * ---------------
 *   - SMN reads/writes restricted to the MP1 C2PMSG register file
 *     (0x03B10900..0x03B10AFF, 128 registers). That window is the SMU
 *     message mailbox: MSG (C2PMSG_66), PARAM (C2PMSG_82), RESP
 *     (C2PMSG_90), and the addr_hi/lo pair (C2PMSG_80/81) that PMFW
 *     populates for TransferTableSmu2DramWithAddr.
 *   - One fixed-size read of the SmuMetrics_t DMA buffer, bounded to the
 *     GPU's own VRAM aperture.
 *   - Nothing else. No system RAM, no other devices, no writes to the
 *     framebuffer, and no SMN address outside the mailbox window.
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
 */

#include <pawnio.inc>

/// PCI vendor id for AMD/ATI.
const AMD_VENDOR_ID = 0x1002;

/// BAR5 offset of PCIE_INDEX2 - the SMN address (index) register.
const SMN_INDEX_OFFSET = 0x38;
/// BAR5 offset of PCIE_DATA2 - the SMN data register.
const SMN_DATA_OFFSET = 0x3C;

/// First SMN address of the MP1 C2PMSG register file (C2PMSG_0).
/// C2PMSG_66 is 0x03B10A08, i.e. base + 66*4.
const MP1_C2PMSG_BASE = 0x03B10900;
/// Byte span of the C2PMSG register file: 128 registers of 4 bytes.
const MP1_C2PMSG_SPAN = 0x200;

/// Dword count of `SmuMetrics_t` on Navi 4x (260 bytes). Fixed rather
/// than caller-supplied so the output array is statically sized.
const SMU_METRICS_DWORDS = 65;

/* Fallback register-BAR window, used only if the BAR5 size probe returns
 * something implausible. The MMIO register BAR is ~512 KB on Navi 44;
 * 1 MB is a safe fallback that still stays inside adjacent MMIO. */
const REG_SPAN = 0x100000;
/// Sanity bound on the probed BAR5 size; larger is treated as a bad read.
const REG_SIZE_MAX = 0x1000000;      /* 16 MB */
/// Sanity bound on the probed BAR0 size; larger is treated as a bad read.
const VRAM_SIZE_MAX = 0x1000000000;  /* 64 GB */

/* Discovered at load (main()). */
new g_ready = 0;
new g_reg_bar = 0;    /* BAR5 base - MMIO register aperture */
new g_reg_size = 0;   /* BAR5 size (probed)                 */
new g_vram_bar = 0;   /* BAR0 base - VRAM aperture          */
new g_vram_size = 0;  /* BAR0 size (probed)                 */

/* ---------------------------------------------------------------------
 * Discovery
 * ------------------------------------------------------------------- */

/// Find the AMD VGA controller with the largest VRAM aperture, read its
/// register and VRAM BAR bases, and probe both sizes. Sets g_ready on
/// success. If it fails, `main()` refuses the load with STATUS_NOT_SUPPORTED.
find_gpu_and_probe() {
    new best_bus = -1, best_dev = -1;
    new best_vram_bar = 0, best_vram_size = 0;

    for (new bus = 0; bus <= 255; bus++) {
        for (new dev = 0; dev < 32; dev++) {
            new vd = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x00, vd) != STATUS_SUCCESS) continue;
            if ((vd & 0xFFFF) != AMD_VENDOR_ID) continue;

            new cls = 0;
            if (pci_config_read_dword(bus, dev, 0, 0x08, cls) != STATUS_SUCCESS) continue;
            if (((cls >> 24) & 0xFF) != 0x03) continue;   /* base class: display */
            if (((cls >> 16) & 0xFF) != 0x00) continue;   /* sub class:  VGA     */

            /* VRAM BAR = BAR0. Validate the type bits before treating
             * 0x14 as the high half: on a 32-bit-BAR device that offset
             * is BAR1, and composing the two yields a garbage base. */
            new b0lo = 0, b0hi = 0;
            pci_config_read_dword(bus, dev, 0, 0x10, b0lo);
            if ((b0lo & 0x1) != 0) continue;            /* I/O space  */
            if (((b0lo >> 1) & 0x3) != 0x2) continue;   /* not 64-bit */
            pci_config_read_dword(bus, dev, 0, 0x14, b0hi);
            new vram_bar = (b0hi << 32) | (b0lo & 0xFFFFFFF0);

            /* Standard write-all-ones / read-mask / restore size probe.
             * Memory decode is not disabled (PCI config access is
             * PASSIVE_LEVEL and cannot be IRQL-protected); each BAR is
             * restored within a few microseconds. */
            new mlo = 0, mhi = 0;
            pci_config_write_dword(bus, dev, 0, 0x10, 0xFFFFFFFF);
            pci_config_write_dword(bus, dev, 0, 0x14, 0xFFFFFFFF);
            pci_config_read_dword(bus, dev, 0, 0x10, mlo);
            pci_config_read_dword(bus, dev, 0, 0x14, mhi);
            pci_config_write_dword(bus, dev, 0, 0x10, b0lo);
            pci_config_write_dword(bus, dev, 0, 0x14, b0hi);

            new mask = (mhi << 32) | (mlo & 0xFFFFFFF0);
            new vram_size = (~mask) + 1;

            /* Fail closed on an implausible read-back: skip the device
             * rather than let a bogus size win the ranking. */
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
    pci_config_read_dword(best_bus, best_dev, 0, 0x24, b5);
    if ((b5 & 0x1) != 0) return;                /* must be memory space */
    new b5type = (b5 >> 1) & 0x3;
    if (b5type != 0x0 && b5type != 0x2) return; /* reserved encoding    */
    new b5hi = 0;
    if (b5type == 0x2) pci_config_read_dword(best_bus, best_dev, 0, 0x28, b5hi);
    new reg_bar = (b5hi << 32) | (b5 & 0xFFFFFFF0);

    new m5 = 0;
    pci_config_write_dword(best_bus, best_dev, 0, 0x24, 0xFFFFFFFF);
    pci_config_read_dword(best_bus, best_dev, 0, 0x24, m5);
    pci_config_write_dword(best_bus, best_dev, 0, 0x24, b5);
    m5 = m5 & 0xFFFFFFF0;
    new reg_size = 0;
    if (m5 != 0) reg_size = ((~m5) & 0xFFFFFFFF) + 1;
    if (reg_size <= 0 || reg_size > REG_SIZE_MAX) reg_size = REG_SPAN;

    /* The SMN index/data pair must fit inside the register aperture. */
    if (reg_size < SMN_DATA_OFFSET + 4) return;

    g_reg_bar = reg_bar;
    g_reg_size = reg_size;
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
        virtual_read_dword(va + i * 4, v);
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
