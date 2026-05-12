//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  CapFrameX Contributors
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

// PawnIO Intel OOBMSM Driver - minimal MCHBAR-style.
//
// OOBMSM (Out-Of-Band Management Services Module) on Core Ultra
// (Meteor Lake and later) sits at PCI 00:0A.0 and hosts Intel
// Platform Monitoring Technology (PMT) telemetry behind PCIe
// extended capabilities (DVSEC 0x000B and TPMI 0x0023). All
// telemetry-bearing apertures live inside BAR0 on shipping silicon.
//
// This module mirrors IntelMCHBAR.p's pattern: probe the canonical
// PCI slot at init, map the relevant BAR with io_space_map, and
// expose a minimal IOCTL surface (PCI-config DWORD read,
// BAR-relative DWORD read). All capability discovery, BAR-base
// resolution and per-platform telemetry-layout decoding lives on
// the C# consumer side (LibreHardwareMonitorLib.PawnIo.IntelOobmsm
// / IntelOobmsmClocks).

#define PCI_VENDOR_ID_INTEL     0x8086

// PCI 00:0A.0 - canonical OOBMSM slot on Core Ultra client parts.
#define OOBMSM_BUS              0x00
#define OOBMSM_DEV              0x0A
#define OOBMSM_FN               0x00

// PCI config-space size we expose. PCIe extends the classic 256-byte
// block to 4 KiB and exposes the ext-cap chain at offset 0x100;
// the C# side walks it via the config-read primitive.
#define PCI_CFG_SPACE_EXP_SIZE  0x1000

// PCI config-space offsets used here.
#define PCI_CFG_BAR0_LOW        0x10
#define PCI_CFG_BAR0_HIGH       0x14
#define PCI_BAR_MEM_TYPE_64BIT  0x4
#define PCI_BAR_MEM_ADDR_MASK   0xFFFFFFF0

// BAR0 mapping size. OOBMSM's BAR0 is 64 KiB on every Core Ultra
// stepping observed; the TPMI capability discovery offsets seen so
// far all sit in the upper 2 KiB of that window (~0xF800+). Mapping
// the full 64 KiB covers any future cap layout without having to
// re-map at runtime.
#define OOBMSM_BAR_MAP_SIZE     0x10000

const CodeName: {
    CPU_UNKNOWN = -1,
    CPU_METEORLAKE,
    CPU_METEORLAKE_L,
    CPU_ARROWLAKE_H,
    CPU_ARROWLAKE,
    CPU_ARROWLAKE_U,
    CPU_LUNARLAKE_M,
    CPU_PANTHERLAKE_L,
    CPU_WILDCATLAKE_L,
    CPU_NOVALAKE,
    CPU_NOVALAKE_L,
};

CodeName:get_code_name(family, model) {
    switch ((family << 8) | model) {
        case 0x06AC:
            return CPU_METEORLAKE;
        case 0x06AA:
            return CPU_METEORLAKE_L;
        case 0x06C5:
            return CPU_ARROWLAKE_H;
        case 0x06C6:
            return CPU_ARROWLAKE;
        case 0x06B5:
            return CPU_ARROWLAKE_U;
        case 0x06BD:
            return CPU_LUNARLAKE_M;
        case 0x06CC:
            return CPU_PANTHERLAKE_L;
        case 0x06D5:
            return CPU_WILDCATLAKE_L;
        case 0x1801:
            return CPU_NOVALAKE;
        case 0x1803:
            return CPU_NOVALAKE_L;

        default:
            return CPU_UNKNOWN;
    }
    return CPU_UNKNOWN;
}

new g_bar_addr = 0;
new g_bar_size = 0;
new VA:g_bar_va = NULL;

NTSTATUS:oobmsm_init() {
    // Confirm an Intel device decodes at the canonical OOBMSM slot.
    new vid_did = 0;
    new NTSTATUS:status = pci_config_read_dword(OOBMSM_BUS, OOBMSM_DEV, OOBMSM_FN, 0, vid_did);
    if (!NT_SUCCESS(status))
        return status;
    if ((vid_did & 0xFFFF) != PCI_VENDOR_ID_INTEL)
        return STATUS_NOT_SUPPORTED;
    if ((vid_did & 0xFFFF) == 0xFFFF)
        return STATUS_NOT_SUPPORTED;

    // Read BAR0 (64-bit memory BAR on every observed stepping).
    new base_lo = 0;
    new base_hi = 0;
    pci_config_read_dword(OOBMSM_BUS, OOBMSM_DEV, OOBMSM_FN, PCI_CFG_BAR0_LOW, base_lo);
    if (base_lo == 0 || base_lo == 0xFFFFFFFF)
        return STATUS_NOT_SUPPORTED;
    if ((base_lo & PCI_BAR_MEM_TYPE_64BIT) != 0)
        pci_config_read_dword(OOBMSM_BUS, OOBMSM_DEV, OOBMSM_FN, PCI_CFG_BAR0_HIGH, base_hi);

    g_bar_addr = ((base_hi & 0xFFFFFFFF) << 32) | (base_lo & PCI_BAR_MEM_ADDR_MASK);
    if (g_bar_addr == 0)
        return STATUS_NOT_SUPPORTED;

    g_bar_size = OOBMSM_BAR_MAP_SIZE;
    g_bar_va = io_space_map(g_bar_addr, g_bar_size);
    if (g_bar_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    debug_print(''IntelOOBMSM: BAR0 mapped at phys 0x%x size 0x%x\n'', g_bar_addr, g_bar_size);
    return STATUS_SUCCESS;
}

/// Read a DWORD from PCI configuration space of OOBMSM (00:0A.0).
///
/// @param in [0] = byte offset (DW-aligned, 0..0xFFC)
/// @param in_size Must be 1
/// @param out [0] = value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_pci_config_read_dword, 1, 1) {
    new offset = in[0];
    if (offset < 0 || offset > (PCI_CFG_SPACE_EXP_SIZE - 4))
        return STATUS_INVALID_PARAMETER;
    if (offset & 0x3)
        return STATUS_INVALID_PARAMETER;
    new value = 0;
    new NTSTATUS:status = pci_config_read_dword(OOBMSM_BUS, OOBMSM_DEV, OOBMSM_FN, offset, value);
    out[0] = value;
    return status;
}

/// Read a DWORD from the mapped BAR0.
///
/// @param in [0] = byte offset within BAR0 (DW-aligned, < mapped size)
/// @param in_size Must be 1
/// @param out [0] = value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_dword, 1, 1) {
    new offset = in[0];
    if (offset < 0 || offset >= g_bar_size)
        return STATUS_ACCESS_DENIED;
    if (offset & 0x3)
        return STATUS_ACCESS_DENIED;
    new value = 0;
    new NTSTATUS:status = virtual_read_dword(g_bar_va + offset, value);
    out[0] = value;
    return status;
}

/// Identify the device + return BAR0 mapping info.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = (DID << 16) | VID
///            [1] = (bus << 16) | (device << 8) | function
///            [2] = BAR0 physical address
///            [3] = BAR0 mapped size (bytes)
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 4) {
    new vid_did = 0;
    new NTSTATUS:status = pci_config_read_dword(OOBMSM_BUS, OOBMSM_DEV, OOBMSM_FN, 0, vid_did);
    if (!NT_SUCCESS(status))
        return status;
    out[0] = vid_did;
    out[1] = (OOBMSM_BUS << 16) | (OOBMSM_DEV << 8) | OOBMSM_FN;
    out[2] = g_bar_addr;
    out[3] = g_bar_size;
    return STATUS_SUCCESS;
}


NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();
    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    debug_print(''IntelOOBMSM: family: %x model: %x\n'', family, model);

    new CodeName:code_name = get_code_name(family, model);
    if (code_name == CPU_UNKNOWN)
        return STATUS_NOT_SUPPORTED;

    return oobmsm_init();
}

public NTSTATUS:unload() {
    if (g_bar_va != NULL) {
        io_space_unmap(g_bar_va, g_bar_size);
        g_bar_va = NULL;
    }
    return STATUS_SUCCESS;
}
