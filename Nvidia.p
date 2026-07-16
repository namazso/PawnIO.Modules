//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  CapFrameX
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

#define PCI_VENDOR_ID_NVIDIA       0x10DE

#define PCI_COMMAND                0x04
#define PCI_COMMAND_MEMORY         0x02
#define PCI_CLASS_REVISION         0x08
#define PCI_CLASS_DISPLAY          0x03
#define PCI_BAR0_LOW               0x10
#define PCI_BAR0_HIGH              0x14
#define PCI_BAR_IO                 0x01
#define PCI_BAR_TYPE_MASK          0x06
#define PCI_BAR_TYPE_32            0x00
#define PCI_BAR_TYPE_64            0x04

#define NV_PMC_BOOT_0              0x000000
#define NV_PMC_BOOT_0_SIZE         0x04
#define NV_PMC_BOOT_0_CHIP_SHIFT   20
#define NV_PMC_BOOT_0_CHIP_MASK    0xFFF
#define NV_PMC_BOOT_0_GB202        0x1B2
#define NV_PMC_BOOT_0_GB203        0x1B3
#define NV_PMC_BOOT_0_GB205        0x1B5
#define NV_PMC_BOOT_0_GB206        0x1B6
#define NV_PMC_BOOT_0_GB207        0x1B7

#define THERMAL_MMIO_BASE          0xAD0A90
#define THERMAL_MMIO_SIZE          0x54
#define THERMAL_HOT_SPOT_OFFSET     0x00
#define THERMAL_HOT_SPOT_2_OFFSET   0x50

// Cache one target GPU per module instance. PCI identity and BAR0 are still
// checked on every request; the mapping is only replaced when either changes.
new g_thermal_bdf = -1;
new g_thermal_didvid = 0;
new g_thermal_bar0 = 0;
new VA:g_thermal_va = NULL;

NTSTATUS:get_bar0(bus, device, function, &didvid, &bar0) {
    didvid = 0;
    bar0 = 0;

    new NTSTATUS:status = pci_config_read_dword(bus, device, function, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;
    if ((didvid & 0xFFFF) != PCI_VENDOR_ID_NVIDIA)
        return STATUS_NOT_SUPPORTED;

    new class_revision = 0;
    status = pci_config_read_dword(bus, device, function, PCI_CLASS_REVISION, class_revision);
    if (!NT_SUCCESS(status))
        return status;
    if (((class_revision >> 24) & 0xFF) != PCI_CLASS_DISPLAY)
        return STATUS_NOT_SUPPORTED;

    new command = 0;
    status = pci_config_read_word(bus, device, function, PCI_COMMAND, command);
    if (!NT_SUCCESS(status))
        return status;
    if (!(command & PCI_COMMAND_MEMORY))
        return STATUS_DEVICE_NOT_READY;

    new bar_low = 0;
    status = pci_config_read_dword(bus, device, function, PCI_BAR0_LOW, bar_low);
    if (!NT_SUCCESS(status))
        return status;
    if (bar_low & PCI_BAR_IO)
        return STATUS_NOT_SUPPORTED;

    new bar_type = bar_low & PCI_BAR_TYPE_MASK;
    if (bar_type == PCI_BAR_TYPE_32) {
        bar0 = bar_low & 0xFFFFFFF0;
    } else if (bar_type == PCI_BAR_TYPE_64) {
        new bar_high = 0;
        status = pci_config_read_dword(bus, device, function, PCI_BAR0_HIGH, bar_high);
        if (!NT_SUCCESS(status))
            return status;
        bar0 = ((bar_high & 0xFFFFFFFF) << 32) | (bar_low & 0xFFFFFFF0);
    } else {
        return STATUS_NOT_SUPPORTED;
    }

    return bar0 == 0 ? STATUS_NOT_SUPPORTED : STATUS_SUCCESS;
}

NTSTATUS:check_gpu_architecture(bar0) {
    new VA:va = io_space_map(bar0 + NV_PMC_BOOT_0, NV_PMC_BOOT_0_SIZE);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    new boot0 = 0;
    new NTSTATUS:status = virtual_read_dword(va, boot0);
    io_space_unmap(va, NV_PMC_BOOT_0_SIZE);
    if (!NT_SUCCESS(status))
        return status;

    switch ((boot0 >> NV_PMC_BOOT_0_CHIP_SHIFT) & NV_PMC_BOOT_0_CHIP_MASK) {
        case NV_PMC_BOOT_0_GB202,
             NV_PMC_BOOT_0_GB203,
             NV_PMC_BOOT_0_GB205,
             NV_PMC_BOOT_0_GB206,
             NV_PMC_BOOT_0_GB207:
            return STATUS_SUCCESS;
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

unmap_thermal_registers() {
    if (g_thermal_va != NULL)
        io_space_unmap(g_thermal_va, THERMAL_MMIO_SIZE);

    g_thermal_bdf = -1;
    g_thermal_didvid = 0;
    g_thermal_bar0 = 0;
    g_thermal_va = NULL;
}

bool:thermal_mapping_matches(bdf, didvid, bar0) {
    return g_thermal_va != NULL
        && g_thermal_bdf == bdf
        && g_thermal_didvid == didvid
        && g_thermal_bar0 == bar0;
}

NTSTATUS:ensure_thermal_mapping(bdf, didvid, bar0) {
    if (thermal_mapping_matches(bdf, didvid, bar0))
        return STATUS_SUCCESS;

    unmap_thermal_registers();

    new NTSTATUS:status = check_gpu_architecture(bar0);
    if (!NT_SUCCESS(status))
        return status;

    new VA:va = io_space_map(bar0 + THERMAL_MMIO_BASE, THERMAL_MMIO_SIZE);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_thermal_bdf = bdf;
    g_thermal_didvid = didvid;
    g_thermal_bar0 = bar0;
    g_thermal_va = va;
    return STATUS_SUCCESS;
}

/// Read NVIDIA GB20x thermal registers.
///
/// @param in [0] = Bus, [1] = Device, [2] = Function
/// @param in_size Must be 3
/// @param out Raw values for Hot Spot and Hot Spot #2
/// @param out_size Must be 2
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_thermal_registers, 3, 2) {
    new bus = in[0];
    new device = in[1];
    new function = in[2];

    if (bus < 0 || bus > 0xFF || device < 0 || device > 0x1F || function < 0 || function > 0x07)
        return STATUS_INVALID_PARAMETER;

    new bdf = (bus << 8) | (device << 3) | function;
    new didvid = 0;
    new bar0 = 0;
    new NTSTATUS:status = get_bar0(bus, device, function, didvid, bar0);
    if (!NT_SUCCESS(status)) {
        if (g_thermal_bdf == bdf)
            unmap_thermal_registers();
        return status;
    }

    status = ensure_thermal_mapping(bdf, didvid, bar0);
    if (!NT_SUCCESS(status))
        return status;

    status = virtual_read_dword(g_thermal_va + THERMAL_HOT_SPOT_OFFSET, out[0]);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(g_thermal_va + THERMAL_HOT_SPOT_2_OFFSET, out[1]);
    if (!NT_SUCCESS(status))
        unmap_thermal_registers();

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    unmap_thermal_registers();
    return STATUS_SUCCESS;
}
