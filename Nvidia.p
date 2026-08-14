//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  CapFrameX, Blacktempel
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
#define THERMAL_MMIO_SIZE          0x18
#define THERMAL_CHANNEL_COUNT      6

#define MEMORY_TEMPERATURE_MMIO_BASE       0x900000
#define MEMORY_TEMPERATURE_MMIO_SIZE       0x60000
#define MEMORY_TEMPERATURE_TOPOLOGY_OFFSET 0x200
#define MEMORY_TEMPERATURE_GROUP_STRIDE    0x4000
#define MEMORY_TEMPERATURE_MASK_OFFSET     0x24D0
#define MEMORY_TEMPERATURE_GROUP_COUNT     24
#define MEMORY_TEMPERATURE_SENSOR_COUNT    48
#define MEMORY_TEMPERATURE_REGISTER_COUNT  8
#define MEMORY_TEMPERATURE_UNAVAILABLE     (-2147483647 - 1)

new const g_memory_temperature_offsets[MEMORY_TEMPERATURE_REGISTER_COUNT] = [
    0x24C0,
    0x24C4,
    0x24C8,
    0x24CC,
    0x1348,
    0x134C,
    0x1350,
    0x1354
];

// Cache one target GPU per module instance. PCI identity and BAR0 are still
// checked on every request; the mapping is only replaced when either changes.
new g_thermal_bdf = -1;
new g_thermal_didvid = 0;
new g_thermal_bar0 = 0;
new VA:g_thermal_va = NULL;

new g_memory_temperature_bdf = -1;
new g_memory_temperature_didvid = 0;
new g_memory_temperature_bar0 = 0;
new VA:g_memory_temperature_va = NULL;

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

NTSTATUS:read_gpu_chip(bar0, &chip) {
    chip = 0;

    new VA:va = io_space_map(bar0 + NV_PMC_BOOT_0, NV_PMC_BOOT_0_SIZE);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    new boot0 = 0;
    new NTSTATUS:status = virtual_read_dword(va, boot0);
    io_space_unmap(va, NV_PMC_BOOT_0_SIZE);
    if (!NT_SUCCESS(status))
        return status;

    chip = (boot0 >> NV_PMC_BOOT_0_CHIP_SHIFT) & NV_PMC_BOOT_0_CHIP_MASK;
    return STATUS_SUCCESS;
}

NTSTATUS:check_thermal_architecture(bar0) {
    new chip = 0;
    new NTSTATUS:status = read_gpu_chip(bar0, chip);
    if (!NT_SUCCESS(status))
        return status;

    switch (chip) {
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

NTSTATUS:check_memory_temp_architecture(bar0) {
    new chip = 0;
    new NTSTATUS:status = read_gpu_chip(bar0, chip);
    if (!NT_SUCCESS(status))
        return status;

    if ((chip >= 0x170 && chip <= 0x17F)
     || (chip >= 0x190 && chip <= 0x19F)
     || (chip >= 0x1B0 && chip <= 0x1BF))
        return STATUS_SUCCESS;

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

    new NTSTATUS:status = check_thermal_architecture(bar0);
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

unmap_memory_temp_registers() {
    if (g_memory_temperature_va != NULL)
        io_space_unmap(g_memory_temperature_va, MEMORY_TEMPERATURE_MMIO_SIZE);

    g_memory_temperature_bdf = -1;
    g_memory_temperature_didvid = 0;
    g_memory_temperature_bar0 = 0;
    g_memory_temperature_va = NULL;
}

bool:memory_temp_mapping_matches(bdf, didvid, bar0) {
    return g_memory_temperature_va != NULL
        && g_memory_temperature_bdf == bdf
        && g_memory_temperature_didvid == didvid
        && g_memory_temperature_bar0 == bar0;
}

NTSTATUS:ensure_memory_temp_mapping(bdf, didvid, bar0) {
    if (memory_temp_mapping_matches(bdf, didvid, bar0))
        return STATUS_SUCCESS;

    unmap_memory_temp_registers();

    new NTSTATUS:status = check_memory_temp_architecture(bar0);
    if (!NT_SUCCESS(status))
        return status;

    new VA:va = io_space_map(
        bar0 + MEMORY_TEMPERATURE_MMIO_BASE,
        MEMORY_TEMPERATURE_MMIO_SIZE);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_memory_temperature_bdf = bdf;
    g_memory_temperature_didvid = didvid;
    g_memory_temperature_bar0 = bar0;
    g_memory_temperature_va = va;
    return STATUS_SUCCESS;
}

/// Read NVIDIA GB20x thermal registers.
///
/// @param in [0] = Bus, [1] = Device, [2] = Function
/// @param in_size Must be 3
/// @param out Six raw DWORDs from BAR0 + 0xAD0A90 through 0xAD0AA4.
///            Channel 0 is the Hot Spot value displayed by HWMonitor 1.65.1;
///            channels 1-5 are additional thermal channels. Consumers must
///            require bit 30 and decode valid samples as
///            (raw & 0xFFFF) / 256.0 degrees Celsius.
/// @param out_size Must be 6
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_thermal_registers, 3, 6) {
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

    for (new channel = 0; channel < THERMAL_CHANNEL_COUNT; channel++) {
        status = virtual_read_dword(g_thermal_va + channel * 4, out[channel]);
        if (!NT_SUCCESS(status)) {
            unmap_thermal_registers();
            return status;
        }
    }

    return STATUS_SUCCESS;
}

/// Read per-chip NVIDIA memory temperatures.
///
/// @param in [0] = Bus, [1] = Device, [2] = Function
/// @param in_size Must be 3
/// @param out [0] = topology, [1] = sensor count, [2..49] = temperatures
///            in degrees Celsius. Unavailable positions contain INT32_MIN.
/// @param out_size Must be 50
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_memory_temperatures, 3, 50) {
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
        if (g_memory_temperature_bdf == bdf)
            unmap_memory_temp_registers();
        return status;
    }

    status = ensure_memory_temp_mapping(bdf, didvid, bar0);
    if (!NT_SUCCESS(status))
        return status;

    new topology = 0;
    status = virtual_read_dword(
        g_memory_temperature_va + MEMORY_TEMPERATURE_TOPOLOGY_OFFSET,
        topology);

    if (!NT_SUCCESS(status)) {
        unmap_memory_temp_registers();
        return status;
    }

    if (topology == 0xFFFFFFFF || (topology & 0xFFFF0000) == 0xBADF0000)
        return STATUS_DEVICE_DATA_ERROR;

    out[0] = topology;
    out[1] = MEMORY_TEMPERATURE_SENSOR_COUNT;

    for (new index = 0; index < MEMORY_TEMPERATURE_SENSOR_COUNT; index++)
        out[index + 2] = MEMORY_TEMPERATURE_UNAVAILABLE;

    for (new index = 0; index < MEMORY_TEMPERATURE_SENSOR_COUNT; index++) {
        new group = 0;
        new chip = 0;
        new first_input = 0;

        if (topology & 0x00400000) {
            group = index >> 2;
            chip = (index >> 1) & 1;
            first_input = index & 1;
        } else {
            group = index >> 1;
            chip = index & 1;
        }

        if (group >= MEMORY_TEMPERATURE_GROUP_COUNT)
            continue;

        new group_offset = group * MEMORY_TEMPERATURE_GROUP_STRIDE;
        new mask_raw = 0;

        status = virtual_read_dword(
            g_memory_temperature_va + group_offset + MEMORY_TEMPERATURE_MASK_OFFSET,
            mask_raw);

        if (!NT_SUCCESS(status)) {
            unmap_memory_temp_registers();
            return status;
        }

        if (mask_raw == 0xFFFFFFFF || (mask_raw & 0xFFFF0000) == 0xBADF0000)
            continue;

        new availability_mask = (mask_raw >>> 24) & 0xFF;
        new bool:has_temperature = false;
        new temperature = MEMORY_TEMPERATURE_UNAVAILABLE;

        for (new input_offset = 0; input_offset <= 2; input_offset += 2) {
            new input = first_input + input_offset;
            new sensor_index = chip + 2 * input;

            if (!(availability_mask & (1 << sensor_index)))
                continue;

            new raw = 0;
            status = virtual_read_dword(
                g_memory_temperature_va
                    + group_offset
                    + g_memory_temperature_offsets[sensor_index],
                raw);

            if (!NT_SUCCESS(status)) {
                unmap_memory_temp_registers();
                return status;
            }

            if (raw == 0xFFFFFFFF || (raw & 0xFFFF0000) == 0xBADF0000)
                continue;

            new current_temperature = (((raw >>> 16) & 0xFF) - 20) * 2;
            if (!has_temperature || current_temperature > temperature)
                temperature = current_temperature;

            has_temperature = true;
        }

        if (has_temperature)
            out[index + 2] = temperature;
    }

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    unmap_thermal_registers();
    unmap_memory_temp_registers();

    return STATUS_SUCCESS;
}
