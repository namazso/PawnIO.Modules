// PawnIO Modules - Modules for various hardware to be used with PawnIO.
// Copyright (C) 2026 Thermal Widget contributors
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// SPDX-License-Identifier: LGPL-2.1-or-later

// Read-only NVIDIA Blackwell BJT temperature access.
// Maps only the 4 KB BAR0 page containing the six BJT sensor registers.

#include <pawnio.inc>

#define PCI_VENDOR_NVIDIA 0x10DE
#define PCI_VENDOR_OFFSET 0x00
#define PCI_CLASS_OFFSET  0x08
#define PCI_BAR0_LOW      0x10
#define PCI_BAR0_HIGH     0x14

#define PCI_CLASS_DISPLAY 0x03
#define BJT_PAGE_OFFSET   0xAD0000
#define BJT_FIRST_OFFSET  0x0A90
#define BJT_SENSOR_COUNT  6
#define BJT_MAP_SIZE      0x1000
#define MAX_GPUS          8

new g_gpu_count = 0;
new g_gpu_bus[MAX_GPUS];
new g_gpu_device[MAX_GPUS];
new g_gpu_function[MAX_GPUS];
new VA:g_bjt_page[MAX_GPUS];

bool:is_valid_bdf(bus, device, function) {
    return bus >= 0 && bus < 256
        && device >= 0 && device < 32
        && function >= 0 && function < 8;
}

bool:is_blackwell_display_gpu(bus, device, function) {
    new id = 0;
    if (!NT_SUCCESS(pci_config_read_dword(bus, device, function, PCI_VENDOR_OFFSET, id)))
        return false;
    if ((id & 0xFFFF) != PCI_VENDOR_NVIDIA)
        return false;

    // NVIDIA's published device table assigns RTX Blackwell to 0x2Bxx-0x2Fxx.
    new device_id = (id >>> 16) & 0xFFFF;
    if (device_id < 0x2B00 || device_id > 0x2FFF)
        return false;

    new class_revision = 0;
    if (!NT_SUCCESS(pci_config_read_dword(bus, device, function, PCI_CLASS_OFFSET, class_revision)))
        return false;

    return ((class_revision >>> 24) & 0xFF) == PCI_CLASS_DISPLAY;
}

find_gpu(bus, device, function) {
    for (new index = 0; index < g_gpu_count; index++) {
        if (g_gpu_bus[index] == bus
            && g_gpu_device[index] == device
            && g_gpu_function[index] == function)
            return index;
    }
    return -1;
}

NTSTATUS:map_gpu(bus, device, function, &index) {
    index = find_gpu(bus, device, function);
    if (index >= 0)
        return STATUS_SUCCESS;
    if (g_gpu_count >= MAX_GPUS)
        return STATUS_INSUFFICIENT_RESOURCES;
    if (!is_valid_bdf(bus, device, function)
        || !is_blackwell_display_gpu(bus, device, function))
        return STATUS_NOT_SUPPORTED;

    new bar_low = 0;
    new bar_high = 0;
    new NTSTATUS:status = pci_config_read_dword(bus, device, function, PCI_BAR0_LOW, bar_low);
    if (!NT_SUCCESS(status))
        return status;

    // Supported NVIDIA GPUs expose BAR0 as a 64-bit memory BAR.
    if ((bar_low & 0x1) != 0 || ((bar_low >>> 1) & 0x3) != 0x2)
        return STATUS_NOT_SUPPORTED;

    status = pci_config_read_dword(bus, device, function, PCI_BAR0_HIGH, bar_high);
    if (!NT_SUCCESS(status))
        return status;

    new bar0 = ((bar_high & 0xFFFFFFFF) << 32) | (bar_low & 0xFFFFFFF0);
    if (bar0 == 0)
        return STATUS_NOT_SUPPORTED;

    new VA:page = io_space_map(bar0 + BJT_PAGE_OFFSET, BJT_MAP_SIZE);
    if (page == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    index = g_gpu_count++;
    g_gpu_bus[index] = bus;
    g_gpu_device[index] = device;
    g_gpu_function[index] = function;
    g_bjt_page[index] = page;
    return STATUS_SUCCESS;
}

/// Validate and map one NVIDIA Blackwell GPU.
///
/// @param in [0..2] PCI bus, device, and function.
/// @param in_size Must be 3.
/// @param out [0] Module GPU index.
/// @param out_size Must be 1.
/// @return An NTSTATUS.
DEFINE_IOCTL_SIZED(ioctl_open_gpu, 3, 1) {
    new index = -1;
    new NTSTATUS:status = map_gpu(in[0], in[1], in[2], index);
    out[0] = index;
    return status;
}

/// Read the six consecutive Blackwell BJT registers.
///
/// @param in [0] Module GPU index.
/// @param in_size Must be 1.
/// @param out [0..5] Raw BJT register values.
/// @param out_size Must be 6.
/// @return An NTSTATUS.
DEFINE_IOCTL_SIZED(ioctl_read_bjt, 1, BJT_SENSOR_COUNT) {
    new index = in[0];
    if (index < 0 || index >= g_gpu_count)
        return STATUS_INVALID_PARAMETER;

    for (new sensor = 0; sensor < BJT_SENSOR_COUNT; sensor++) {
        new NTSTATUS:status = virtual_read_dword(
            g_bjt_page[index] + BJT_FIRST_OFFSET + sensor * 4,
            out[sensor]
        );
        if (!NT_SUCCESS(status))
            return status;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    return get_arch() == ARCH_X64 ? STATUS_SUCCESS : STATUS_NOT_SUPPORTED;
}

public NTSTATUS:unload() {
    for (new index = 0; index < g_gpu_count; index++) {
        if (g_bjt_page[index] != NULL) {
            io_space_unmap(g_bjt_page[index], BJT_MAP_SIZE);
            g_bjt_page[index] = NULL;
        }
    }
    g_gpu_count = 0;
    return STATUS_SUCCESS;
}
