//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  M-Control Contributors
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

// Read-only Intel PCH temperature module.
//
// Two register layouts are supported:
//  - the documented Intel PCH thermal PCI function, whose BAR0 temperature
//    register has 0.5 C resolution and a -50 C offset;
//  - the fixed PWRM MMIO window used by recent Intel client PCH generations,
//    where PWRMBASE + 0x1560 carries a valid bit and an integer Celsius value.

#define PCI_VENDOR_ID_INTEL             0x8086

#define PCI_BUS_PCH                     0x00
#define PCI_MAX_DEVICE                  0x20
#define PCI_MAX_FUNCTION                0x08

#define PCI_CFG_VENDOR_DEVICE           0x00
#define PCI_CFG_BAR0_LOW                0x10
#define PCI_CFG_BAR0_HIGH               0x14
#define PCI_BAR_IO_SPACE                0x01
#define PCI_BAR_MEM_TYPE_MASK           0x06
#define PCI_BAR_MEM_TYPE_64BIT          0x04
#define PCI_BAR_MEM_ADDR_MASK           0xFFFFFFF0

#define LEGACY_TEMP_OFFSET              0x00
#define LEGACY_TEMP_MAP_SIZE            0x100

#define MODERN_PWRM_BASE                0xFE000000
#define MODERN_TEMP_OFFSET              0x1560
#define MODERN_TEMP_MAP_SIZE            0x2000

#define PCH_THERMAL_ROUTE_NONE          0
#define PCH_THERMAL_ROUTE_PCI_BAR0      1
#define PCH_THERMAL_ROUTE_FIXED_PWRM    2

new g_route = PCH_THERMAL_ROUTE_NONE;
new g_didvid = 0;
new g_pci_address = 0;
new g_mmio_addr = 0;
new g_mmio_size = 0;
new VA:g_mmio_va = NULL;

bool:is_legacy_thermal_device(device_id) {
    switch (device_id) {
        // Linux intel_pch_thermal device IDs, plus 0xA2B1 observed in
        // HWMonitor's legacy thermal-controller route.
        case 0x9C24, 0x8C24, 0x9CA4, 0x9D31, 0xA131, 0x9DF9,
             0xA379, 0x02F9, 0x06F9, 0xA1B1, 0x8D24, 0xA2B1:
            return true;
    }
    return false;
}

bool:is_modern_lpc_device(device_id) {
    switch (device_id) {
        // LPC/eSPI IDs for which HWMonitor 1.65 probes the fixed PWRM window.
        case 0x4381, 0x4385, 0x4386, 0x4387, 0x4388, 0x4389, 0x438B, 0x438F,
             0x7A83, 0x7A84, 0x7A85, 0x7A86, 0x7A87, 0x7A88,
             0x7A04, 0x7A05, 0x7A06, 0x7A0C,
             0xAE0C, 0xAE0D, 0x7F0C, 0x7F0D, 0x7F12,
             0x5182, 0x519D, 0x7E02, 0xA807, 0x7702, 0x5481,
             0xE302, 0xE402:
            return true;
    }
    return false;
}

NTSTATUS:find_intel_device(bool:modern, &didvid, &pci_address) {
    for (new device = 0; device < PCI_MAX_DEVICE; ++device) {
        for (new function = 0; function < PCI_MAX_FUNCTION; ++function) {
            new value = 0;
            new NTSTATUS:status = pci_config_read_dword(
                PCI_BUS_PCH, device, function, PCI_CFG_VENDOR_DEVICE, value);
            if (!NT_SUCCESS(status))
                continue;

            new vendor_id = value & 0xFFFF;
            if (vendor_id != PCI_VENDOR_ID_INTEL)
                continue;

            new device_id = (value >> 16) & 0xFFFF;
            if ((modern && is_modern_lpc_device(device_id)) ||
                (!modern && is_legacy_thermal_device(device_id))) {
                didvid = value;
                pci_address = (PCI_BUS_PCH << 16) | (device << 8) | function;
                return STATUS_SUCCESS;
            }
        }
    }

    return STATUS_NO_SUCH_DEVICE;
}

NTSTATUS:map_fixed_pwrm() {
    new didvid = 0;
    new pci_address = 0;
    new NTSTATUS:status = find_intel_device(true, didvid, pci_address);
    if (!NT_SUCCESS(status))
        return status;

    new VA:mmio_va = io_space_map(MODERN_PWRM_BASE, MODERN_TEMP_MAP_SIZE);
    if (mmio_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_route = PCH_THERMAL_ROUTE_FIXED_PWRM;
    g_didvid = didvid;
    g_pci_address = pci_address;
    g_mmio_addr = MODERN_PWRM_BASE;
    g_mmio_size = MODERN_TEMP_MAP_SIZE;
    g_mmio_va = mmio_va;
    return STATUS_SUCCESS;
}

NTSTATUS:map_legacy_thermal_bar() {
    new didvid = 0;
    new pci_address = 0;
    new NTSTATUS:status = find_intel_device(false, didvid, pci_address);
    if (!NT_SUCCESS(status))
        return status;

    new device = (pci_address >> 8) & 0xFF;
    new function = pci_address & 0xFF;
    new base_lo = 0;
    new base_hi = 0;

    status = pci_config_read_dword(
        PCI_BUS_PCH, device, function, PCI_CFG_BAR0_LOW, base_lo);
    if (!NT_SUCCESS(status))
        return status;
    if (base_lo == 0 || base_lo == 0xFFFFFFFF || (base_lo & PCI_BAR_IO_SPACE))
        return STATUS_NOT_SUPPORTED;

    if ((base_lo & PCI_BAR_MEM_TYPE_MASK) == PCI_BAR_MEM_TYPE_64BIT) {
        status = pci_config_read_dword(
            PCI_BUS_PCH, device, function, PCI_CFG_BAR0_HIGH, base_hi);
        if (!NT_SUCCESS(status))
            return status;
    }

    new bar_addr = ((base_hi & 0xFFFFFFFF) << 32) |
        (base_lo & PCI_BAR_MEM_ADDR_MASK);
    if (bar_addr == 0)
        return STATUS_NOT_SUPPORTED;

    new VA:mmio_va = io_space_map(bar_addr, LEGACY_TEMP_MAP_SIZE);
    if (mmio_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_route = PCH_THERMAL_ROUTE_PCI_BAR0;
    g_didvid = didvid;
    g_pci_address = pci_address;
    g_mmio_addr = bar_addr;
    g_mmio_size = LEGACY_TEMP_MAP_SIZE;
    g_mmio_va = mmio_va;
    return STATUS_SUCCESS;
}

/// Read the current raw PCH temperature register.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = raw temperature register
///            [1] = route (1 = PCI thermal BAR0, 2 = fixed PWRM window)
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @note Consumers must validate and convert the raw value for the returned route.
DEFINE_IOCTL_SIZED(ioctl_read_raw, 0, 2) {
    if (g_mmio_va == NULL || g_route == PCH_THERMAL_ROUTE_NONE)
        return STATUS_DEVICE_NOT_READY;

    new raw = 0;
    new NTSTATUS:status;

    if (g_route == PCH_THERMAL_ROUTE_FIXED_PWRM) {
        status = virtual_read_dword(g_mmio_va + MODERN_TEMP_OFFSET, raw);
        if (!NT_SUCCESS(status))
            return status;
    }
    else if (g_route == PCH_THERMAL_ROUTE_PCI_BAR0) {
        status = virtual_read_word(g_mmio_va + LEGACY_TEMP_OFFSET, raw);
        if (!NT_SUCCESS(status))
            return status;
    }
    else {
        return STATUS_NOT_SUPPORTED;
    }

    out[0] = raw;
    out[1] = g_route;
    return STATUS_SUCCESS;
}

/// Return the selected read-only register route and mapping information.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = route (1 = PCI thermal BAR0, 2 = fixed PWRM window)
///            [1] = (device ID << 16) | vendor ID
///            [2] = (bus << 16) | (device << 8) | function
///            [3] = physical MMIO base
///            [4] = mapped size in bytes
/// @param out_size Must be 5
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 5) {
    if (g_mmio_va == NULL || g_route == PCH_THERMAL_ROUTE_NONE)
        return STATUS_DEVICE_NOT_READY;

    out[0] = g_route;
    out[1] = g_didvid;
    out[2] = g_pci_address;
    out[3] = g_mmio_addr;
    out[4] = g_mmio_size;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    // HWMonitor uses the modern PWRM route whenever a supported LPC/eSPI
    // controller is present, and otherwise falls back to the PCI thermal BAR.
    new NTSTATUS:status = map_fixed_pwrm();
    if (NT_SUCCESS(status))
        return STATUS_SUCCESS;

    return map_legacy_thermal_bar();
}

public NTSTATUS:unload() {
    if (g_mmio_va != NULL) {
        io_space_unmap(g_mmio_va, g_mmio_size);
        g_mmio_va = NULL;
    }
    g_route = PCH_THERMAL_ROUTE_NONE;
    g_mmio_addr = 0;
    g_mmio_size = 0;
    return STATUS_SUCCESS;
}
