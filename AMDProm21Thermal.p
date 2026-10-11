//  PawnIO AMD Promontory 21 thermal module - reads the AMD Promontory 21
//  chipset temperature sensor exposed via the xHCI controller's
//  vendor-specific MMIO registers.
//  Copyright (C) 2026  TempMonitor project
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
//  02110-1301  USA
//
//  SPDX-License-Identifier: LGPL-2.1-or-later

//  Protocol facts (REQ-039), sourced from the Linux prom21-xhci driver (v7,
//  lore.kernel.org 20260519000732.2334711):
//  - The sensor lives behind AMD Promontory 21 xHCI controllers,
//    PCI vendor 0x1022, device ids 0x43F7 (single-die Prom21, found on B650
//    boards) and 0x43FC / 0x43FD (dual-die Prom21, matched by the kernel driver).
//  - On each controller's MMIO BAR0 there is a vendor-specific
//    index/data register pair at offsets 0x3000 / 0x3008.
//  - Write the temperature selector 0x0001E520 to the index register,
//    then read the data register: the low 8 bits are the raw reading.
//  - The previous index value is saved and restored around every read
//    (the index register is shared with other users, e.g. firmware).
//
//  Client contract (cells are 8 bytes, -C64; the driver does not propagate
//  module NTSTATUS to the host, so unavailability is expressed via out values):
//
//    ioctl_get_count(in: -, out[1]: number of controllers found and mapped)
//    ioctl_get_bar(in[1]: controller index, out[1]: BAR0 physical address, 0 if unavailable)
//    ioctl_get_temp_raw(in[1]: controller index, out[1]: raw byte 1..255, 0 if invalid)
//
//  Conversion to Celsius happens on the host (kept out of the module so that
//  recalibration never requires a new signed release):
//      celsius = raw * 0.9066 - 78.624
//  The constants were cross-checked against HWiNFO64; see the kernel driver
//  documentation for how they were derived.

#include <pawnio.inc>

// PawnIO AMD Promontory 21 xHCI temperature driver

#define PCI_VENDOR_ID_AMD 0x1022

#define XHCI_DEVICE_0 0x43F7   // 单 die Prom21（B650 等常见 AM5 板型，本机实测）
#define XHCI_DEVICE_1 0x43FC   // 双 die Prom21（X870E，Linux prom21-xhci v7 匹配）
#define XHCI_DEVICE_2 0x43FD   // 双 die Prom21（X870E，Linux prom21-xhci v7 匹配）

// Board-dependent: single-die boards expose one xHCI, dual-die (X870E) two.
// Four slots leave headroom for hybrid board layouts at zero memory cost.
#define MAX_CONTROLLERS 4

#define INDEX_REG_OFFSET 0x3000
#define DATA_REG_OFFSET 0x3008
#define TEMP_SELECTOR 0x0001E520

// The vendor registers live at 0x3000..0x3010, so a fixed 16 KiB window
// always covers them regardless of the actual BAR length. Reads and writes
// are confined to the two vendor registers, so mapping beyond the BAR's own
// size is harmless.
#define MAP_SIZE 0x4000

#define PCI_REG_BAR0 0x10
#define PCI_REG_HEADER_TYPE 0x0E

#define PCI_HEADER_MULTIFUNCTION 0x80
#define PCI_BAR_TYPE_MASK 0x3
#define PCI_BAR_TYPE_64BIT 0x2
#define PCI_BAR_ADDR_MASK 0xFFFFFFF0

new g_bar[MAX_CONTROLLERS];
new VA:g_va[MAX_CONTROLLERS];
new g_count = 0;

// Read the 64-bit memory base address of a BAR0 (offset 0x10, upper half at
// 0x14 only when the BAR declares itself 64-bit).
NTSTATUS:read_bar0(bus, device, function, &addr) {
    new bar_lo = 0;
    new NTSTATUS:status = pci_config_read_dword(bus, device, function, PCI_REG_BAR0, bar_lo);
    if (!NT_SUCCESS(status))
        return status;

    if ((bar_lo & 0x1) != 0)   // I/O space BAR, not our device
        return STATUS_NOT_FOUND;

    new bar_hi = 0;
    if ((bar_lo & PCI_BAR_TYPE_MASK) == PCI_BAR_TYPE_64BIT) {
        status = pci_config_read_dword(bus, device, function, PCI_REG_BAR0 + 4, bar_hi);
        if (!NT_SUCCESS(status))
            return status;
    }

    addr = ((bar_hi << 32) | bar_lo) & PCI_BAR_ADDR_MASK;
    return STATUS_SUCCESS;
}

NTSTATUS:scan_controllers() {
    for (new bus = 0; bus < 256; ++bus) {
        for (new device = 0; device < 32; ++device) {
            // Function 0 决定该设备是否存在与是否多功能——但这只影响扫描深度，
            // 不预设 func0 的 vendor（xHCI 可能是多功能设备中 func0 非 AMD 的一路）。
            new header_type = 0;
            new NTSTATUS:status = pci_config_read_word(bus, device, 0, PCI_REG_HEADER_TYPE, header_type);
            if (!NT_SUCCESS(status))
                continue;
            new max_function = (header_type & PCI_HEADER_MULTIFUNCTION) != 0 ? 8 : 1;

            for (new function = 0; function < max_function; ++function) {
                new vid_did = 0;
                status = pci_config_read_word(bus, device, function, 0, vid_did);
                if (!NT_SUCCESS(status))
                    continue;
                if ((vid_did & 0xFFFF) != PCI_VENDOR_ID_AMD)
                    continue;
                new device_id = (vid_did >>> 16) & 0xFFFF;
                if (device_id != XHCI_DEVICE_0 && device_id != XHCI_DEVICE_1 && device_id != XHCI_DEVICE_2)
                    continue;

                new addr = 0;
                if (!NT_SUCCESS(read_bar0(bus, device, function, addr)))
                    continue;
                if (addr == 0)
                    continue;

                if (g_count >= MAX_CONTROLLERS)
                    return STATUS_SUCCESS;

                g_bar[g_count] = addr;
                g_count++;
            }
        }
    }
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    new NTSTATUS:status = scan_controllers();
    if (!NT_SUCCESS(status))
        return status;

    for (new i = 0; i < g_count; ++i) {
        g_va[i] = io_space_map(g_bar[i], MAP_SIZE);
        // 映射失败只影响该控制器（g_va 保持 NULL），不拖垮整颗模块。
    }

    return STATUS_SUCCESS;
}

/// Number of Prom21 xHCI controllers found and mapped.
///
/// @param in Unused
/// @param out [0] = controller count
DEFINE_IOCTL_SIZED(ioctl_get_count, 0, 1) {
    out[0] = g_count;
    return STATUS_SUCCESS;
}

/// Physical BAR0 address of a controller (diagnostics).
///
/// @param in [0] = controller index
/// @param out [0] = BAR0 physical address, 0 when unavailable
DEFINE_IOCTL_SIZED(ioctl_get_bar, 1, 1) {
    new index = in[0];
    if (index < 0 || index >= MAX_CONTROLLERS)
        return STATUS_INVALID_PARAMETER;

    out[0] = index < g_count ? g_bar[index] : 0;
    return STATUS_SUCCESS;
}

/// Raw temperature reading of one controller (low 8 bits).
///
/// Protocol per read: save index -> write selector -> read data -> restore.
/// @param in [0] = controller index
/// @param out [0] = raw byte, 0 when the reading is invalid
DEFINE_IOCTL_SIZED(ioctl_get_temp_raw, 1, 1) {
    new index = in[0];
    if (index < 0 || index >= MAX_CONTROLLERS)
        return STATUS_INVALID_PARAMETER;
    if (g_va[index] == NULL)
        return STATUS_DEVICE_NOT_READY;

    new previous = 0;
    new NTSTATUS:status = virtual_read_dword(g_va[index] + INDEX_REG_OFFSET, previous);
    if (!NT_SUCCESS(status))
        return status;

    status = virtual_write_dword(g_va[index] + INDEX_REG_OFFSET, TEMP_SELECTOR);
    if (!NT_SUCCESS(status))
        return status;

    new data = 0;
    status = virtual_read_dword(g_va[index] + DATA_REG_OFFSET, data);

    // 恢复原 index 值（驱动纪律：index 寄存器是共享资源，读写序列必须复原）。
    virtual_write_dword(g_va[index] + INDEX_REG_OFFSET, previous);

    if (!NT_SUCCESS(status))
        return status;

    out[0] = data & 0xFF;
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    for (new i = 0; i < MAX_CONTROLLERS; ++i) {
        if (g_va[i] != NULL) {
            io_space_unmap(g_va[i], MAP_SIZE);
            g_va[i] = NULL;
        }
    }
    return STATUS_SUCCESS;
}
