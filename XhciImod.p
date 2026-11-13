//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Abstract
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

// xHCI interrupter moderation (IMOD).
//
// Reads and sets the Interrupter Moderation register of USB 3 (xHCI) host controllers, the job
// tools currently do through WinRing0/inpoutx64-style raw physical memory access. Access is
// limited to exactly one register type: IMOD of interrupter N of a PCI device whose class code is
// 0C0330 (xHCI). Only the IMODI field (bits 15:0, the moderation interval in 250 ns units) can be
// written; IMODC (bits 31:16, the down-counter) is written back as read.
//
// Reference: eXtensible Host Controller Interface for Universal Serial Bus (xHCI), Revision 1.2,
//   5.3.1 CAPLENGTH, 5.3.2 HCIVERSION, 5.3.3 HCSPARAMS1 (MaxIntrs bits 18:8), 5.3.8 RTSOFF (bits 31:5),
//   5.5 Host Controller Runtime Registers (Interrupter Register Set n at RTSOFF + 0x20 + 32 * n),
//   5.5.2.2 Interrupter Moderation Register (IMOD) at offset 0x04 of the set: IMODI 15:0, IMODC 31:16.
// PCI class code 0C0330 = Serial bus controller / USB / xHCI (PCI Code and ID Assignment Spec).
//
// Every access re-reads BAR0 and the command register, so a controller that was moved by a resource
// rebalance or stopped/powered down is refused instead of writing to a stale address.

#define PCI_CFG_VENDOR          0x00
#define PCI_CFG_COMMAND         0x04
#define PCI_CFG_CLASS_REV       0x08
#define PCI_CFG_HEADER_TYPE     0x0E
#define PCI_CFG_BAR0_LOW        0x10
#define PCI_CFG_BAR0_HIGH       0x14
#define PCI_COMMAND_MEMORY      0x02
#define PCI_BAR_IO_SPACE        0x1
#define PCI_BAR_MEM_TYPE_MASK   0x6
#define PCI_BAR_MEM_TYPE_64BIT  0x4
#define PCI_BAR_MEM_ADDR_MASK   0xFFFFFFF0

#define PCI_CLASS_XHCI          0x0C0330

#define XHCI_CAP_MAP_SIZE       0x20
#define XHCI_CAP_CAPLENGTH      0x00 // byte 0: CAPLENGTH, bytes 2-3: HCIVERSION
#define XHCI_CAP_HCSPARAMS1     0x04
#define XHCI_CAP_RTSOFF         0x18
#define XHCI_RT_IR0             0x20
#define XHCI_IR_STRIDE          0x20
#define XHCI_IR_IMOD            0x04
#define XHCI_MMIO_MIN_SIZE      0x10000 // the runtime registers used here must sit inside the first 64 KiB
#define XHCI_MAX_INTRS          1024

#define MAX_CONTROLLERS         16

new g_count = 0;
new g_bdf[MAX_CONTROLLERS];       // (bus << 16) | (device << 8) | function
new g_bar[MAX_CONTROLLERS];       // BAR0 physical address seen at load
new g_caps[MAX_CONTROLLERS];      // (HCIVERSION << 16) | CAPLENGTH
new g_max_intrs[MAX_CONTROLLERS]; // HCSPARAMS1.MaxIntrs
new g_rtsoff[MAX_CONTROLLERS];    // RTSOFF (32-byte aligned offset of the runtime registers)

bdf_bus(bdf) { return (bdf >> 16) & 0xFF; }
bdf_dev(bdf) { return (bdf >> 8) & 0x1F; }
bdf_fn(bdf)  { return bdf & 0x7; }

// Current BAR0 of a function, 0 when it isn't a decoding memory BAR.
read_bar0(bus, dev, fn) {
    new command = 0;
    if (!NT_SUCCESS(pci_config_read_word(bus, dev, fn, PCI_CFG_COMMAND, command)))
        return 0;
    if ((command & PCI_COMMAND_MEMORY) == 0)
        return 0;

    new lo = 0;
    new hi = 0;
    if (!NT_SUCCESS(pci_config_read_dword(bus, dev, fn, PCI_CFG_BAR0_LOW, lo)))
        return 0;
    if (lo == 0xFFFFFFFF || (lo & PCI_BAR_IO_SPACE) != 0)
        return 0;
    if ((lo & PCI_BAR_MEM_TYPE_MASK) == PCI_BAR_MEM_TYPE_64BIT) {
        if (!NT_SUCCESS(pci_config_read_dword(bus, dev, fn, PCI_CFG_BAR0_HIGH, hi)))
            return 0;
    }
    return ((hi & 0xFFFFFFFF) << 32) | (lo & PCI_BAR_MEM_ADDR_MASK);
}

bool:is_xhci(bus, dev, fn) {
    new class_rev = 0;
    if (!NT_SUCCESS(pci_config_read_dword(bus, dev, fn, PCI_CFG_CLASS_REV, class_rev)))
        return false;
    return ((class_rev >> 8) & 0xFFFFFF) == PCI_CLASS_XHCI;
}

// Reads the capability registers of one controller and records it when they look sane.
probe(bus, dev, fn) {
    if (g_count >= MAX_CONTROLLERS)
        return;

    new bar = read_bar0(bus, dev, fn);
    if (bar == 0)
        return;

    new VA:va = io_space_map(bar, XHCI_CAP_MAP_SIZE);
    if (va == NULL)
        return;

    new cap0 = 0;
    new hcs1 = 0;
    new rtsoff = 0;
    new NTSTATUS:status = virtual_read_dword(va + XHCI_CAP_CAPLENGTH, cap0);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(va + XHCI_CAP_HCSPARAMS1, hcs1);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(va + XHCI_CAP_RTSOFF, rtsoff);
    io_space_unmap(va, XHCI_CAP_MAP_SIZE);
    if (!NT_SUCCESS(status))
        return;

    cap0 &= 0xFFFFFFFF;
    if (cap0 == 0xFFFFFFFF) // powered down / not decoding
        return;

    new caplength = cap0 & 0xFF;
    new hciversion = (cap0 >> 16) & 0xFFFF;
    new max_intrs = (hcs1 >> 8) & 0x7FF;
    rtsoff &= 0xFFFFFFE0;

    if (caplength < 0x20 || hciversion < 0x0090 || hciversion > 0x01FF)
        return;
    if (max_intrs < 1 || max_intrs > XHCI_MAX_INTRS)
        return;
    if (rtsoff < XHCI_CAP_MAP_SIZE || rtsoff + XHCI_RT_IR0 + XHCI_IR_STRIDE * max_intrs > XHCI_MMIO_MIN_SIZE)
        return;

    g_bdf[g_count] = (bus << 16) | (dev << 8) | fn;
    g_bar[g_count] = bar;
    g_caps[g_count] = (hciversion << 16) | caplength;
    g_max_intrs[g_count] = max_intrs;
    g_rtsoff[g_count] = rtsoff;
    debug_print(''XhciImod: %x:%x.%x BAR0 %x, HCIVERSION %x, MaxIntrs %d, RTSOFF %x\n'', bus, dev, fn, bar, hciversion, max_intrs, rtsoff);
    ++g_count;
}

// Physical address of IMOD of interrupter `ir` on controller `idx`, after re-checking the controller.
// Returns 0 when the access must be refused.
imod_address(idx, ir) {
    if (idx < 0 || idx >= g_count)
        return 0;
    if (ir < 0 || ir >= g_max_intrs[idx])
        return 0;
    new bdf = g_bdf[idx];
    if (!is_xhci(bdf_bus(bdf), bdf_dev(bdf), bdf_fn(bdf)))
        return 0;
    if (read_bar0(bdf_bus(bdf), bdf_dev(bdf), bdf_fn(bdf)) != g_bar[idx])
        return 0;
    return g_bar[idx] + g_rtsoff[idx] + XHCI_RT_IR0 + XHCI_IR_STRIDE * ir + XHCI_IR_IMOD;
}

/// List the xHCI controllers found at load.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = number of controllers (at most 16),
///            [1..16] = (bus << 16) | (device << 8) | function of each, 0 for unused slots
/// @param out_size Must be 17
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_controllers, 0, 17) {
    out[0] = g_count;
    for (new i = 0; i < MAX_CONTROLLERS; ++i)
        out[i + 1] = i < g_count ? g_bdf[i] : 0;
    return STATUS_SUCCESS;
}

/// Get the capability values of one controller.
///
/// @param in [0] = controller index (0 .. count-1)
/// @param in_size Must be 1
/// @param out [0] = (bus << 16) | (device << 8) | function
///            [1] = BAR0 physical address
///            [2] = (HCIVERSION << 16) | CAPLENGTH
///            [3] = number of interrupters (HCSPARAMS1.MaxIntrs)
///            [4] = RTSOFF
/// @param out_size Must be 5
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_info, 1, 5) {
    new idx = in[0];
    if (idx < 0 || idx >= g_count)
        return STATUS_INVALID_PARAMETER;
    out[0] = g_bdf[idx];
    out[1] = g_bar[idx];
    out[2] = g_caps[idx];
    out[3] = g_max_intrs[idx];
    out[4] = g_rtsoff[idx];
    return STATUS_SUCCESS;
}

/// Read the Interrupter Moderation register (IMOD) of one interrupter.
///
/// @param in [0] = controller index, [1] = interrupter number (0 .. MaxIntrs-1)
/// @param in_size Must be 2
/// @param out [0] = IMOD (IMODC << 16 | IMODI)
/// @param out_size Must be 1
/// @return An NTSTATUS; STATUS_DEVICE_NOT_READY when the controller moved, stopped or is powered down
DEFINE_IOCTL_SIZED(ioctl_read_imod, 2, 1) {
    if (in[0] < 0 || in[0] >= g_count || in[1] < 0 || in[1] >= g_max_intrs[in[0]])
        return STATUS_INVALID_PARAMETER;
    new pa = imod_address(in[0], in[1]);
    if (pa == 0)
        return STATUS_DEVICE_NOT_READY;
    new VA:va = io_space_map(pa, 4);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;
    new value = 0;
    new NTSTATUS:status = virtual_read_dword(va, value);
    io_space_unmap(va, 4);
    if (NT_SUCCESS(status) && (value & 0xFFFFFFFF) == 0xFFFFFFFF)
        status = STATUS_DEVICE_NOT_READY;
    out[0] = value & 0xFFFFFFFF;
    return status;
}

/// Set the moderation interval (IMODI, bits 15:0) of one interrupter. IMODC is written back as read.
///
/// @param in [0] = controller index, [1] = interrupter number, [2] = interval in 250 ns units (0 .. 0xFFFF; 0 = off)
/// @param in_size Must be 3
/// @param out Unused
/// @param out_size Must be 0
/// @return An NTSTATUS; STATUS_DEVICE_NOT_READY when the controller moved, stopped or is powered down
DEFINE_IOCTL_SIZED(ioctl_write_imod, 3, 0) {
    if (in[0] < 0 || in[0] >= g_count || in[1] < 0 || in[1] >= g_max_intrs[in[0]])
        return STATUS_INVALID_PARAMETER;
    if (in[2] < 0 || in[2] > 0xFFFF)
        return STATUS_INVALID_PARAMETER;
    new pa = imod_address(in[0], in[1]);
    if (pa == 0)
        return STATUS_DEVICE_NOT_READY;
    new VA:va = io_space_map(pa, 4);
    if (va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;
    new cur = 0;
    new NTSTATUS:status = virtual_read_dword(va, cur);
    if (NT_SUCCESS(status)) {
        cur &= 0xFFFFFFFF;
        if (cur == 0xFFFFFFFF)
            status = STATUS_DEVICE_NOT_READY;
        else
            status = virtual_write_dword(va, (cur & 0xFFFF0000) | in[2]);
    }
    io_space_unmap(va, 4);
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    for (new bus = 0; bus < 256; ++bus) {
        for (new dev = 0; dev < 32; ++dev) {
            new vid = 0;
            if (!NT_SUCCESS(pci_config_read_dword(bus, dev, 0, PCI_CFG_VENDOR, vid)))
                continue;
            if ((vid & 0xFFFF) == 0xFFFF || (vid & 0xFFFF) == 0)
                continue;
            new header = 0;
            pci_config_read_byte(bus, dev, 0, PCI_CFG_HEADER_TYPE, header);
            new fns = (header & 0x80) != 0 ? 8 : 1;
            for (new fn = 0; fn < fns; ++fn) {
                if (fn != 0) {
                    new v = 0;
                    if (!NT_SUCCESS(pci_config_read_dword(bus, dev, fn, PCI_CFG_VENDOR, v)) || (v & 0xFFFF) == 0xFFFF)
                        continue;
                }
                if (is_xhci(bus, dev, fn))
                    probe(bus, dev, fn);
            }
        }
    }

    if (g_count == 0)
        return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}
