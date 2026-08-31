//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Ravinity
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

// Restricted AMD xHCI interrupt-moderation module.

#define PCI_VENDOR_ID_AMD              0x1022
#define PCI_DEVICE_ID_AMD_USB_15B7     0x15B7
#define PCI_DEVICE_ID_AMD_USB_43FD     0x43FD

#define PCI_CFG_COMMAND                0x04
#define PCI_CFG_CLASS_REVISION         0x08
#define PCI_CFG_BAR0_LOW               0x10
#define PCI_CFG_BAR0_HIGH              0x14
#define PCI_COMMAND_MEMORY             0x0002
#define PCI_BAR_IO_SPACE               0x0001
#define PCI_BAR_MEM_TYPE_MASK          0x0006
#define PCI_BAR_MEM_TYPE_64BIT         0x0004
#define PCI_BAR_MEM_ADDR_MASK          0xFFFFFFF0

#define XHCI_CLASS_CODE                0x0C033000
#define XHCI_CLASS_MASK                0xFFFFFF00
#define XHCI_CAPLENGTH                 0x00
#define XHCI_HCSPARAMS1                0x04
#define XHCI_RTSOFF                    0x18
#define XHCI_RTSOFF_MASK               0xFFFFFFE0
#define XHCI_INTR_BASE                 0x20
#define XHCI_INTR_STRIDE               0x20
#define XHCI_IMAN                      0x00
#define XHCI_IMOD                      0x04
#define XHCI_ERSTSZ                    0x08
#define XHCI_IMAN_IE                   0x02
#define XHCI_ERSTSZ_MASK               0xFFFF
#define XHCI_IMOD_INTERVAL_MASK        0xFFFF

#define XHCI_PROBE_MAP_SIZE            0x1000
#define XHCI_MAX_MAP_SIZE              0x100000
#define XHCI_PAGE_SIZE                 0x1000

new VA:g_mmio_va = NULL;
new g_mmio_size = 0;
new g_mmio_addr = 0;
new g_didvid = 0;
new g_classrev = 0;
new g_caplength = 0;
new g_hcsparams1 = 0;
new g_rtsoff = 0;
new g_max_intrs = 0;

bool:is_allowed_device(device_id) {
    switch (device_id) {
        case PCI_DEVICE_ID_AMD_USB_15B7, PCI_DEVICE_ID_AMD_USB_43FD:
            return true;
    }
    return false;
}

unmap_controller() {
    if (g_mmio_va != NULL) {
        io_space_unmap(g_mmio_va, g_mmio_size);
        g_mmio_va = NULL;
    }

    g_mmio_size = 0;
    g_mmio_addr = 0;
    g_didvid = 0;
    g_classrev = 0;
    g_caplength = 0;
    g_hcsparams1 = 0;
    g_rtsoff = 0;
    g_max_intrs = 0;
}

NTSTATUS:map_controller(bus, device, function) {
    if (g_mmio_va != NULL)
        return STATUS_DEVICE_BUSY;

    if (bus < 0 || bus > 0xFF || device < 0 || device > 0x1F ||
        function < 0 || function > 0x07)
        return STATUS_INVALID_PARAMETER;

    new didvid = 0;
    new NTSTATUS:status = pci_config_read_dword(
        bus, device, function, 0x00, didvid);
    if (!NT_SUCCESS(status))
        return status;

    new vendor_id = didvid & 0xFFFF;
    new device_id = (didvid >> 16) & 0xFFFF;
    if (vendor_id != PCI_VENDOR_ID_AMD || !is_allowed_device(device_id))
        return STATUS_NOT_SUPPORTED;

    new classrev = 0;
    status = pci_config_read_dword(
        bus, device, function, PCI_CFG_CLASS_REVISION, classrev);
    if (!NT_SUCCESS(status))
        return status;
    if ((classrev & XHCI_CLASS_MASK) != XHCI_CLASS_CODE)
        return STATUS_NOT_SUPPORTED;

    new command = 0;
    status = pci_config_read_word(
        bus, device, function, PCI_CFG_COMMAND, command);
    if (!NT_SUCCESS(status))
        return status;
    if ((command & PCI_COMMAND_MEMORY) == 0)
        return STATUS_DEVICE_NOT_READY;

    new bar_lo = 0;
    new bar_hi = 0;
    status = pci_config_read_dword(
        bus, device, function, PCI_CFG_BAR0_LOW, bar_lo);
    if (!NT_SUCCESS(status))
        return status;
    if (bar_lo == 0 || bar_lo == 0xFFFFFFFF ||
        (bar_lo & PCI_BAR_IO_SPACE) != 0)
        return STATUS_NOT_SUPPORTED;

    if ((bar_lo & PCI_BAR_MEM_TYPE_MASK) == PCI_BAR_MEM_TYPE_64BIT) {
        status = pci_config_read_dword(
            bus, device, function, PCI_CFG_BAR0_HIGH, bar_hi);
        if (!NT_SUCCESS(status))
            return status;
    }

    new bar_addr = ((bar_hi & 0xFFFFFFFF) << 32) |
        (bar_lo & PCI_BAR_MEM_ADDR_MASK);
    if (bar_addr == 0)
        return STATUS_NOT_SUPPORTED;

    new VA:probe_va = io_space_map(bar_addr, XHCI_PROBE_MAP_SIZE);
    if (probe_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    new caplength = 0;
    new hcsparams1 = 0;
    new rtsoff_raw = 0;

    status = virtual_read_byte(probe_va + XHCI_CAPLENGTH, caplength);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(probe_va + XHCI_HCSPARAMS1, hcsparams1);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(probe_va + XHCI_RTSOFF, rtsoff_raw);

    io_space_unmap(probe_va, XHCI_PROBE_MAP_SIZE);
    if (!NT_SUCCESS(status))
        return status;

    if (caplength < 0x20 || caplength > 0xFF)
        return STATUS_DEVICE_DATA_ERROR;

    new max_intrs = (hcsparams1 >> 8) & 0x7FF;
    new rtsoff = rtsoff_raw & XHCI_RTSOFF_MASK;
    if (max_intrs < 1 || max_intrs > 0x400 || rtsoff < 0x20)
        return STATUS_DEVICE_DATA_ERROR;

    new required_size = rtsoff + XHCI_INTR_BASE +
        (max_intrs * XHCI_INTR_STRIDE);
    new map_size = (required_size + XHCI_PAGE_SIZE - 1) &
        ~(XHCI_PAGE_SIZE - 1);
    if (map_size < XHCI_PROBE_MAP_SIZE)
        map_size = XHCI_PROBE_MAP_SIZE;
    if (map_size > XHCI_MAX_MAP_SIZE)
        return STATUS_NOT_SUPPORTED;

    new VA:mmio_va = io_space_map(bar_addr, map_size);
    if (mmio_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_mmio_va = mmio_va;
    g_mmio_size = map_size;
    g_mmio_addr = bar_addr;
    g_didvid = didvid;
    g_classrev = classrev;
    g_caplength = caplength;
    g_hcsparams1 = hcsparams1;
    g_rtsoff = rtsoff;
    g_max_intrs = max_intrs;
    return STATUS_SUCCESS;
}

NTSTATUS:validate_interrupter(index, &intr_va) {
    if (g_mmio_va == NULL)
        return STATUS_DEVICE_NOT_READY;
    if (index < 0 || index >= g_max_intrs)
        return STATUS_INVALID_PARAMETER;

    new intr_offset = g_rtsoff + XHCI_INTR_BASE +
        (index * XHCI_INTR_STRIDE);
    if (intr_offset < 0 || intr_offset + XHCI_ERSTSZ + 4 > g_mmio_size)
        return STATUS_ACCESS_DENIED;

    intr_va = _:g_mmio_va + intr_offset;
    return STATUS_SUCCESS;
}

/// Return validated xHCI controller identity and layout information.
///
/// @param in [0] PCI bus, [1] device, [2] function
/// @param out [0] DIDVID, [1] class/revision, [2] BAR0 base,
///             [3] CAPLENGTH, [4] HCSPARAMS1, [5] RTSOFF,
///             [6] MaxIntrs, [7] mapped byte count
DEFINE_IOCTL_SIZED(ioctl_controller_info, 3, 8) {
    new NTSTATUS:status = map_controller(in[0], in[1], in[2]);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = g_didvid;
    out[1] = g_classrev;
    out[2] = g_mmio_addr;
    out[3] = g_caplength;
    out[4] = g_hcsparams1;
    out[5] = g_rtsoff;
    out[6] = g_max_intrs;
    out[7] = g_mmio_size;

    unmap_controller();
    return STATUS_SUCCESS;
}

/// Read one interrupter register set without changing it.
///
/// @param in [0] PCI bus, [1] device, [2] function, [3] interrupter index
/// @param out [0] index, [1] IMAN, [2] IMOD, [3] ERSTSZ
DEFINE_IOCTL_SIZED(ioctl_get_interrupter, 4, 4) {
    new NTSTATUS:status = map_controller(in[0], in[1], in[2]);
    if (!NT_SUCCESS(status))
        return status;

    new intr_va_raw = 0;
    status = validate_interrupter(in[3], intr_va_raw);
    if (!NT_SUCCESS(status)) {
        unmap_controller();
        return status;
    }

    new VA:intr_va = VA:intr_va_raw;
    new iman = 0;
    new imod = 0;
    new erstsz = 0;

    status = virtual_read_dword(intr_va + XHCI_IMAN, iman);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(intr_va + XHCI_IMOD, imod);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(intr_va + XHCI_ERSTSZ, erstsz);

    if (NT_SUCCESS(status)) {
        out[0] = in[3];
        out[1] = iman;
        out[2] = imod;
        out[3] = erstsz;
    }

    unmap_controller();
    return status;
}

/// Set only the low 16-bit IMOD interval for one configured, enabled
/// interrupter. The live high 16-bit counter is never written.
///
/// @param in [0] PCI bus, [1] device, [2] function,
///           [3] interrupter index, [4] interval (0..65535)
/// @param out [0] old IMOD, [1] requested interval,
///             [2] readback IMOD, [3] ERSTSZ
DEFINE_IOCTL_SIZED(ioctl_set_imod, 5, 4) {
    new interval = in[4];
    if (interval < 0 || interval > XHCI_IMOD_INTERVAL_MASK)
        return STATUS_INVALID_PARAMETER;

    new NTSTATUS:status = map_controller(in[0], in[1], in[2]);
    if (!NT_SUCCESS(status))
        return status;

    new intr_va_raw = 0;
    status = validate_interrupter(in[3], intr_va_raw);
    if (!NT_SUCCESS(status)) {
        unmap_controller();
        return status;
    }

    new VA:intr_va = VA:intr_va_raw;
    new iman = 0;
    new old_imod = 0;
    new erstsz = 0;
    new readback = 0;

    status = virtual_read_dword(intr_va + XHCI_IMAN, iman);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(intr_va + XHCI_ERSTSZ, erstsz);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(intr_va + XHCI_IMOD, old_imod);
    if (!NT_SUCCESS(status)) {
        unmap_controller();
        return status;
    }

    // Never touch dormant or unconfigured interrupters.
    if ((erstsz & XHCI_ERSTSZ_MASK) == 0 || (iman & XHCI_IMAN_IE) == 0) {
        unmap_controller();
        return STATUS_DEVICE_NOT_READY;
    }

    // A 16-bit write updates only the interval. It does not copy a sampled
    // high-word counter back into the live IMOD register.
    status = virtual_write_word(intr_va + XHCI_IMOD, interval);
    if (NT_SUCCESS(status))
        status = virtual_read_dword(intr_va + XHCI_IMOD, readback);

    if (NT_SUCCESS(status) &&
        (readback & XHCI_IMOD_INTERVAL_MASK) != interval) {
        // Best-effort immediate rollback if verification does not match.
        virtual_write_word(
            intr_va + XHCI_IMOD, old_imod & XHCI_IMOD_INTERVAL_MASK);
        status = STATUS_DATA_ERROR;
    }

    out[0] = old_imod;
    out[1] = interval;
    out[2] = readback;
    out[3] = erstsz;

    unmap_controller();
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    debug_print(''XhciImodAmd: restricted xHCI IMOD module loaded\n'');
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    unmap_controller();
    return STATUS_SUCCESS;
}

