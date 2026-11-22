//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  uncoreworks
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

// Intel Core Ultra 200S CFG & MEM Registers, document 835538, MCHBAR_0_0_0_PCI.
// The BAR layout is documented. The two queries were observed on Arrow Lake-S:
// D2D reply bits 6:0 and NGU reply bits 15:8 carry the respective multipliers.

#define MCHBAR_BASE_REG_LOW     0x48
#define MCHBAR_BASE_REG_HIGH    0x4C
#define MAILBOX_PAGE_OFFSET     0x5000
#define MAILBOX_PAGE_SIZE       0x1000
#define MAILBOX_DATA            (0x5DA0 - MAILBOX_PAGE_OFFSET)
#define MAILBOX_INTERFACE       (0x5DA4 - MAILBOX_PAGE_OFFSET)
#define BCLK_FREQ               (0x5F60 - MAILBOX_PAGE_OFFSET)
#define D2D_QUERY               0x1237
#define NGU_QUERY               0x22
#define MAILBOX_RUN_BUSY        0x80000000

new VA:g_mailbox_va = NULL;
new g_mchbar_addr;

NTSTATUS:read_mchbar(&base) {
    // On Arrow Lake-S the BAR uses bits 41:17. Bit 0 enables decoding.
    new lo, hi;
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, MCHBAR_BASE_REG_LOW, lo);
    if (!NT_SUCCESS(status))
        return status;
    status = pci_config_read_dword(0, 0, 0, MCHBAR_BASE_REG_HIGH, hi);
    if (!NT_SUCCESS(status))
        return status;
    if ((lo & 0x1ffff) != 1 || (hi & 0xffffffff) > 0x3ff)
        return STATUS_DEVICE_NOT_READY;
    base = ((hi & 0x3ff) << 32) | (lo & 0xfffe0000);
    if (!base)
        return STATUS_DEVICE_NOT_READY;
    return STATUS_SUCCESS;
}

/// Read an Arrow Lake-S fabric clock ratio reply and its reference clock.
///
/// @param in [0] = domain (0 for D2D, 1 for NGU)
/// @param in_size Must be 1
/// @param out [0] = raw domain reply
///            [1] = reference clock in kHz
/// @param out_size Must be 2
/// @return STATUS_SUCCESS, STATUS_DEVICE_BUSY if the mailbox is in use,
///         STATUS_IO_TIMEOUT on timeout, or the underlying access error.
/// @note Reload the module on STATUS_DEVICE_NOT_READY after a BAR change.
/// @warning The caller must hold Global\Access_Intel_OS_Mailbox during this call.
DEFINE_IOCTL_SIZED(ioctl_fabric_clock, 1, 2) {
    if (in[0] != 0 && in[0] != 1)
        return STATUS_INVALID_PARAMETER;
    new base;
    new NTSTATUS:status = read_mchbar(base);
    if (!NT_SUCCESS(status))
        return status;
    if (g_mailbox_va == NULL || base != g_mchbar_addr)
        return STATUS_DEVICE_NOT_READY;
    new reference, reply_status, data;
    status = virtual_read_dword(g_mailbox_va + BCLK_FREQ, reference);
    if (!NT_SUCCESS(status))
        return status;
    if (reference < 50000 || reference > 250000)
        return STATUS_DEVICE_DATA_ERROR;
    status = virtual_read_dword(g_mailbox_va + MAILBOX_INTERFACE, reply_status);
    if (!NT_SUCCESS(status))
        return status;
    if (reply_status & MAILBOX_RUN_BUSY)
        return STATUS_DEVICE_BUSY;
    status = virtual_write_dword(g_mailbox_va + MAILBOX_DATA, 0);
    if (!NT_SUCCESS(status))
        return status;
    status = virtual_read_dword(g_mailbox_va + MAILBOX_DATA, data);
    if (!NT_SUCCESS(status))
        return status;
    if (data != 0)
        return STATUS_DEVICE_DATA_ERROR;
    // Both commands are fixed no-argument telemetry queries.
    status = virtual_write_dword(g_mailbox_va + MAILBOX_INTERFACE, MAILBOX_RUN_BUSY | (in[0] == 1 ? NGU_QUERY : D2D_QUERY));
    if (!NT_SUCCESS(status))
        return status;
    for (new poll = 0; poll < 32; ++poll) {
        status = virtual_read_dword(g_mailbox_va + MAILBOX_INTERFACE, reply_status);
        if (!NT_SUCCESS(status))
            return status;
        if (!(reply_status & MAILBOX_RUN_BUSY)) {
            if (reply_status & 0xff)
                return STATUS_IO_DEVICE_ERROR;
            status = virtual_read_dword(g_mailbox_va + MAILBOX_DATA, data);
            if (!NT_SUCCESS(status))
                return status;
            out[0] = data;
            out[1] = reference;
            return STATUS_SUCCESS;
        }
        status = microsleep(50);
        if (!NT_SUCCESS(status))
            return status;
    }
    return STATUS_IO_TIMEOUT;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;
    new fms = get_cpu_fms();
    if (cpu_fms_family(fms) != 6 || cpu_fms_model(fms) != 0xc6)
        return STATUS_NOT_SUPPORTED;
    new vendor, class_code;
    new NTSTATUS:status = pci_config_read_word(0, 0, 0, 0, vendor);
    if (!NT_SUCCESS(status))
        return status;
    status = pci_config_read_word(0, 0, 0, 0x0a, class_code);
    if (!NT_SUCCESS(status))
        return status;
    if (vendor != 0x8086 || class_code != 0x0600)
        return STATUS_NOT_SUPPORTED;
    status = read_mchbar(g_mchbar_addr);
    if (!NT_SUCCESS(status))
        return status;
    g_mailbox_va = io_space_map(g_mchbar_addr + MAILBOX_PAGE_OFFSET, MAILBOX_PAGE_SIZE);
    return g_mailbox_va == NULL ? STATUS_INSUFFICIENT_RESOURCES : STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_mailbox_va != NULL)
        io_space_unmap(g_mailbox_va, MAILBOX_PAGE_SIZE);
    g_mailbox_va = NULL;
    g_mchbar_addr = 0;
    return STATUS_SUCCESS;
}
