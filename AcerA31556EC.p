//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Celso Naemen <180766906+celsonaemen@users.noreply.github.com>
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

// Acer Aspire A315-56 (Sleepy_IL, BIOS V1.27) extended EC mailbox.
// The addresses and transaction order mirror the EC0.FANG ACPI method.

#define ACER_EC_MMIO_BASE               0xFE0B0800
#define ACER_EC_MMIO_SIZE               0x1000
#define ACER_LPC_DIDVID                  0x34828086

#define OFFSET_ERIB                      0x05D
#define OFFSET_ECT_FLAGS                 0x582
#define OFFSET_ERN1                      0x583
#define OFFSET_ECMD                      0x58B
#define OFFSET_EDT1                      0x58C
#define OFFSET_EDT2                      0x58D

#define FLAG_ECTB                        0x01
#define FLAG_ECTE                        0x02
#define COMMAND_EXTENDED_READ            0xB0
#define EC_REGISTER_ERBD                 0x5F

#define TRANSACTION_POLL_COUNT           100
#define TRANSACTION_POLL_DELAY_US        10000

new VA:g_mmio_va = NULL;

NTSTATUS:read_byte(offset, &value) {
    if (g_mmio_va == NULL || offset < 0 || offset >= ACER_EC_MMIO_SIZE)
        return STATUS_INVALID_PARAMETER;

    return virtual_read_byte(g_mmio_va + offset, value);
}

NTSTATUS:write_byte(offset, value) {
    if (g_mmio_va == NULL || offset < 0 || offset >= ACER_EC_MMIO_SIZE)
        return STATUS_INVALID_PARAMETER;

    return virtual_write_byte(g_mmio_va + offset, value);
}

NTSTATUS:clear_transaction_busy() {
    new flags = 0;
    new NTSTATUS:status = read_byte(OFFSET_ECT_FLAGS, flags);
    if (!NT_SUCCESS(status))
        return status;

    return write_byte(OFFSET_ECT_FLAGS, flags & ~FLAG_ECTB);
}

/// Execute the semantic equivalent of the Acer ACPI EC0.FANG(index) method.
///
/// This reads an extended EC register. The firmware protocol necessarily writes
/// the requested index and mailbox command before returning the register value.
///
/// @param in [0] = 16-bit extended EC index
/// @param in_size Must be 1
/// @param out [0] = byte returned by the firmware mailbox
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning Acquire the "\BaseNamedObjects\Access_EC" mutant before calling this.
DEFINE_IOCTL_SIZED(ioctl_acer_extended_ec_read, 1, 1) {
    new index = in[0];
    if (index < 0 || index > 0xFFFF)
        return STATUS_INVALID_PARAMETER;

    new flags = 0;
    new NTSTATUS:status = read_byte(OFFSET_ECT_FLAGS, flags);
    if (!NT_SUCCESS(status))
        return status;

    // Refuse to disturb a firmware transaction already in progress. The
    // user-mode Access_EC mutant serializes callers before this best-effort
    // firmware-side check.
    if ((flags & (FLAG_ECTB | FLAG_ECTE)) != 0)
        return STATUS_DEVICE_BUSY;

    status = virtual_write_word(g_mmio_va + OFFSET_ERIB, index);
    if (!NT_SUCCESS(status))
        return status;

    status = write_byte(OFFSET_ECT_FLAGS, flags | FLAG_ECTB);
    if (!NT_SUCCESS(status))
        return status;

    status = write_byte(OFFSET_ECMD, COMMAND_EXTENDED_READ);
    if (NT_SUCCESS(status))
        status = write_byte(OFFSET_EDT1, EC_REGISTER_ERBD);
    if (NT_SUCCESS(status))
        status = write_byte(OFFSET_EDT2, 0);

    if (NT_SUCCESS(status)) {
        status = read_byte(OFFSET_ECT_FLAGS, flags);
        if (NT_SUCCESS(status))
            status = write_byte(OFFSET_ECT_FLAGS, flags | FLAG_ECTE);
    }

    new bool:completed = false;
    if (NT_SUCCESS(status)) {
        for (new attempt = 0; attempt < TRANSACTION_POLL_COUNT; ++attempt) {
            status = read_byte(OFFSET_ECT_FLAGS, flags);
            if (!NT_SUCCESS(status))
                break;

            if ((flags & FLAG_ECTE) == 0) {
                completed = true;
                break;
            }

            status = microsleep(TRANSACTION_POLL_DELAY_US);
            if (!NT_SUCCESS(status))
                break;
        }
    }

    if (NT_SUCCESS(status) && completed)
        status = read_byte(OFFSET_ERN1, out[0]);
    else if (NT_SUCCESS(status))
        status = STATUS_IO_TIMEOUT;

    new NTSTATUS:cleanup_status = clear_transaction_busy();
    if (NT_SUCCESS(status) && !NT_SUCCESS(cleanup_status))
        status = cleanup_status;

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();
    if (cpu_fms_family(fms) != 0x06 || cpu_fms_model(fms) != 0x7E)
        return STATUS_NOT_SUPPORTED;

    new didvid = 0;
    new NTSTATUS:status = pci_config_read_dword(0, 0x1F, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;
    if (didvid != ACER_LPC_DIDVID)
        return STATUS_NOT_SUPPORTED;

    g_mmio_va = io_space_map(ACER_EC_MMIO_BASE, ACER_EC_MMIO_SIZE);
    if (g_mmio_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_mmio_va != NULL) {
        io_space_unmap(g_mmio_va, ACER_EC_MMIO_SIZE);
        g_mmio_va = NULL;
    }

    return STATUS_SUCCESS;
}
