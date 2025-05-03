//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  Steve-Tech <me@stevetech.au>
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

/*
Following documentation from https://git.kernel.org/pub/scm/linux/kernel/git/tip/tip.git/commit/?id=b343579de7b250101e4ed6c354b4c1fa986973a7

Random reboot issues
====================
When a random reboot occurs, the high-level reason for the reboot is stored
in a register that will persist onto the next boot.

There are 6 classes of reasons for the reboot:
 * Software induced
 * Power state transition
 * Pin induced
 * Hardware induced
 * Remote reset
 * Internal CPU event

| Bit | Type     | Reason                                                     |
|-----|----------|------------------------------------------------------------|
| 0   | Pin      | thermal pin BP_THERMTRIP_L was tripped                     |
| 1   | Pin      | power button was pressed for 4 seconds                     |
| 2   | Pin      | shutdown pin was shorted                                   |
| 4   | Remote   | remote ASF power off command was received                  |
| 9   | Internal | internal CPU thermal limit was tripped                     |
| 16  | Pin      | system reset pin BP_SYS_RST_L was tripped                  |
| 17  | Software | software issued PCI reset                                  |
| 18  | Software | software wrote 0x4 to reset control register 0xCF9         |
| 19  | Software | software wrote 0x6 to reset control register 0xCF9         |
| 20  | Software | software wrote 0xE to reset control register 0xCF9         |
| 21  | Sleep    | ACPI power state transition occurred                       |
| 22  | Pin      | keyboard reset pin KB_RST_L was asserted                   |
| 23  | Internal | internal CPU shutdown event occurred                       |
| 24  | Hardware | system failed to boot before failed boot timer expired     |
| 25  | Hardware | hardware watchdog timer expired                            |
| 26  | Remote   | remote ASF reset command was received                      |
| 27  | Internal | an uncorrected error caused a data fabric sync flood event |
| 29  | Internal | FCH and MP1 failed warm reset handshake                    |
| 30  | Internal | a parity error occurred                                    |
| 31  | Internal | a software sync flood event occurred                       |
*/

#define FCH_PM_BASE				0xFED80300
#define FCH_PM_S5_RESET_STATUS	0xC0

/// Read the PMx000000C0 (FCH::PM::S5_RESET_STATUS) register.
///
/// @param out Value read
/// @return An NTSTATUS
NTSTATUS:amd_reset_status(&out) {
    new NTSTATUS:status;

    // Map 32bits of MMIO space
    new VA:va = io_space_map(FCH_PM_BASE + FCH_PM_S5_RESET_STATUS, 32/8);
    if (va == NULL) {
        debug_print(''Failed to map MMIO space\n'');
        return STATUS_NO_MEMORY;
    }

    // Read the reset status register
    new reset_status;
    status = virtual_read_dword(va, reset_status);
    if (!NT_SUCCESS(status))
        debug_print(''Failed to read reset status: %x\n'', _:status);
    else if (reset_status == 0xFFFFFFFF) {
        debug_print(''Failed to read reset status\n'');
        status = STATUS_UNSUCCESSFUL;
    }

    // Unmap MMIO space
    io_space_unmap(va, 32/8);
    if (!NT_SUCCESS(status)) {
        debug_print(''Failed to unmap MMIO space\n'');
        return status;
    }
    
    out = reset_status;

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    new vendor[4];
    cpuid(0, 0, vendor);
    if (!is_amd(vendor))
        return STATUS_NOT_SUPPORTED;

    new procinfo[4];
    cpuid(1, 0, procinfo);

    new family = ((procinfo[0] & 0x0FF00000) >> 20) + ((procinfo[0] & 0x0F00) >> 8);
    new model = ((procinfo[0] & 0x0F0000) >> 12) + ((procinfo[0] & 0xF0) >> 4);

    debug_print(''AMDReset: family: %x model: %x\n'', family, model);

    // Check if the CPU is 17h (Zen) or newer
    // This does seem to be supported on 16h CPUs, but the bit mapping is different.
    if (family < 0x17)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

/// Read the PMx000000C0 (FCH::PM::S5_RESET_STATUS) register.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = The status register value
/// @param out_size Must be 1
/// @return An NTSTATUS
forward NTSTATUS:ioctl_amd_reset_status(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_amd_reset_status(in[], in_size, out[], out_size) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    return amd_reset_status(out[0]);
}
