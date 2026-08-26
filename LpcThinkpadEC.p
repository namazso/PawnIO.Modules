//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  mews-se
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

// ThinkPads decode a second interface to the embedded controller as a 32
// byte window at 0x1600: the ACPI EC command set on 0x1600 (data) and
// 0x1604 (status/command), plus a 16 byte row transfer region at
// 0x1610..0x161F. Present since roughly the Sandy Bridge generation,
// alongside the standard 0x62/0x66 interface covered by LpcACPIEC.
// References: TPFanCtrl2 fancontrol/portio.cpp, tp_smapi thinkpad_ec.c.

is_port_allowed(port) {
    return port >= 0x1600 && port <= 0x161F;
}

/// Read byte from the ThinkPad EC window.
///
/// @param in [0] = Port
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_pio_read, 1, 1) {
    new port = in[0] & 0xFFFF;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

/// Write byte to the ThinkPad EC window.
///
/// @param in [0] = Port, [1] = Value
/// @param in_size Must be 2
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_pio_write, 2, 0) {
    new port = in[0] & 0xFFFF;
    new value = in[1];

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    io_out_byte(port, value);
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // Best effort hardware gate without DMI access: an unimplemented LPC
    // port reads as 0xFF, and 0xFF is never a valid EC status (it would
    // mean every bit set including both reserved ones). A machine with
    // this interface answers with a live status byte here.
    if (io_in_byte(0x1604) == 0xFF)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
