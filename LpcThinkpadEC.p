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
#include <registry.inc>

// ThinkPads decode a second interface to the embedded controller as a 32
// byte window at 0x1600: the ACPI EC command set on 0x1600 (data) and
// 0x1604 (status/command), plus a 16 byte row transfer region at
// 0x1610..0x161F. Present since roughly the Sandy Bridge generation,
// alongside the standard 0x62/0x66 interface covered by LpcACPIEC.
// References: TPFanCtrl2 fancontrol/portio.cpp, tp_smapi thinkpad_ec.c.

is_port_allowed(port) {
    return port >= 0x1600 && port <= 0x161F;
}

/// Compare two NUL-terminated codepoint strings
///
/// @param str1 Unpacked string (one codepoint per cell)
/// @param str2 Unpacked string (one codepoint per cell)
/// @return 0 if equal, nonzero otherwise
stock str_eq(const str1[], const str2[]) {
    new i = 0;
    while (str1[i] != 0) {
        new c = str2[i];
        if (c == 0)
            return 1; // str2 ended before str1
        if (str1[i] != c)
            return 1; // mismatch
        i++;
    }
    // str1 ended; check that str2 also ended
    return str2[i] != 0 ? 1 : 0;
}

/// Check whether a NUL-terminated codepoint string starts with a prefix
///
/// @param str Unpacked string (one codepoint per cell)
/// @param prefix Unpacked string (one codepoint per cell)
/// @return 0 if str starts with prefix, nonzero otherwise
stock str_prefix(const str[], const prefix[]) {
    new i = 0;
    while (prefix[i] != 0) {
        if (str[i] != prefix[i])
            return 1;
        i++;
    }
    return 0;
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

    // ThinkPads identify as manufacturer LENOVO with the model name in
    // SystemFamily or SystemVersion ("ThinkPad X13 Gen 3"), while
    // SystemProductName holds the machine type (e.g. 21BN00B7MX)
    new bios_path[] = ''\\Registry\\Machine\\HARDWARE\\DESCRIPTION\\System\\BIOS'';

    new manufacturer[64];
    new mfg_len = 0;
    if (reg_query_sz(bios_path, ''SystemManufacturer'', manufacturer, sizeof manufacturer, mfg_len) != STATUS_SUCCESS)
        return STATUS_NOT_SUPPORTED;

    if (str_eq(manufacturer, ''LENOVO'') != 0)
        return STATUS_NOT_SUPPORTED;

    new family[64];
    new family_len = 0;
    if (reg_query_sz(bios_path, ''SystemFamily'', family, sizeof family, family_len) != STATUS_SUCCESS)
        family[0] = 0;

    new version[64];
    new version_len = 0;
    if (reg_query_sz(bios_path, ''SystemVersion'', version, sizeof version, version_len) != STATUS_SUCCESS)
        version[0] = 0;

    if (str_prefix(family, ''ThinkPad'') != 0 && str_prefix(version, ''ThinkPad'') != 0)
        return STATUS_NOT_SUPPORTED;

    // An unimplemented LPC port reads as 0xFF, and 0xFF is never a valid
    // EC status (it would mean every bit set including both reserved
    // ones). A machine with this interface answers with a live byte here.
    if (io_in_byte(0x1604) == 0xFF)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
