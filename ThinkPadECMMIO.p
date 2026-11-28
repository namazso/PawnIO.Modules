//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Gabag
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

// Lenovo ThinkPad T14 Gen 2a AMD exposes the EC RAM window through physical
// MMIO instead of the classic 0x62/0x66 ACPI port protocol. The DSDT ECOR
// opregion maps the same memory. This module maps that fixed window and
// exposes byte/word access so fan RPM (0x84/0x85) and fan level (0x2F)
// can be read/written without a vulnerable physical-memory driver.
//
// References:
//  - thinkpad_acpi / TPFanControl2: classic ThinkPad EC register layout
//  - DSDT ECOR opregion on 20XLS0911R (T14 Gen 2a AMD)
#define EC_BASE    0x00000000FE00DA00
#define EC_SIZE    0x80

new VA:g_ec = NULL;

/// Compare two NUL-terminated codepoint strings
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
    return str2[i] != 0 ? 1 : 0;
}

/// Check whether a NUL-terminated codepoint string starts with a prefix
stock str_prefix(const str[], const prefix[]) {
    new i = 0;
    while (prefix[i] != 0) {
        if (str[i] != prefix[i])
            return 1;
        i++;
    }
    return 0;
}

NTSTATUS:ec_unmap() {
    if (g_ec != NULL) {
        io_space_unmap(g_ec, EC_SIZE);
        g_ec = NULL;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:ec_map() {
    if (g_ec != NULL)
        return STATUS_SUCCESS;

    g_ec = io_space_map(EC_BASE, EC_SIZE);
    if (g_ec == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    return STATUS_SUCCESS;
}

NTSTATUS:ec_read_byte(offset, &value) {
    if (g_ec == NULL)
        return STATUS_DEVICE_NOT_READY;
    if (offset < 0 || offset >= EC_SIZE)
        return STATUS_INVALID_PARAMETER;

    return virtual_read_byte(g_ec + offset, value);
}

NTSTATUS:ec_read_word(offset, &value) {
    if (g_ec == NULL)
        return STATUS_DEVICE_NOT_READY;
    if (offset < 0 || offset + 1 >= EC_SIZE)
        return STATUS_INVALID_PARAMETER;

    return virtual_read_word(g_ec + offset, value);
}

NTSTATUS:ec_write_byte(offset, value) {
    if (g_ec == NULL)
        return STATUS_DEVICE_NOT_READY;
    if (offset < 0 || offset >= EC_SIZE)
        return STATUS_INVALID_PARAMETER;

    return virtual_write_byte(g_ec + offset, value);
}

/// Map the ThinkPad EC MMIO window.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out Unused
/// @param out_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_map, 0, 0) {
    return ec_map();
}

/// Unmap the ThinkPad EC MMIO window.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out Unused
/// @param out_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_unmap, 0, 0) {
    return ec_unmap();
}

/// Report module identity and current mapping state.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = 1 if currently mapped, 0 otherwise
///           [1] = Physical EC base
///           [2] = Mapped size in bytes
///           [3] = Current virtual address (0 if not mapped)
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 4) {
    out[0] = g_ec != NULL;
    out[1] = EC_BASE;
    out[2] = EC_SIZE;
    out[3] = _:g_ec;
    return STATUS_SUCCESS;
}

/// Read a byte from the ThinkPad EC window.
///
/// @param in [0] = Offset from EC base (0..0x7F)
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_read, 1, 1) {
    new offset = in[0] & 0xFFFF;
    return ec_read_byte(offset, out[0]);
}

/// Read a little-endian word from the ThinkPad EC window.
///
/// @param in [0] = Offset from EC base (0..0x7E)
/// @param in_size Must be 1
/// @param out [0] = Word read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_read_u16, 1, 1) {
    new offset = in[0] & 0xFFFF;
    return ec_read_word(offset, out[0]);
}

/// Write a byte to the ThinkPad EC window.
///
/// @param in [0] = Offset from EC base (0..0x7F), [1] = Value
/// @param in_size Must be 2
/// @param out Unused
/// @param out_size Must be 0
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_write, 2, 0) {
    new offset = in[0] & 0xFFFF;
    new value = in[1];
    return ec_write_byte(offset, value);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // Only on AMD CPUs - this chassis family.
    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();
    new family = cpu_fms_family(fms);
    // Zen3 / Zen4 (T14 Gen 2a AMD and close relatives)
    if (family < 0x19 || family > 0x1A)
        return STATUS_NOT_SUPPORTED;

    // ThinkPads identify as manufacturer LENOVO with the model name in
    // SystemFamily or SystemVersion ("ThinkPad T14 Gen 2"), while
    // SystemProductName/SystemSKU hold the machine type (e.g. 20XLS0911R).
    new bios_path[] = ''\\Registry\\Machine\\HARDWARE\\DESCRIPTION\\System\\BIOS'';

    new manufacturer[64];
    new mfg_len = 0;
    if (reg_query_sz(bios_path, ''SystemManufacturer'', manufacturer, sizeof manufacturer, mfg_len) != STATUS_SUCCESS)
        return STATUS_NOT_SUPPORTED;
    if (str_eq(manufacturer, ''LENOVO'') != 0)
        return STATUS_NOT_SUPPORTED;

    new family_str[64];
    new family_len = 0;
    if (reg_query_sz(bios_path, ''SystemFamily'', family_str, sizeof family_str, family_len) != STATUS_SUCCESS)
        family_str[0] = 0;

    new version[64];
    new version_len = 0;
    if (reg_query_sz(bios_path, ''SystemVersion'', version, sizeof version, version_len) != STATUS_SUCCESS)
        version[0] = 0;

    if (str_prefix(family_str, ''ThinkPad'') != 0 && str_prefix(version, ''ThinkPad'') != 0)
        return STATUS_NOT_SUPPORTED;

    // Map the window up front; a failed map means the chassis does not
    // actually expose this EC region.
    g_ec = io_space_map(EC_BASE, EC_SIZE);
    if (g_ec == NULL)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    return ec_unmap();
}
