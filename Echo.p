//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  namazso <admin@namazso.eu>
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

/// Bitwise not.
///
/// @param in [0] = Value
/// @param in_size Must be 1
/// @param out Bitwise not of Value
/// @param out_size Must be 1
/// @return An NTSTATUS
forward NTSTATUS:ioctl_not(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_not(in[], in_size, out[], out_size) {
    if (in_size != 1 || out_size != 1)
        return STATUS_INVALID_PARAMETER;

    new v = in[0];

    debug_print(''Inverting %x'', v);

    out[0] = ~v;

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    // Only calling one native in a module triggers some interpreter or compiler bug...
    new CPUArch:arch = get_arch();

    debug_print(''Echo module loaded! Arch: %d'', _:arch);
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    debug_print(''Echo module unloaded!'');
    return STATUS_SUCCESS;
}
