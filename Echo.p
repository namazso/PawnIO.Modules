//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2023  namazso <admin@namazso.eu>
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

#include <pawnio.inc>

forward ioctl_not(in[], in_size, out[], out_size);
public ioctl_not(in[], in_size, out[], out_size) {
    if (in_size != 1 || out_size != 1)
        return STATUS_INVALID_PARAMETER;

    new v = in[0];

    debug_print(''Inverting %x'', v);

    out[0] = ~v;

    return STATUS_SUCCESS;
}

main() {
    // I don't know why this is needed, probably some compiler or interpreter bug
    if (get_arch() == 0)
        return STATUS_UNSUCCESSFUL;

    debug_print(''Echo module loaded!'');
    return STATUS_SUCCESS;
}

forward unload();
public unload() {
    debug_print(''Echo module unloaded!'');
    return STATUS_SUCCESS;
}
