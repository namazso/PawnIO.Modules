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

forward ioctl_get_manufacturer_id(in[], in_size, out[], out_size);
public ioctl_get_manufacturer_id(in[], in_size, out[], out_size) {
    if (out_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new out_buf[4];
    cpuid(0, 0, out_buf);
    out[0] = (out_buf[1] & 0xFFFFFFFF) | ((out_buf[3] & 0xFFFFFFFF) << 32);
    out[1] = out_buf[2] & 0xFFFFFFFF;
    return STATUS_SUCCESS;
}

main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_IMPLEMENTED;
    return STATUS_SUCCESS;
}

