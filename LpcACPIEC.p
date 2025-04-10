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

is_port_allowed(port) {
    return port == 0x62 || port == 0x66;
}

forward ioctl_pio_read(in[], in_size, out[], out_size);
public ioctl_pio_read(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new port = in[0] & 0xFFFF;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

forward ioctl_pio_write(in[], in_size, out[], out_size);
public ioctl_pio_write(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new port = in[0] & 0xFFFF;
    new value = in[1];

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    io_out_byte(port, value);
    return STATUS_SUCCESS;
}

main() {
    return STATUS_SUCCESS;
}
