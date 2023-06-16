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

const _: {
    Vendor_Unknown = 0,
    Vendor_Winbond,
    Vendor_IT87,
    Vendor_Smsc
}

const CHIP_ID_REGISTER = 0x20;
const CHIP_REVISION_REGISTER = 0x21;

const BASE_ADDRESS_REGISTER = 0x60;

new const CHIP_REGISTER_PORT[] = [ 0x2e, 0x4e ];
new const CHIP_VALUE_PORT[] = [ 0x2f, 0x4f ];
new g_chip_type[] = [ 0, 0 ];

read_byte(register_port, value_port, reg) {
    io_out_byte(register_port, reg);
    return io_in_byte(value_port);
}

write_byte(register_port, value_port, reg, val) {
    io_out_byte(register_port, reg);
    io_out_byte(value_port, val);
}

winbond_enter(register_port) {
    io_out_byte(register_port, 0x87);
    io_out_byte(register_port, 0x87);
}

winbond_exit(register_port) {
    io_out_byte(register_port, 0xaa);
}

it87_enter(register_port) {
    io_out_byte(register_port, 0x87);
    io_out_byte(register_port, 0x01);
    io_out_byte(register_port, 0x55);
    io_out_byte(register_port, register_port == 0x4e ? 0xaa : 0x55);
}

it87_exit(register_port, value_port) {
    if (register_port == 0x4e)
        return;
    io_out_byte(register_port, 0x02);
    io_out_byte(value_port, 0x02);
}

smsc_enter(register_port) {
    io_out_byte(register_port, 0x55);
}

smsc_exit(register_port) {
    io_out_byte(register_port, 0xaa);
}

detect_chip(register_port, value_port, &type) {
    new chip_id, chip_revision;
    type = 0;

    // ========= Winbond / Nuvoton / Fintek

    winbond_enter(register_port);

    chip_id = read_byte(register_port, value_port, CHIP_ID_REGISTER);
    chip_revision = read_byte(register_port, value_port, CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's Winbond!
        type = (Vendor_Winbond << 32) | (chip_id << 8) | (chip_revision);
        winbond_exit(register_port);
        return;
    }

    // ========= IT87

    it87_enter(register_port);

    chip_id = read_byte(register_port, value_port, CHIP_ID_REGISTER);
    chip_revision = read_byte(register_port, value_port, CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's IT87!
        type = (Vendor_IT87 << 32) | (chip_id << 8) | (chip_revision);
        it87_exit(register_port, value_port);
        return;
    }

    // ========= Smsc

    smsc_enter(register_port);

    chip_id = read_byte(register_port, value_port, CHIP_ID_REGISTER);
    chip_revision = read_byte(register_port, value_port, CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's Smsc!
        type = (Vendor_Smsc << 32) | (chip_id << 8) | (chip_revision);
        smsc_exit(register_port);
        return;
    }

    // unknown, just leave it as 0
}

forward ioctl_detect(in[], in_size, out[], out_size);
public ioctl_detect(in[], in_size, out[], out_size) {
    if (out_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    for (new i = 0; i < 2; ++i) {
        detect_chip(CHIP_REGISTER_PORT[i], CHIP_VALUE_PORT[i], g_chip_type[i]);
        out[i] = g_chip_type[i];
    }

    return STATUS_SUCCESS;
}

forward ioctl_read(in[], in_size, out[], out_size);
public ioctl_read(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0] & 0xFF;
    new reg = (in[0] >> 8) & 0xFF;

    if (idx > 1)
        return STATUS_INVALID_PARAMETER;

    if ((g_chip_type[idx] >> 32) == Vendor_Unknown)
        return STATUS_DEVICE_NOT_READY;

    out[0] = read_byte(CHIP_REGISTER_PORT[idx], CHIP_VALUE_PORT[idx], reg);
    return STATUS_SUCCESS;
}

forward ioctl_write(in[], in_size, out[], out_size);
public ioctl_write(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0] & 0xFF;
    new reg = (in[0] >> 8) & 0xFF;
    new val = (in[0] >> 16) & 0xFF;

    if (idx > 1)
        return STATUS_INVALID_PARAMETER;

    if ((g_chip_type[idx] >> 32) == Vendor_Unknown)
        return STATUS_DEVICE_NOT_READY;

    write_byte(CHIP_REGISTER_PORT[idx], CHIP_VALUE_PORT[idx], reg, val);
    return STATUS_SUCCESS;
}

forward ioctl_enter(in[], in_size, out[], out_size);
public ioctl_enter(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0] & 0xFF;

    if (idx > 1)
        return STATUS_INVALID_PARAMETER;

    if ((g_chip_type[idx] >> 32) == Vendor_Unknown)
        return STATUS_DEVICE_NOT_READY;

    switch (g_chip_type[idx] >> 32) {
        case Vendor_Winbond:
            winbond_enter(CHIP_REGISTER_PORT[idx]);
        case Vendor_IT87:
            it87_enter(CHIP_REGISTER_PORT[idx]);
        case Vendor_Smsc:
            smsc_enter(CHIP_REGISTER_PORT[idx]);
        default:
            return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

forward ioctl_exit(in[], in_size, out[], out_size);
public ioctl_exit(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0] & 0xFF;

    if (idx > 1)
        return STATUS_INVALID_PARAMETER;

    if ((g_chip_type[idx] >> 32) == Vendor_Unknown)
        return STATUS_DEVICE_NOT_READY;

    switch (g_chip_type[idx] >> 32) {
        case Vendor_Winbond:
            winbond_exit(CHIP_REGISTER_PORT[idx]);
        case Vendor_IT87:
            it87_exit(CHIP_REGISTER_PORT[idx], CHIP_VALUE_PORT[idx]);
        case Vendor_Smsc:
            smsc_exit(CHIP_REGISTER_PORT[idx]);
        default:
            return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

main() {
    return STATUS_SUCCESS;
}

