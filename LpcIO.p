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
new g_chip_ec_base[] = [ 0, 0 ];

read_byte(register_port, value_port, reg) {
    io_out_byte(register_port, reg);
    return io_in_byte(value_port);
}

write_byte(register_port, value_port, reg, val) {
    io_out_byte(register_port, reg);
    io_out_byte(value_port, val);
}

read_word(register_port, value_port, reg) {
    return (read_byte(register_port, value_port, reg) << 8) | read_byte(register_port, value_port, reg + 1);
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

const DEVICE_SELECT_REGISTER = 0x07;
const IT87XX_SMFI_LDN = 0x0F;
const IT87_SMFI_HLPC_RAM_BAR = 0xF5;
const IT87_SMFI_HLPC_RAM_BAR_HIGH = 0xFC;
const IT87_LD_ACTIVE_REGISTER = 0x30;

it87_find_ec(register_port, value_port, type, &base) {
    base = 0;

    // Check if the SMFI logical device is enabled

    write_byte(register_port, value_port, DEVICE_SELECT_REGISTER, IT87XX_SMFI_LDN);
    new enabled = read_byte(register_port, value_port, IT87_LD_ACTIVE_REGISTER);
    microsleep(1000);
    new enabled_v = read_byte(register_port, value_port, IT87_LD_ACTIVE_REGISTER);

    if (!enabled || enabled != enabled_v)
        return false;

    new word = read_word(register_port, value_port, IT87_SMFI_HLPC_RAM_BAR);
    new high = read_byte(register_port, value_port, IT87_SMFI_HLPC_RAM_BAR_HIGH);

    microsleep(1000);

    new word_v = read_word(register_port, value_port, IT87_SMFI_HLPC_RAM_BAR);
    new high_v = read_byte(register_port, value_port, IT87_SMFI_HLPC_RAM_BAR_HIGH);

    if (word_v != word_v)
        return false;

    if ((type & 0xFFFF) == 0x8695) {
        // IT87952E

        if (high != high_v)
            return false;

        base = 0xFC000000 | (word & 0xF000) | ((word & 0xFF) << 16) | ((high & 0xF) << 24);
    } else {
        base = 0xFF000000 | (word & 0xF000) | ((word & 0xFF) << 16);
    }

    return true;
}

detect_chip(register_port, value_port, &type, search_for_ec, &ec_base) {
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

        if (search_for_ec)
            it87_find_ec(register_port, value_port, type, ec_base);

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
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    if (out_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    for (new i = 0; i < 2; ++i) {
        detect_chip(CHIP_REGISTER_PORT[i], CHIP_VALUE_PORT[i], g_chip_type[i], in[i] != 0, g_chip_ec_base[i]);
        out[i] = g_chip_type[i];
    }

    return STATUS_SUCCESS;
}

forward ioctl_read(in[], in_size, out[], out_size);
public ioctl_read(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0];
    new reg = in[1] & 0xFF;

    if (idx < 0 || idx > 1)
        return STATUS_INVALID_PARAMETER;

    if ((g_chip_type[idx] >> 32) == Vendor_Unknown)
        return STATUS_DEVICE_NOT_READY;

    out[0] = read_byte(CHIP_REGISTER_PORT[idx], CHIP_VALUE_PORT[idx], reg);
    return STATUS_SUCCESS;
}

forward ioctl_write(in[], in_size, out[], out_size);
public ioctl_write(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0];
    new reg = in[1] & 0xFF;
    new val = in[2] & 0xFF;

    if (idx < 0 || idx > 1)
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

    new idx = in[0];

    if (idx < 0 || idx > 1)
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

    if (idx < 0 || idx > 1)
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

forward ioctl_ec_read(in[], in_size, out[], out_size);
public ioctl_ec_read(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0];
    new offset = in[1];
    new size = in[2];

    if (idx < 0 || idx > 1 || offset < 0 || offset > PAGE_SIZE || size < 0 || size > 8 || offset + size > PAGE_SIZE)
        return STATUS_INVALID_PARAMETER;

    if (g_chip_ec_base[idx] == 0)
        return STATUS_DEVICE_NOT_READY;

    new va = io_space_map(g_chip_ec_base[idx], PAGE_SIZE);
    if (!va)
        return STATUS_NO_MEMORY;

    new status = STATUS_SUCCESS;
    new value;

    switch (size) {
        case 1:
            status = virtual_read_byte(va + offset, value);
        case 2:
            status = virtual_read_word(va + offset, value);
        case 4:
            status = virtual_read_dword(va + offset, value);
        case 8:
            status = virtual_read_qword(va + offset, value);
        default:
            status = STATUS_INVALID_PARAMETER;
    }

    io_space_unmap(va, size);

    out[0] = value;

    return status;
}

forward ioctl_ec_write(in[], in_size, out[], out_size);
public ioctl_ec_write(in[], in_size, out[], out_size) {
    if (in_size < 4)
        return STATUS_BUFFER_TOO_SMALL;

    new idx = in[0];
    new offset = in[1];
    new size = in[2];
    new value = in[3];

    if (idx < 0 || idx > 1 || offset < 0 || offset > PAGE_SIZE || size < 0 || size > 8 || offset + size > PAGE_SIZE)
        return STATUS_INVALID_PARAMETER;

    if (g_chip_ec_base[idx] == 0)
        return STATUS_DEVICE_NOT_READY;

    new va = io_space_map(g_chip_ec_base[idx], PAGE_SIZE);
    if (!va)
        return STATUS_NO_MEMORY;

    new status = STATUS_SUCCESS;

    switch (size) {
        case 1:
            status = virtual_write_byte(va + offset, value);
        case 2:
            status = virtual_write_word(va + offset, value);
        case 4:
            status = virtual_write_dword(va + offset, value);
        case 8:
            status = virtual_write_qword(va + offset, value);
        default:
            status = STATUS_INVALID_PARAMETER;
    }

    io_space_unmap(va, size);

    return status;
}

main() {
    return STATUS_SUCCESS;
}

