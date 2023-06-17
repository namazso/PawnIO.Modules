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
const BASE_ADDRESS_REGISTER_2 = 0x62;

const DEVICE_SELECT_REGISTER = 0x07;
const IT87XX_SMFI_LDN = 0x0F;
const IT87_SMFI_HLPC_RAM_BAR = 0xF5;
const IT87_SMFI_HLPC_RAM_BAR_HIGH = 0xFC;
const IT87_LD_ACTIVE_REGISTER = 0x30;

new g_register_port;
new g_value_port;
new g_type;
new g_mmio_base;
new g_bars[128];
new g_bars_count = 0;

reset() {
    g_register_port = 0;
    g_value_port = 0;
    g_type = 0;
    g_mmio_base = 0;
    g_bars_count = 0;
}

is_ready() {
    return (g_type >> 32) != Vendor_Unknown;
}

read_byte(reg) {
    io_out_byte(g_register_port, reg);
    return io_in_byte(g_value_port);
}

write_byte(reg, val) {
    io_out_byte(g_register_port, reg);
    io_out_byte(g_value_port, val);
}

select(val) {
    write_byte(DEVICE_SELECT_REGISTER, val);
}

read_word(reg) {
    return (read_byte(reg) << 8) | read_byte(reg + 1);
}

winbond_enter() {
    io_out_byte(g_register_port, 0x87);
    io_out_byte(g_register_port, 0x87);
}

winbond_exit() {
    io_out_byte(g_register_port, 0xaa);
}

it87_enter() {
    io_out_byte(g_register_port, 0x87);
    io_out_byte(g_register_port, 0x01);
    io_out_byte(g_register_port, 0x55);
    io_out_byte(g_register_port, g_register_port == 0x4e ? 0xaa : 0x55);
}

it87_exit() {
    // According to Linux:
    //   Disabling configuration mode on some chips can result in system
    //   hang-ups and access failures to the Super-IO chip at the
    //   second SIO address. Never exit configuration mode on these
    //   chips to avoid the problem.

    if (g_register_port == 0x4e)
        return;
    write_byte(0x02, 0x02);
}

smsc_enter() {
    io_out_byte(g_register_port, 0x55);
}

smsc_exit() {
    io_out_byte(g_register_port, 0xaa);
}

it87_find_mmio() {
    g_mmio_base = 0;

    if (g_register_port != 0x4e)
        return false;

    // Check if the SMFI logical device is enabled

    select(IT87XX_SMFI_LDN);
    new enabled = read_byte(IT87_LD_ACTIVE_REGISTER);
    microsleep(1000);
    new enabled_v = read_byte(IT87_LD_ACTIVE_REGISTER);

    if (enabled != enabled_v || !enabled)
        return false;

    new word = read_word(IT87_SMFI_HLPC_RAM_BAR);
    new high = read_byte(IT87_SMFI_HLPC_RAM_BAR_HIGH);

    microsleep(1000);

    new word_v = read_word(IT87_SMFI_HLPC_RAM_BAR);
    new high_v = read_byte(IT87_SMFI_HLPC_RAM_BAR_HIGH);

    if (word_v != word_v)
        return false;

    if ((g_type & 0xFFFF) == 0x8695) {
        // IT87952E

        if (high != high_v)
            return false;

        g_mmio_base = 0xFC000000 | (word & 0xF000) | ((word & 0xFF) << 16) | ((high & 0xF) << 24);
    } else {
        g_mmio_base = 0xFF000000 | (word & 0xF000) | ((word & 0xFF) << 16);
    }

    return true;
}

check_bar(val, val_v) {
    if (val == val_v && val != 0 && val != 0xFFFF) {
        // in fixed range, probably some garbage
        if (val < 0x100)
            return;

        // some Fintek chips have address register offset 0x05 added already
        if ((val & 0x07) == 0x05)
            val &= 0xFFF8;

        // duplicate
        if (g_bars_count > 0 && g_bars[g_bars_count - 1] == val)
            return;

        g_bars[g_bars_count] = val;
        g_bars_count++;
        debug_print(''LpcIO: Added %X as BAR\n'', val);
    }
}

find_bars() {
    debug_print(''LpcIO: Finding BARs for %x\n'', g_register_port);
    new vals[256][2];
    for (new i = 0; i < 0x100; i++) {
        select(i);
        vals[i][0] = read_word(BASE_ADDRESS_REGISTER);
        vals[i][1] = read_word(BASE_ADDRESS_REGISTER_2);
    }
    microsleep(1000);
    for (new i = 0; i < 0x100; i++) {
        select(i);
        new vals_v[2];
        vals_v[0] = read_word(BASE_ADDRESS_REGISTER);
        vals_v[1] = read_word(BASE_ADDRESS_REGISTER_2);

        check_bar(vals[i][0], vals_v[0]);
        check_bar(vals[i][1], vals_v[1]);
    }
}

detect_chip(search_for_mmio) {
    new chip_id, chip_revision;

    // ========= Winbond / Nuvoton / Fintek

    winbond_enter();

    chip_id = read_byte(CHIP_ID_REGISTER);
    chip_revision = read_byte(CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's Winbond!
        g_type = (Vendor_Winbond << 32) | (chip_id << 8) | (chip_revision);
        find_bars();
        winbond_exit();
        return;
    }

    // ========= IT87

    it87_enter();

    chip_id = read_byte(CHIP_ID_REGISTER);
    chip_revision = read_byte(CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's IT87!
        g_type = (Vendor_IT87 << 32) | (chip_id << 8) | (chip_revision);
        find_bars();

        if (search_for_mmio)
            it87_find_mmio();

        it87_exit();
        return;
    }

    // ========= Smsc

    smsc_enter();

    chip_id = read_byte(CHIP_ID_REGISTER);
    chip_revision = read_byte(CHIP_REVISION_REGISTER);

    if (chip_id != 0x00 && chip_id != 0xff) {
        // it's Smsc!
        g_type = (Vendor_Smsc << 32) | (chip_id << 8) | (chip_revision);
        find_bars();
        smsc_exit();
        return;
    }

    // unknown, just leave it as 0
}

forward ioctl_detect(in[], in_size, out[], out_size);
public ioctl_detect(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 2)
        return STATUS_BUFFER_TOO_SMALL;

    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    reset();

    new slot = in[IOCTL_IN_OFFSET + 0];
    new should_scan_for_mmio = in[IOCTL_IN_OFFSET + 1];

    debug_print(''LpcIO: Scanning slot %d. Scanning for mmio: %d\n'', slot, should_scan_for_mmio);

    if (slot == 0) {
        g_register_port = 0x2e;
        g_value_port = 0x2f;
    } else if (slot == 1) {
        g_register_port = 0x4e;
        g_value_port = 0x4f;
    } else {
        return STATUS_INVALID_PARAMETER;
    }

    detect_chip(should_scan_for_mmio != 0);

    out[0] = g_type;

    return STATUS_SUCCESS;
}

forward ioctl_read(in[], in_size, out[], out_size);
public ioctl_read(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new reg = in[IOCTL_IN_OFFSET + 0] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    out[0] = read_byte(reg);
    return STATUS_SUCCESS;
}

forward ioctl_write(in[], in_size, out[], out_size);
public ioctl_write(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 2)
        return STATUS_BUFFER_TOO_SMALL;

    new reg = in[IOCTL_IN_OFFSET + 0] & 0xFF;
    new val = in[IOCTL_IN_OFFSET + 1] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    write_byte(reg, val);
    return STATUS_SUCCESS;
}

forward ioctl_enter(in[], in_size, out[], out_size);
public ioctl_enter(in[], in_size, out[], out_size) {
    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    switch (g_type >> 32) {
        case Vendor_Winbond:
            winbond_enter();
        case Vendor_IT87:
            it87_enter();
        case Vendor_Smsc:
            smsc_enter();
        default:
            return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

forward ioctl_exit(in[], in_size, out[], out_size);
public ioctl_exit(in[], in_size, out[], out_size) {
    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    switch (g_type >> 32) {
        case Vendor_Winbond:
            winbond_exit();
        case Vendor_IT87:
            it87_exit();
        case Vendor_Smsc:
            smsc_exit();
        default:
            return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

forward ioctl_mmio_read(in[], in_size, out[], out_size);
public ioctl_mmio_read(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new offset = in[IOCTL_IN_OFFSET + 0];
    new size = in[IOCTL_IN_OFFSET + 1];

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (offset < 0 || offset > PAGE_SIZE || size < 0 || size > 8 || offset + size > PAGE_SIZE)
        return STATUS_INVALID_PARAMETER;

    if (g_mmio_base == 0)
        return STATUS_DEVICE_NOT_READY;

    new va = io_space_map(g_mmio_base, PAGE_SIZE);
    if (!va)
        return STATUS_NO_MEMORY;

    new status = STATUS_SUCCESS;
    new value;

    switch (size) {
        case 1:
            status = fix_status(virtual_read_byte(va + offset, value));
        case 2:
            status = fix_status(virtual_read_word(va + offset, value));
        case 4:
            status = fix_status(virtual_read_dword(va + offset, value));
        case 8:
            status = fix_status(virtual_read_qword(va + offset, value));
        default:
            status = STATUS_INVALID_PARAMETER;
    }

    io_space_unmap(va, size);

    out[0] = value;

    return status;
}

forward ioctl_mmio_write(in[], in_size, out[], out_size);
public ioctl_mmio_write(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 3)
        return STATUS_BUFFER_TOO_SMALL;

    new offset = in[IOCTL_IN_OFFSET + 0];
    new size = in[IOCTL_IN_OFFSET + 1];
    new value = in[IOCTL_IN_OFFSET + 2];

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (offset < 0 || offset > PAGE_SIZE || size < 0 || size > 8 || offset + size > PAGE_SIZE)
        return STATUS_INVALID_PARAMETER;

    if (g_mmio_base == 0)
        return STATUS_DEVICE_NOT_READY;

    new va = io_space_map(g_mmio_base, PAGE_SIZE);
    if (!va)
        return STATUS_NO_MEMORY;

    new status = STATUS_SUCCESS;

    switch (size) {
        case 1:
            status = fix_status(virtual_write_byte(va + offset, value));
        case 2:
            status = fix_status(virtual_write_word(va + offset, value));
        case 4:
            status = fix_status(virtual_write_dword(va + offset, value));
        case 8:
            status = fix_status(virtual_write_qword(va + offset, value));
        default:
            status = STATUS_INVALID_PARAMETER;
    }

    io_space_unmap(va, size);

    return status;
}

is_port_allowed(port) {
    if (port == g_register_port || port == g_value_port)
        return true;

    // we assume that each BAR is a range of 8 bytes at most
    new port_clamped = port & 0xFFF8;
    new valid = false;
    for (new i = 0; i < g_bars_count; i++) {
        if (port_clamped == g_bars[i]) {
            valid = true;
            break;
        }
    }
    return valid;
}

forward ioctl_pio_read(in[], in_size, out[], out_size);
public ioctl_pio_read(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new port = in[IOCTL_IN_OFFSET + 0] & 0xFFFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

forward ioctl_pio_write(in[], in_size, out[], out_size);
public ioctl_pio_write(in[], in_size, out[], out_size) {
    if (in_size < IOCTL_IN_OFFSET + 2)
        return STATUS_BUFFER_TOO_SMALL;

    new port = in[IOCTL_IN_OFFSET + 0] & 0xFFFF;
    new value = in[IOCTL_IN_OFFSET + 1];

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    io_out_byte(port, value);
    return STATUS_SUCCESS;
}

main() {
    return STATUS_SUCCESS;
}

