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

const CHIP_ID_REGISTER = 0x20;
const CHIP_REVISION_REGISTER = 0x21;

const BASE_ADDRESS_REGISTER = 0x60;
const BASE_ADDRESS_REGISTER_2 = 0x62;

const DEVICE_SELECT_REGISTER = 0x07;

new g_register_port;
new g_bars[128];
new g_bars_count = 0;

reset() {
    g_register_port = 0;
    g_bars_count = 0;
}

is_ready() {
    return g_register_port != 0;
}

superio_inb(reg) {
    io_out_byte(g_register_port, reg);
    return io_in_byte(g_register_port + 1);
}

superio_outb(reg, val) {
    io_out_byte(g_register_port, reg);
    io_out_byte(g_register_port + 1, val);
}

superio_inw(reg) {
    return (superio_inb(reg) << 8) | superio_inb(reg + 1);
}

bool:select(val) {
    superio_outb(DEVICE_SELECT_REGISTER, val);
    return superio_inb(DEVICE_SELECT_REGISTER) == val;
}

check_and_add_bar(val, val_v) {
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

        // duplicate
        if (g_bars_count > 1 && g_bars[g_bars_count - 2] == val)
            return;

        g_bars[g_bars_count] = val;
        g_bars_count++;
        debug_print(''LpcIO: Added %X as BAR\n'', val);
    }
}

NTSTATUS:find_bars() {
    new chip_id = superio_inb(CHIP_ID_REGISTER);

    if (chip_id == 0x00 || chip_id == 0xFF) {
        debug_print(''LpcIO: Invalid chip ID %x, not entered?\n'', chip_id);
        return STATUS_NOT_FOUND;
    }

    debug_print(''LpcIO: Finding BARs for %x\n'', g_register_port);
    new vals[256][2];
    for (new i = 0; i < 0xFF; i++) {
        if(select(i)) {
            vals[i][0] = superio_inw(BASE_ADDRESS_REGISTER);
            vals[i][1] = superio_inw(BASE_ADDRESS_REGISTER_2);
        }
    }
    microsleep(1000);
    for (new i = 0; i < 0xFF; i++) {
        if(select(i)) {
            new vals_v[2];
            vals_v[0] = superio_inw(BASE_ADDRESS_REGISTER);
            vals_v[1] = superio_inw(BASE_ADDRESS_REGISTER_2);

            check_and_add_bar(vals[i][0], vals_v[0]);
            check_and_add_bar(vals[i][1], vals_v[1]);
        }
    }

    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_select_slot, 1, 0) {
    reset();

    new slot = in[0];

    debug_print(''LpcIO: Selected slot %d\n'', slot);

    if (slot == 0) {
        g_register_port = 0x2e;
    } else if (slot == 1) {
        g_register_port = 0x4e;
    } else {
        return STATUS_INVALID_PARAMETER;
    }

    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_find_bars, 0, 0) {
    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    g_bars_count = 0;
    return find_bars();
}

bool:is_port_allowed(port) {
    if (port == g_register_port || port == g_register_port + 1)
        return true;

    // we assume that each BAR is a range of 8 bytes at most
    new port_clamped = port & 0xFFF8;
    new bool:valid = false;
    for (new i = 0; i < g_bars_count; i++) {
        if (port_clamped == g_bars[i]) {
            valid = true;
            break;
        }
    }
    return valid;
}

DEFINE_IOCTL_SIZED(ioctl_pio_inb, 1, 1) {
    new port = in[0] & 0xFFFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_pio_outb, 2, 0) {
    new port = in[0] & 0xFFFF;
    new value = in[1];

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    io_out_byte(port, value);
    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_superio_inb, 1, 1) {
    new reg = in[0] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    out[0] = superio_inb(reg);
    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_superio_inw, 1, 1) {
    new reg = in[0] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    out[0] = superio_inw(reg);
    return STATUS_SUCCESS;
}

DEFINE_IOCTL_SIZED(ioctl_superio_outb, 2, 0) {
    new reg = in[0] & 0xFF;
    new val = in[1] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    superio_outb(reg, val);
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    
    return STATUS_SUCCESS;
}
