//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2023  namazso <admin@namazso.eu>
//  Modified 2026-07-12 for an MSI NCT6687D-R-only BAR discovery boundary.
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
const DEVICE_SELECT_REGISTER = 0x07;
const TARGET_CHIP_ID = 0xD5;
const TARGET_CHIP_REVISION = 0x92;
const TARGET_LOGICAL_DEVICE = 0x0B;

new g_register_port;
new g_bars[1];
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

NTSTATUS:find_bars() {
    new chip_id = superio_inb(CHIP_ID_REGISTER);
    new chip_revision = superio_inb(CHIP_REVISION_REGISTER);

    if (chip_id != TARGET_CHIP_ID || chip_revision != TARGET_CHIP_REVISION) {
        debug_print(''LpcIO: Unsupported chip identity %x/%x\n'', chip_id, chip_revision);
        return STATUS_NOT_SUPPORTED;
    }

    new original_logical_device = superio_inb(DEVICE_SELECT_REGISTER);
    new NTSTATUS:status = STATUS_SUCCESS;

    if (!select(TARGET_LOGICAL_DEVICE))
        status = STATUS_NOT_FOUND;

    new address = 0;
    new verify = 0;
    if (status == STATUS_SUCCESS)
        address = superio_inw(BASE_ADDRESS_REGISTER);

    microsleep(1000);
    if (status == STATUS_SUCCESS)
        verify = superio_inw(BASE_ADDRESS_REGISTER);

    if (status == STATUS_SUCCESS && address != verify)
        status = STATUS_CONFLICTING_ADDRESSES;
    else if (status == STATUS_SUCCESS && (address < 0x100 || (address & 0xF007) != 0))
        status = STATUS_INVALID_ADDRESS;
    else if (status == STATUS_SUCCESS) {
        g_bars[0] = address;
        g_bars_count = 1;
        debug_print(''LpcIO: Authorized NCT6687D-R BAR %X\n'', address);
    }

    if (!select(original_logical_device)) {
        g_bars_count = 0;
        return STATUS_DEVICE_PROTOCOL_ERROR;
    }

    return status;
}

/// Select chip slot.
///
/// Slot 0 is at 0x2e/0x2f, slot 1 at 0x4e/0x4f.
///
/// @param in [0] = Slot (0 or 1)
/// @param in_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
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

/// Find BARs to update allowed ports.
///
/// This should be called after configuration mode was entered and chip ID is valid. After calling this,
/// you can use ioctl_pio_inb/ioctl_pio_outb to read/write from/to the found BARs.
///
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
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

/// Read a byte from a port.
///
/// @param in [0] = Port
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_pio_inb, 1, 1) {
    new port = in[0] & 0xFFFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

/// Write a byte to a port.
///
/// @param in [0] = Port, [1] = Value
/// @param in_size Must be 2
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
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

/// Read a byte from Super IO.
///
/// @param in [0] = Register
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_superio_inb, 1, 1) {
    new reg = in[0] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    out[0] = superio_inb(reg);
    return STATUS_SUCCESS;
}

/// Read a word from Super IO.
///
/// @param in [0] = Register
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_superio_inw, 1, 1) {
    new reg = in[0] & 0xFF;

    if (!is_ready())
        return STATUS_DEVICE_NOT_READY;

    out[0] = superio_inw(reg);
    return STATUS_SUCCESS;
}

/// Write a byte to Super IO.
///
/// @param in [0] = Register, [1] = Value
/// @param in_size Must be 2
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_ISABUS.HTP.Method" mutant before calling this
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
