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

const PCI_CONFIG_PORT_FIRST = 0x0CF8;
const PCI_CONFIG_PORT_LAST = 0x0CFF;

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

bool:is_port_blacklisted(port) {
    return port >= PCI_CONFIG_PORT_FIRST && port <= PCI_CONFIG_PORT_LAST;
}

check_and_add_bar(val, val_v) {
    if (val == val_v && val != 0 && val != 0xFFFF) {
        // in fixed range, probably some garbage
        if (val < 0x100)
            return;

        // some Fintek chips have address register offset 0x05 added already
        if ((val & 0x07) == 0x05)
            val &= 0xFFF8;

        if (is_port_blacklisted(val) || is_port_blacklisted(val + 7))
            return;

        // duplicate
        if (g_bars_count > 0 && g_bars[g_bars_count - 1] == val)
            return;

        // duplicate
        if (g_bars_count > 1 && g_bars[g_bars_count - 2] == val)
            return;

        if (g_bars_count >= sizeof g_bars)
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
    if (is_port_blacklisted(port))
        return false;

    if (port == g_register_port || port == g_register_port + 1)
        return true;

    // Allow fixed ports 0x25C and 0x25D for ASUS EC
    // Please keep in mind that the official AsusSAIO driver holds a kernel MUTEX, which usermode can't possibly
    // acquire, so it is recommended that you make sure that the official driver is unloaded when poking these.
    if (port == 0x25C || port == 0x25D)
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

// Proposed LpcIO extension, LGPL-2.1-or-later, same terms as LpcIO.p.
// No generic whitelist changes; no app/board-name lockdown.

const NCT67_REPORT_CELLS = 64;
const NCT67_PRESENT = 63;

nct67_put(report[NCT67_REPORT_CELLS], field, value) {
    report[field] = value;
    report[NCT67_PRESENT] |= (1 << field);
}

bool:nct67_indexed_base(base) {
    return base >= 0x100 && base <= 0xFFF8 && (base & 7) == 0
        && !(base <= 0xCFF && base + 7 >= 0xCF8);
}

bool:nct67_flat_base(base) {
    return base >= 0x100 && base <= 0xFE80 && (base & 0x7F) == 0
        && !(base <= 0xCFF && base + 0x20 >= 0xCF8);
}

// Full initial/final endpoint bracket; restores its own LDN before AA.
bool:nct67_endpoint(report[NCT67_REPORT_CELLS], bool:after) {
    new old_ldn = 0;
    new bool:saved = false;
    new bool:good = false;
    new first = after ? 10 : 5;
    new cleanup_shift = after ? 3 : 0;
    io_out_byte(g_register_port, 0x87);
    io_out_byte(g_register_port, 0x87);
    for (new once = 0; once < 1; once++) {
        new chip = superio_inw(0x20);
        nct67_put(report, after ? 4 : 3, chip);
        if (chip != 0xD806)
            break;
        old_ldn = superio_inb(7);
        nct67_put(report, after ? 35 : 27, old_ldn);
        saved = true;
        if (!select(0x0B))
            break;
        nct67_put(report, first, superio_inw(0x60));
        nct67_put(report, first + 1, superio_inw(0x60));
        nct67_put(report, first + 2, superio_inw(0x64));
        nct67_put(report, first + 3, superio_inw(0x64));
        nct67_put(report, first + 4, superio_inb(0x30));
        nct67_put(report, after ? 30 : 29, superio_inb(0x28));
        if (report[first] != report[first + 1] || report[first + 2] != report[first + 3]
            || !nct67_indexed_base(report[first]) || !nct67_flat_base(report[first + 2])
            || !(report[first + 4] & 1) || (report[after ? 30 : 29] & 0x10))
            break;
        if (after && (report[first] != report[5] || report[first + 2] != report[7]))
            break;
        good = true;
    }
    if (saved) {
        new fresh_chip = superio_inw(0x20);
        nct67_put(report, after ? 38 : 37, fresh_chip);
        if (fresh_chip == 0xD806) {
            superio_outb(7, old_ldn);
            new restored = superio_inb(7);
            nct67_put(report, after ? 36 : 28, restored);
            if (restored == old_ldn)
                report[31] |= (2 << cleanup_shift);
            else
                good = false;
        } else {
            good = false;
        }
    }
    io_out_byte(g_register_port, 0xAA);
    report[31] |= (1 << cleanup_shift);
    return good;
}

// Fresh endpoint check before restoring any saved indexed-bank selector.
// This never uses the old indexed aperture after a changed/unknown endpoint.
bool:nct67_restore_endpoint(report[NCT67_REPORT_CELLS], bool:after) {
    new offset = after ? 45 : 39;
    new old_ldn = 0;
    new bool:saved = false;
    new bool:good = false;
    io_out_byte(g_register_port, 0x87);
    io_out_byte(g_register_port, 0x87);
    for (new once = 0; once < 1; once++) {
        nct67_put(report, offset, superio_inw(0x20));
        if (report[offset] != 0xD806)
            break;
        old_ldn = superio_inb(7);
        nct67_put(report, offset + 1, old_ldn);
        saved = true;
        if (!select(0x0B))
            break;
        nct67_put(report, offset + 2, superio_inw(0x60));
        nct67_put(report, offset + 3, superio_inw(0x60));
        nct67_put(report, offset + 4, superio_inb(0x30));
        good = report[offset + 2] == report[5] && report[offset + 3] == report[5]
            && (report[offset + 4] & 1) != 0;
    }
    if (saved) {
        if (superio_inw(0x20) == 0xD806) {
            superio_outb(7, old_ldn);
            nct67_put(report, offset + 5, superio_inb(7));
            good = good && report[offset + 5] == old_ldn;
        } else {
            good = false;
        }
    }
    io_out_byte(g_register_port, 0xAA);
    return good;
}

nct67_hwm_index(base, reg) {
    io_out_byte(base + 5, reg);
    return io_in_byte(base + 6);
}

nct67_hwm_bank(base, bank) {
    io_out_byte(base + 5, 0x4E);
    io_out_byte(base + 6, bank);
}

bool:nct67_identity(report[NCT67_REPORT_CELLS], bool:after) {
    new base = report[5];
    new bank = nct67_hwm_index(base, 0x4E);
    nct67_put(report, after ? 51 : 25, bank);
    nct67_hwm_bank(base, 0);
    new low = nct67_hwm_index(base, 0x4F);
    nct67_put(report, after ? 16 : 15, nct67_hwm_index(base, 0x58));
    nct67_hwm_bank(base, 0x80);
    new high = nct67_hwm_index(base, 0x4F);
    nct67_put(report, after ? 18 : 17, (high << 8) | low);
    if (!nct67_restore_endpoint(report, after))
        return false;
    nct67_hwm_bank(base, bank);
    new restored = nct67_hwm_index(base, 0x4E);
    nct67_put(report, after ? 52 : 26, restored);
    if (restored != bank)
        return false;
    report[31] |= after ? 32 : 4;
    return report[after ? 16 : 15] == 0xC1 && report[after ? 18 : 17] == 0x5CA3;
}

/// Capture one fixed NCT67 D806/C1 flat-temperature byte and endpoint evidence.
///
/// No generic PIO window is added. This operation derives LDN0B64/65 itself.
/// The caller owns Access_ISABUS.HTP.Method and configuration mode is CLOSED.
/// This is cooperative OS serialization, not ownership of an AML/SMM mutex.
/// @param in_size Must be0; no caller address, offset or control data.
/// @param out_size Must be64 cells;64-bit Pawn cells required.
/// @param out Fixed version1 raw/presence/phase/cleanup report; see schema.
/// @return STATUS_SUCCESS means a structured report, NOT a valid temperature.
///         out[1]==0 and all phase/cleanup flags are additionally mandatory.
///         VM/native abort or invalid returned count means unknown cleanup.
/// @warning Unsupported chips/revision/options are withheld, never enabled.
DEFINE_IOCTL_SIZED(ioctl_nct67_temp9_snapshot, 0, 64) {
    if (in_size != 0 || out_size != NCT67_REPORT_CELLS)
        return STATUS_INVALID_PARAMETER;
    for (new i = 0; i < NCT67_REPORT_CELLS; i++)
        out[i] = 0;
    nct67_put(out, 0, 1);
    new NTSTATUS:status = STATUS_DEVICE_NOT_READY;
    for (new once = 0; once < 1; once++) {
        nct67_put(out, 32, 1);
        if (g_register_port != 0x2E && g_register_port != 0x4E)
            break;
        nct67_put(out, 32, 2);
        if (!nct67_endpoint(out, false))
            break;
        out[2] |= 1;
        nct67_put(out, 32, 3);
        if (!nct67_identity(out, false))
            break;
        out[2] |= 2;
        nct67_put(out, 32, 4);
        new bool:ready = false;
        for (new i = 0; i < 4; i++) {
            new offset = i == 0 ? 0 : i == 1 ? 2 : i == 2 ? 0x10 : 0x20;
            nct67_put(out, 20 + i, io_in_byte(out[7] + offset));
            out[19] |= (1 << i);
            if (out[20 + i] != 0xFF) {
                ready = true;
                break;
            }
        }
        if (!ready)
            break;
        out[2] |= 4;
        nct67_put(out, 32, 5);
        nct67_put(out, 24, io_in_byte(out[7] + 0x16));
        out[2] |= 8;
        nct67_put(out, 32, 6);
        if (!nct67_endpoint(out, true))
            break;
        out[2] |= 16;
        nct67_put(out, 32, 7);
        if (!nct67_identity(out, true))
            break;
        out[2] |= 32;
        nct67_put(out, 32, 8);
        if (out[31] != 63)
            break;
        status = STATUS_SUCCESS;
        nct67_put(out, 32, 9);
    }
    nct67_put(out, 1, (_:status) & 0xFFFFFFFF);
    return STATUS_SUCCESS;
}
