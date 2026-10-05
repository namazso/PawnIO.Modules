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
//
//  SPDX-License-Identifier: LGPL-2.1-or-later

#include <pawnio.inc>

const EC_DATA_PORT = 0x62;
const EC_COMMAND_PORT = 0x66;
const EC_COMMAND_READ = 0x80;
const EC_STATUS_OUTPUT_BUFFER_FULL = 0x01;
const EC_STATUS_INPUT_BUFFER_FULL = 0x02;
const EC_REGISTER_COUNT = 0x100;
const EC_WAIT_RETRIES = 50;
const EC_WAIT_DELAY_US = 1000;
const EC_READ_RETRIES = 5;

is_port_allowed(port) {
    return port == 0x62 || port == 0x66;
}

/// Wait for one ACPI EC status condition.
///
/// The delay is deliberately bounded. A module call must not spin forever if
/// the EC is absent, suspended, or left in an invalid protocol state.
wait_ec_status(mask, set) {
    for (new i = 0; i < EC_WAIT_RETRIES; i++) {
        new status = io_in_byte(EC_COMMAND_PORT);
        if (set) {
            if ((status & mask) != 0)
                return true;
        } else {
            if ((status & mask) == 0)
                return true;
        }
        microsleep(EC_WAIT_DELAY_US);
    }

    return false;
}

/// Execute one standard ACPI EC register read.
///
/// This is the atomic RD_EC sequence from ACPI section 12. The caller is
/// responsible for holding \BaseNamedObjects\Access_EC for the whole batch.
read_ec_byte(register, &value) {
    if (!wait_ec_status(EC_STATUS_INPUT_BUFFER_FULL, false))
        return false;

    io_out_byte(EC_COMMAND_PORT, EC_COMMAND_READ);

    if (!wait_ec_status(EC_STATUS_INPUT_BUFFER_FULL, false))
        return false;

    io_out_byte(EC_DATA_PORT, register);

    if (!wait_ec_status(EC_STATUS_INPUT_BUFFER_FULL, false))
        return false;

    if (!wait_ec_status(EC_STATUS_OUTPUT_BUFFER_FULL, true))
        return false;

    value = io_in_byte(EC_DATA_PORT);
    return true;
}

/// Read one register with bounded retries while keeping the caller's mutex.
read_ec_byte_retry(register, &value) {
    for (new attempt = 0; attempt < EC_READ_RETRIES; attempt++) {
        if (read_ec_byte(register, value))
            return true;
    }

    return false;
}

/// Read byte from ACPI EC.
///
/// @param in [0] = Port
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_pio_read, 1, 1) {
    new port = in[0] & 0xFFFF;

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    out[0] = io_in_byte(port);
    return STATUS_SUCCESS;
}

/// Write byte to ACPI EC.
///
/// @param in [0] = Port, [1] = Value
/// @param in_size Must be 2
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_pio_write, 2, 0) {
    new port = in[0] & 0xFFFF;
    new value = in[1];

    if (!is_port_allowed(port))
        return STATUS_ACCESS_DENIED;

    io_out_byte(port, value);
    return STATUS_SUCCESS;
}

/// Read one EC register using the complete ACPI EC command sequence.
///
/// @param in [0] = register
/// @param in_size Must be 1
/// @param out [0] = value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_ec_read_byte, 1, 1) {
    new register = in[0];
    new value;

    if (register < 0 || register >= EC_REGISTER_COUNT)
        return STATUS_INVALID_PARAMETER;

    if (!read_ec_byte_retry(register, value))
        return STATUS_IO_TIMEOUT;

    out[0] = value;
    return STATUS_SUCCESS;
}

/// Read a contiguous range of EC registers using one PawnIO request.
///
/// @param in [0] = start register, [1] = register count
/// @param in_size Must be 2
/// @param out Packed little-endian bytes, eight bytes per cell
/// @param out_size Must equal ceil(count / 8)
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_EC" mutant before calling this
DEFINE_IOCTL(ioctl_ec_read_range) {
    if (in_size != 2)
        return STATUS_INVALID_PARAMETER;

    new start = in[0];
    new count = in[1];
    if (start < 0 || start >= EC_REGISTER_COUNT || count <= 0 || count > EC_REGISTER_COUNT)
        return STATUS_INVALID_PARAMETER;
    if (start + count > EC_REGISTER_COUNT)
        return STATUS_INVALID_PARAMETER;

    new required_cells = div_ceil(count, cellbytes);
    if (out_size != required_cells)
        return STATUS_INVALID_PARAMETER;

    // Keep results private until every register has been read successfully.
    new data[EC_REGISTER_COUNT];
    for (new i = 0; i < count; i++) {
        new value;
        if (!read_ec_byte_retry(start + i, value))
            return STATUS_IO_TIMEOUT;
        data[i] = value;
    }

    // METHOD_BUFFERED reuses one system buffer for input and output. Clear
    // every output cell first so unused bytes in the final partial cell are
    // deterministic zero rather than stale input data.
    for (new i = 0; i < required_cells; i++)
        out[i] = 0;

    pack_bytes_le(data, out, count);
    return STATUS_SUCCESS;
}

// TODO: Should probably move register read and write from usermode

NTSTATUS:main() {
    return STATUS_SUCCESS;
}
