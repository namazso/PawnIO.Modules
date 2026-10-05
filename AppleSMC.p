//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  (module author to be filled in on PR)
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

// Apple SMC (System Management Controller) I/O-port protocol, as used by every
// Intel Mac (including under Boot Camp). This is a long-public, widely
// reimplemented protocol - see e.g. the Linux kernel's drivers/hwmon/applesmc.c
// (GPL, Nicolas Boichat et al.) for another independent implementation of the
// same hardware interface. Only these two fixed, hardcoded ports are ever
// touched - unlike LpcIO this module cannot be used to poke arbitrary I/O.

const APPLESMC_DATA_PORT = 0x300;
const APPLESMC_CMD_PORT  = 0x304;

const APPLESMC_READ_CMD  = 0x10;
const APPLESMC_WRITE_CMD = 0x11;

// Status bits polled on the command port.
const APPLESMC_STATUS_READY      = 0x04; // ready to accept a byte
const APPLESMC_STATUS_DATA_READY = 0x05; // response byte ready to read (0x01|0x04)

const APPLESMC_MAX_DATA = 32; // largest SMC value we'll ever move (actual keys are <=32 bytes)
const APPLESMC_TIMEOUT_ITERS = 4000; // ~4000 * ~1us polling budget per wait, generous but bounded

// Poll the command port until (status & mask) == mask, or time out.
NTSTATUS:wait_status(mask) {
    for (new i = 0; i < APPLESMC_TIMEOUT_ITERS; i++) {
        new status = io_in_byte(APPLESMC_CMD_PORT) & 0x0f;
        if ((status & mask) == mask)
            return STATUS_SUCCESS;
        microsleep(10);
    }
    return STATUS_IO_TIMEOUT;
}

NTSTATUS:send_command(cmd) {
    new st = wait_status(APPLESMC_STATUS_READY);
    if (st != STATUS_SUCCESS)
        return st;
    io_out_byte(APPLESMC_CMD_PORT, cmd);
    return STATUS_SUCCESS;
}

NTSTATUS:send_argument(const key[4]) {
    for (new i = 0; i < 4; i++) {
        new st = wait_status(APPLESMC_STATUS_READY);
        if (st != STATUS_SUCCESS)
            return st;
        io_out_byte(APPLESMC_DATA_PORT, key[i]);
    }
    return STATUS_SUCCESS;
}

NTSTATUS:read_byte(&value) {
    new st = wait_status(APPLESMC_STATUS_DATA_READY);
    if (st != STATUS_SUCCESS)
        return st;
    value = io_in_byte(APPLESMC_DATA_PORT);
    return STATUS_SUCCESS;
}

NTSTATUS:smc_read(const key[4], len, data[APPLESMC_MAX_DATA]) {
    if (len <= 0 || len > APPLESMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    new NTSTATUS:st = send_command(APPLESMC_READ_CMD);
    if (st != STATUS_SUCCESS)
        return st;

    st = send_argument(key);
    if (st != STATUS_SUCCESS)
        return st;

    st = wait_status(APPLESMC_STATUS_READY);
    if (st != STATUS_SUCCESS)
        return st;
    io_out_byte(APPLESMC_DATA_PORT, len);

    for (new i = 0; i < len; i++) {
        new value;
        st = read_byte(value);
        if (st != STATUS_SUCCESS)
            return st;
        data[i] = value;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:smc_write(const key[4], len, const data[APPLESMC_MAX_DATA]) {
    if (len <= 0 || len > APPLESMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    new NTSTATUS:st = send_command(APPLESMC_WRITE_CMD);
    if (st != STATUS_SUCCESS)
        return st;

    st = send_argument(key);
    if (st != STATUS_SUCCESS)
        return st;

    st = wait_status(APPLESMC_STATUS_READY);
    if (st != STATUS_SUCCESS)
        return st;
    io_out_byte(APPLESMC_DATA_PORT, len);

    for (new i = 0; i < len; i++) {
        st = wait_status(APPLESMC_STATUS_READY);
        if (st != STATUS_SUCCESS)
            return st;
        io_out_byte(APPLESMC_DATA_PORT, data[i]);
    }
    return STATUS_SUCCESS;
}

/// Read an SMC key.
///
/// @param in [0] = 4-byte ASCII key packed little-endian into a cell,
///           [1] = number of bytes to read (1-32)
/// @param in_size Must be 2
/// @param out [0..3] = up to 32 bytes read, packed 8 bytes per cell, little-endian
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_smc_read, 2, 4) {
    new key[4];
    unpack_bytes_le(in, key, 4, 0, 0);
    new len = in[1];

    new data[APPLESMC_MAX_DATA];
    for (new i = 0; i < APPLESMC_MAX_DATA; i++) data[i] = 0;

    new NTSTATUS:st = smc_read(key, len, data);
    if (st != STATUS_SUCCESS)
        return st;

    for (new i = 0; i < 4; i++) out[i] = 0;
    pack_bytes_le(data, out, APPLESMC_MAX_DATA, 0, 0);
    return STATUS_SUCCESS;
}

/// Write an SMC key.
///
/// @param in [0] = 4-byte ASCII key packed little-endian into a cell,
///           [1] = number of bytes to write (1-32),
///           [2..5] = data to write, packed 8 bytes per cell, little-endian
/// @param in_size Must be 6
/// @param out_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_smc_write, 6, 0) {
    new key[4];
    unpack_bytes_le(in, key, 4, 0, 0);
    new len = in[1];

    new data[APPLESMC_MAX_DATA];
    new payload[4];
    for (new i = 0; i < 4; i++) payload[i] = in[2 + i];
    unpack_bytes_le(payload, data, APPLESMC_MAX_DATA, 0, 0);

    return smc_write(key, len, data);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
