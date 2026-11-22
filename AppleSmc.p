//  AppleSmc - PawnIO module for the legacy Apple SMC (I/O port interface)
//  Copyright (C) 2026  Jesse Muth
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
//
//  Pre-T2 Intel Macs (everything before the 2018 models) expose the Apple
//  System Management Controller through two I/O ports: a data port at 0x300
//  and a command/status port at 0x304. This module performs the SMC
//  request/response handshake in kernel mode and exposes only SMC key
//  reads and fan-control key writes - never raw port access. Writes are
//  restricted to fan-control keys so the module cannot be used to poke
//  arbitrary SMC state.
//
//  The port protocol, status bits and wait behaviour mirror the Linux
//  kernel applesmc driver (drivers/hwmon/applesmc.c), which is also what
//  smcFanControl and every other open implementation follows. On T2
//  machines the T2 chip intercepts these ports and the protocol does not
//  answer; use the AppleT2Smc module there instead. The load-time probe
//  below performs a real key read and refuses to load when no legacy SMC
//  responds, so this module stays inert on T2 and non-Apple hardware.
//
//  Transactions are not serialised against other SMC clients (such as a
//  concurrently running vendor tool). Callers should use a single handle
//  and avoid running two SMC controllers at once, the same constraint the
//  Linux driver notes with its global lock.

#include <pawnio.inc>
#include <registry.inc>

// I/O ports of the legacy SMC interface
#define SMC_DATA_PORT       0x300
#define SMC_CMD_PORT        0x304

// SMC commands
#define SMC_READ_CMD        0x10
#define SMC_WRITE_CMD       0x11
#define SMC_GET_INDEX_CMD   0x12
#define SMC_GET_TYPE_CMD    0x13

// Status register bits (command/status port)
#define SMC_AWAITING_DATA   0x01    // SMC has data waiting to be read
#define SMC_IB_CLOSED       0x02    // SMC will ignore any input
#define SMC_BUSY            0x04    // Command in progress

#define SMC_MAX_DATA        32
#define SMC_MIN_WAIT        8       // microseconds, as in applesmc.c

// "#KEY" packed little-endian ('#' in the low byte): the key count, present
// on every SMC and used as the load-time probe.
#define SMC_KEY_COUNT_PACKED 0x59454B23

/// Compare two NUL-terminated codepoint strings.
///
/// @param str1 Unpacked string (one codepoint per cell)
/// @param str2 Unpacked string (one codepoint per cell)
/// @return 0 if equal, nonzero otherwise
stock str_eq(const str1[], const str2[]) {
    new i = 0;
    while (str1[i] != 0) {
        new c = str2[i];
        if (c == 0)
            return 1;
        if (str1[i] != c)
            return 1;
        i++;
    }
    return str2[i] != 0 ? 1 : 0;
}

// Refuse to run unless the firmware says this is an Apple machine. The
// port addresses below are constants, so this is the first of two gates
// (the second is a live SMC probe in main) that keep the module from
// touching ports on hardware where they belong to something else.
// Same DMI route AppleT2Smc.p uses.
bool:is_apple_machine() {
    if (!reg_resolve_imports())
        return false;

    new bios_path[] = ''\\Registry\\Machine\\HARDWARE\\DESCRIPTION\\System\\BIOS'';

    new manufacturer[64];
    new mfg_len = 0;
    if (reg_query_sz(bios_path, ''SystemManufacturer'', manufacturer, sizeof manufacturer, mfg_len) != STATUS_SUCCESS)
        return false;

    return str_eq(manufacturer, ''Apple Inc.'') == 0;
}

// Wait until (status & mask) == val, with the applesmc.c schedule: 24
// polls, starting at SMC_MIN_WAIT microseconds and doubling the delay
// after the tenth poll. Returns an NTSTATUS.
NTSTATUS:wait_status(val, mask) {
    new us = SMC_MIN_WAIT;
    for (new i = 0; i < 24; i++) {
        new status = io_in_byte(SMC_CMD_PORT);
        if ((status & mask) == val)
            return STATUS_SUCCESS;
        microsleep(us);
        if (i > 9)
            us <<= 1;
    }
    return STATUS_IO_TIMEOUT;
}

// Write one byte to a port the way applesmc.c's send_byte does: wait for
// the input buffer to open, then (as a separate read) for the busy bit
// to set, then write.
NTSTATUS:send_byte(b, port) {
    new NTSTATUS:st = wait_status(0, SMC_IB_CLOSED);
    if (!NT_SUCCESS(st))
        return st;
    st = wait_status(SMC_BUSY, SMC_BUSY);
    if (!NT_SUCCESS(st))
        return st;
    io_out_byte(port, b & 0xFF);
    return STATUS_SUCCESS;
}

// Write a command byte to the command port (applesmc.c's send_command).
NTSTATUS:send_command(cmd) {
    new NTSTATUS:st = wait_status(0, SMC_IB_CLOSED);
    if (!NT_SUCCESS(st))
        return st;
    io_out_byte(SMC_CMD_PORT, cmd & 0xFF);
    return STATUS_SUCCESS;
}

// applesmc.c's smc_sane: if the busy bit is stuck high, issue a read
// command to reset the SMC state machine; if it is still stuck after
// that, the SMC is jammed and the transaction is abandoned.
NTSTATUS:smc_sane() {
    new NTSTATUS:st = wait_status(0, SMC_BUSY);
    if (NT_SUCCESS(st))
        return st;
    st = send_command(SMC_READ_CMD);
    if (!NT_SUCCESS(st))
        return st;
    return wait_status(0, SMC_BUSY);
}

// Send the 4 key bytes. key is packed little-endian (key[0], the first
// character, in the low byte) and is sent first character first.
NTSTATUS:send_argument(key) {
    for (new i = 0; i < 4; i++) {
        new NTSTATUS:st = send_byte((key >> (8 * i)) & 0xFF, SMC_DATA_PORT);
        if (!NT_SUCCESS(st))
            return st;
    }
    return STATUS_SUCCESS;
}

// Issue a read-type command (READ / GET_INDEX / GET_TYPE) and collect
// len bytes into data. Mirrors applesmc.c's read_smc, including the
// length byte (a no-op on 2012 and later SMCs, required before) and the
// flush of any trailing bytes after the requested payload.
NTSTATUS:smc_read_op(cmd, key, len, data[SMC_MAX_DATA]) {
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    new NTSTATUS:st = smc_sane();
    if (!NT_SUCCESS(st))
        return st;

    st = send_command(cmd);
    if (NT_SUCCESS(st))
        st = send_argument(key);
    if (NT_SUCCESS(st))
        st = send_byte(len, SMC_DATA_PORT);
    if (!NT_SUCCESS(st))
        return st;

    for (new i = 0; i < len; i++) {
        st = wait_status(SMC_AWAITING_DATA | SMC_BUSY, SMC_AWAITING_DATA | SMC_BUSY);
        if (!NT_SUCCESS(st))
            return st;
        data[i] = io_in_byte(SMC_DATA_PORT) & 0xFF;
    }

    // Read the data port until the awaiting-data bit clears.
    for (new i = 0; i < 16; i++) {
        microsleep(SMC_MIN_WAIT);
        new status = io_in_byte(SMC_CMD_PORT);
        if (!(status & SMC_AWAITING_DATA))
            break;
        io_in_byte(SMC_DATA_PORT);
    }

    return wait_status(0, SMC_BUSY);
}

// Only fan-control keys may be written: F<n>Tg (target RPM), F<n>Md
// (manual mode), F<n>Mn (minimum RPM, the classic smcFanControl method)
// and "FS! " (manual fan mask). key is packed little-endian.
bool:is_allowed_write(key) {
    new c0 =  key        & 0xFF;
    new c1 = (key >> 8)  & 0xFF;
    new c2 = (key >> 16) & 0xFF;
    new c3 = (key >> 24) & 0xFF;
    if (c0 != 'F')
        return false;
    if (c2 == 'T' && c3 == 'g')   // F?Tg
        return true;
    if (c2 == 'M' && c3 == 'd')   // F?Md
        return true;
    if (c2 == 'M' && c3 == 'n')   // F?Mn
        return true;
    if (c1 == 'S' && c2 == '!' && c3 == ' ') // FS!
        return true;
    return false;
}

// Issue a write of len bytes to a fan-control key. Mirrors applesmc.c's
// write_smc.
NTSTATUS:smc_write_op(key, len, data[SMC_MAX_DATA]) {
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;
    if (!is_allowed_write(key))
        return STATUS_ACCESS_DENIED;

    new NTSTATUS:st = smc_sane();
    if (!NT_SUCCESS(st))
        return st;

    st = send_command(SMC_WRITE_CMD);
    if (NT_SUCCESS(st))
        st = send_argument(key);
    if (NT_SUCCESS(st))
        st = send_byte(len, SMC_DATA_PORT);
    if (!NT_SUCCESS(st))
        return st;

    for (new i = 0; i < len; i++) {
        st = send_byte(data[i], SMC_DATA_PORT);
        if (!NT_SUCCESS(st))
            return st;
    }

    return wait_status(0, SMC_BUSY);
}

/// Read an SMC key (or query a key name / type).
///
/// @param in [0] = command (0x10 read, 0x12 get-key-by-index, 0x13 get-key-type)
/// @param in [1] = key packed little-endian (key[0] in the low byte)
/// @param in [2] = number of bytes to read (1..32)
/// @param in_size Must be 3
/// @param out [0..len-1] = bytes read, one byte per cell
/// @param out_size Must be 32
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_smc_read, 3, 32) {
    new cmd = in[0];
    new key = in[1];
    new len = in[2];
    if (cmd != SMC_READ_CMD && cmd != SMC_GET_INDEX_CMD && cmd != SMC_GET_TYPE_CMD)
        return STATUS_INVALID_PARAMETER;
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    new data[SMC_MAX_DATA];
    new NTSTATUS:st = smc_read_op(cmd, key, len, data);
    if (!NT_SUCCESS(st))
        return st;

    for (new i = 0; i < SMC_MAX_DATA; i++)
        out[i] = (i < len) ? data[i] : 0;

    return STATUS_SUCCESS;
}

/// Write an SMC fan-control key.
///
/// @param in [0] = key packed little-endian
/// @param in [1] = number of bytes to write (1..32)
/// @param in [2..33] = bytes to write, one byte per cell
/// @param in_size Must be 34
/// @param out_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_smc_write, 34, 0) {
    new key = in[0];
    new len = in[1];
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    new data[SMC_MAX_DATA];
    for (new i = 0; i < SMC_MAX_DATA; i++)
        data[i] = in[2 + i];

    return smc_write_op(key, len, data);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // Read-only, and before any port is touched: if the firmware does
    // not say Apple, these ports are not an SMC and we have no business
    // there.
    if (!is_apple_machine()) {
        debug_print(''AppleSmc: not an Apple machine, declining\n'');
        return STATUS_NOT_SUPPORTED;
    }

    // The protocol's own gate: read the key count ("#KEY", big-endian
    // ui32). A legacy SMC answers; a T2 machine, whose T2 chip
    // intercepts these ports, does not answer with a plausible count,
    // and neither does unrelated hardware that happens to sit at
    // 0x300/0x304 on a non-Mac board that passed the DMI check by
    // spoofing. Only proceed on a sane count.
    new data[SMC_MAX_DATA];
    new NTSTATUS:st = smc_read_op(SMC_READ_CMD, SMC_KEY_COUNT_PACKED, 4, data);
    if (!NT_SUCCESS(st)) {
        debug_print(''AppleSmc: SMC probe read failed, declining\n'');
        return STATUS_NOT_SUPPORTED;
    }

    new count = (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3];
    if (count < 10 || count > 10000) {
        debug_print(''AppleSmc: implausible key count %d, declining\n'', count);
        return STATUS_NOT_SUPPORTED;
    }

    debug_print(''AppleSmc: legacy SMC ok, %d keys\n'', count);
    return STATUS_SUCCESS;
}
