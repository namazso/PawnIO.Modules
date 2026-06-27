//  AppleT2Smc - PawnIO module for the Apple T2 SMC (MMIO interface)
//  Copyright (C) 2026  golirt1
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
//  SPDX-License-Identifier: LGPL-2.1-or-later
//
//  On 2018-2020 Intel Macs the T2 chip intercepts the legacy SMC I/O-port
//  protocol (0x300/0x304) and instead exposes the SMC through an MMIO window
//  at physical 0xFE0B0000. This module maps that window and performs the SMC
//  request/response handshake in kernel mode, exposing only SMC read / get-key
//  / write operations — never raw physical memory access. Writes are further
//  restricted to fan-control keys so the module can't be used to poke arbitrary
//  SMC state.
//
//  Register layout, access widths and protocol mirror the Linux T2 applesmc
//  driver (MCMrARM/mbp2018-etc, applesmc_t2_kmod.c): control registers are
//  written as 32-bit words, status/length/error are read as bytes, the data
//  buffer is byte-addressed.

#include <pawnio.inc>

#define T2_SMC_PHYS         0xFE0B0000
#define T2_SMC_SIZE         0x10000

// Register offsets from the mapped base
#define SMC_DATA            0x0000      // data buffer (byte-addressed, up to 32 bytes)
#define SMC_KEY_NAME        0x0078      // 4-byte ASCII key name (dword write)
#define SMC_KEY_DATALEN     0x007D      // data length
#define SMC_KEY_SMCID       0x007E      // always 0 (dword write)
#define SMC_KEY_CMD         0x007F      // command (dword write) / error byte (byte read)
#define SMC_KEY_STATUS      0x4005      // status byte: bit 0x20 = operation complete
#define SMC_DONE_BIT        0x20

// GET_KEY_TYPE result layout (overlaps the data window)
#define SMC_TYPE_CODE       0x0000      // 4-byte type code
#define SMC_TYPE_DATALEN    0x0005      // key data length
#define SMC_TYPE_FLAGS      0x0006      // key flags

// SMC commands
#define SMC_READ_CMD        0x10
#define SMC_WRITE_CMD       0x11
#define SMC_GET_INDEX_CMD   0x12
#define SMC_GET_TYPE_CMD    0x13

#define SMC_MAX_DATA        32
#define SMC_MIN_WAIT        0x10
#define SMC_RETRY_WAIT      0x100
#define SMC_MAX_WAIT        0x20000

new VA:g_smc_va = NULL;

// Wait until the SMC signals completion (status bit 0x20), with the same
// exponential backoff as the Linux driver. Returns NTSTATUS.
NTSTATUS:smc_wait_done() {
    for (new us = SMC_MIN_WAIT; us < SMC_MAX_WAIT; us <<= 1) {
        microsleep(us);
        new value = 0;
        new NTSTATUS:st = virtual_read_byte(g_smc_va + SMC_KEY_STATUS, value);
        if (!NT_SUCCESS(st))
            return st;
        if (value & SMC_DONE_BIT)
            return STATUS_SUCCESS;
        microsleep(SMC_RETRY_WAIT);
    }
    return STATUS_IO_TIMEOUT;
}

smc_clear_status() {
    new value = 0;
    virtual_read_byte(g_smc_va + SMC_KEY_STATUS, value);
    if (value != 0)
        virtual_write_byte(g_smc_va + SMC_KEY_STATUS, 0);
}

// Issue a read-type command (READ / GET_INDEX / GET_TYPE). key is the 4-byte
// key name packed little-endian (key[0] in the low byte), matching *(u32*)key
// on x86. Fills data[0..len-1].
NTSTATUS:smc_read_op(cmd, key, len, data[SMC_MAX_DATA]) {
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;

    smc_clear_status();
    virtual_write_dword(g_smc_va + SMC_KEY_NAME, key);
    virtual_write_dword(g_smc_va + SMC_KEY_SMCID, 0);
    virtual_write_dword(g_smc_va + SMC_KEY_CMD, cmd);

    new NTSTATUS:st = smc_wait_done();
    if (!NT_SUCCESS(st))
        return st;

    new err = 0;
    st = virtual_read_byte(g_smc_va + SMC_KEY_CMD, err);
    if (!NT_SUCCESS(st)) return st;
    if (err != 0)
        return STATUS_UNSUCCESSFUL;

    if (cmd == SMC_GET_TYPE_CMD) {
        // Special MMIO layout: type@0..3, datalen@5, flags@6. We repack it into
        // the 6-byte form the I/O-port path returns: [len, type0..3, flags].
        new tlen = 0, flags = 0;
        virtual_read_byte(g_smc_va + SMC_TYPE_DATALEN, tlen);
        virtual_read_byte(g_smc_va + SMC_TYPE_FLAGS, flags);
        data[0] = tlen & 0xFF;
        for (new i = 0; i < 4; i++) {
            new b = 0;
            virtual_read_byte(g_smc_va + SMC_TYPE_CODE + i, b);
            data[1 + i] = b & 0xFF;
        }
        data[5] = flags & 0xFF;
        return STATUS_SUCCESS;
    }

    if (cmd == SMC_READ_CMD) {
        // The SMC reports how many bytes the key holds; it must match the request.
        new rlen = 0;
        virtual_read_byte(g_smc_va + SMC_KEY_DATALEN, rlen);
        if ((rlen & 0xFF) != len)
            return STATUS_INVALID_PARAMETER;
    }

    for (new i = 0; i < len; i++) {
        new b = 0;
        st = virtual_read_byte(g_smc_va + SMC_DATA + i, b);
        if (!NT_SUCCESS(st)) return st;
        data[i] = b & 0xFF;
    }
    return STATUS_SUCCESS;
}

// Only fan-control keys may be written: F<n>Tg (target RPM), F<n>Md (manual
// mode) and "FS! " (manual mask). key is packed little-endian.
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
    if (c1 == 'S' && c2 == '!' && c3 == ' ') // FS!
        return true;
    return false;
}

NTSTATUS:smc_write_op(key, len, data[SMC_MAX_DATA]) {
    if (len < 0 || len > SMC_MAX_DATA)
        return STATUS_INVALID_PARAMETER;
    if (!is_allowed_write(key))
        return STATUS_ACCESS_DENIED;

    smc_clear_status();
    virtual_write_dword(g_smc_va + SMC_KEY_NAME, key);
    for (new i = 0; i < len; i++)
        virtual_write_byte(g_smc_va + SMC_DATA + i, data[i] & 0xFF);
    virtual_write_dword(g_smc_va + SMC_KEY_DATALEN, len);
    virtual_write_dword(g_smc_va + SMC_KEY_SMCID, 0);
    virtual_write_dword(g_smc_va + SMC_KEY_CMD, SMC_WRITE_CMD);

    new NTSTATUS:st = smc_wait_done();
    if (!NT_SUCCESS(st))
        return st;

    new err = 0;
    st = virtual_read_byte(g_smc_va + SMC_KEY_CMD, err);
    if (!NT_SUCCESS(st)) return st;
    return (err == 0) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
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
    if (g_smc_va == NULL)
        return STATUS_DEVICE_NOT_READY;

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
    if (g_smc_va == NULL)
        return STATUS_DEVICE_NOT_READY;

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

    g_smc_va = io_space_map(T2_SMC_PHYS, T2_SMC_SIZE);
    if (g_smc_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    // Probe the SMC: read "FNum" (fan count, 1 byte). On a real T2 SMC this
    // succeeds; on non-Apple hardware the handshake times out or the error byte
    // is set, so we refuse to load and never touch memory that isn't an SMC.
    // "FNum" packed little-endian = 'm'<<24 | 'u'<<16 | 'N'<<8 | 'F'.
    new data[SMC_MAX_DATA];
    new NTSTATUS:st = smc_read_op(SMC_READ_CMD, 0x6D754E46, 1, data);
    if (!NT_SUCCESS(st)) {
        io_space_unmap(g_smc_va, T2_SMC_SIZE);
        g_smc_va = NULL;
        return STATUS_NOT_SUPPORTED;
    }

    debug_print("AppleT2Smc: SMC ok, FNum=%d", data[0]);
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_smc_va != NULL) {
        io_space_unmap(g_smc_va, T2_SMC_SIZE);
        g_smc_va = NULL;
    }
    return STATUS_SUCCESS;
}
