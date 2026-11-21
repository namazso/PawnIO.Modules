//  PawnIO Modules - Modules for various hardware operations.
//  Copyright (C) 2026  namazso <admin@namazso.eu>
//
//  Permission to use, copy, modify, and/or distribute this software for any
//  purpose with or without fee is hereby granted.
//
//  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
//  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
//  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
//  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
//  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
//  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
//  PERFORMANCE OF THIS SOFTWARE.
//
//  SPDX-License-Identifier: 0BSD

#include <pawnio.inc>

const EC_CMD_PORT = 0x0258;
const EC_DATA_PORT = 0x0257;

public NTSTATUS:main() {
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    return STATUS_SUCCESS;
}

static NTSTATUS:wait_ibf() {
    for (new i = 0; i < 5000; i++) {
        if ((io_in_byte(EC_CMD_PORT) & 0x02) == 0)
            return STATUS_SUCCESS;
        microsleep(10);
    }
    return STATUS_IO_TIMEOUT;
}

static NTSTATUS:wait_obf() {
    for (new i = 0; i < 5000; i++) {
        if ((io_in_byte(EC_CMD_PORT) & 0x01) != 0)
            return STATUS_SUCCESS;
        microsleep(10);
    }
    return STATUS_IO_TIMEOUT;
}

/// Set keyboard backlight PWM brightness (0x00 - 0xFF).
DEFINE_IOCTL_SIZED(ioctl_set_brightness, 1, 0) {
    new level = in[0] & 0xFF;

    new NTSTATUS:status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_CMD_PORT, 0xFF);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_CMD_PORT, 0x9E);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_DATA_PORT, 0x1F);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_DATA_PORT, 0xFF);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_DATA_PORT, level);

    return wait_ibf();
}

/// Read current keyboard backlight PWM brightness (0x00 - 0xFF).
DEFINE_IOCTL_SIZED(ioctl_get_brightness, 0, 1) {
    new NTSTATUS:status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_CMD_PORT, 0xFF);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_CMD_PORT, 0x8E);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_DATA_PORT, 0x1F);

    status = wait_ibf();
    if (status != STATUS_SUCCESS) return status;
    io_out_byte(EC_DATA_PORT, 0x00);

    status = wait_obf();
    if (status != STATUS_SUCCESS) return status;

    out[0] = io_in_byte(EC_DATA_PORT) & 0xFF;
    return STATUS_SUCCESS;
}
