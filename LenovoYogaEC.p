//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Dragos Bas
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

// Fan control for Lenovo Yoga laptops exposing the EC mailbox at IO 0x5C0/0x5C4.
//
// The mailbox is declared in the DSDT as:
//     OperationRegion(CMDA, SystemIO, 0x05C0, 0x05)
//     X5C0 = data, X5C4 = command/status
//     Method MBEY(cmd, sub, arg)
//
// Verified on a Yoga Pro 9 16IMH9 (83DN), BIOS NKCN35WW. The Lenovo WMI classes
// (LENOVO_FAN_METHOD, LENOVO_GAMEZONE_DATA) exist in the MOF on these machines
// but have no instances, so the mailbox is the only available interface.
//
// Only the fan command (0xEF) is accepted; no general purpose port IO is exposed.

#include <pawnio.inc>

const PORT_DATA = 0x5C0;
const PORT_CMD  = 0x5C4;

const CMD_FAN         = 0xEF;
const SUBCMD_SET_FAN1 = 0x61;
const SUBCMD_SET_FAN2 = 0x62;
const SUBCMD_QUERY    = 0x63;

const QUERY_MIN = 0x01;  // read fan 1
const QUERY_MAX = 0x03;  // 0x02 read fan 2, 0x03 restore EC automatic mode

const TIMEOUT_ITERATIONS = 10000;
const POLL_US            = 10;

const STS_OBF = 0x01;  // output buffer full
const STS_IBF = 0x02;  // input buffer full

static NTSTATUS:wait_ibe() {
    for (new i = 0; i < TIMEOUT_ITERATIONS; i++) {
        if ((io_in_byte(PORT_CMD) & STS_IBF) == 0)
            return STATUS_SUCCESS;
        microsleep(POLL_US);
    }
    return STATUS_IO_TIMEOUT;
}

static NTSTATUS:wait_obf() {
    for (new i = 0; i < TIMEOUT_ITERATIONS; i++) {
        if ((io_in_byte(PORT_CMD) & STS_OBF) != 0)
            return STATUS_SUCCESS;
        microsleep(POLL_US);
    }
    return STATUS_IO_TIMEOUT;
}

static NTSTATUS:wait_obe() {
    for (new i = 0; i < TIMEOUT_ITERATIONS; i++) {
        if ((io_in_byte(PORT_CMD) & STS_OBF) == 0)
            return STATUS_SUCCESS;
        io_in_byte(PORT_DATA);
        microsleep(POLL_US);
    }
    return STATUS_IO_TIMEOUT;
}

static NTSTATUS:mbey(subcmd, arg, &result) {
    new NTSTATUS:st;

    st = wait_ibe(); if (st != STATUS_SUCCESS) return st;
    st = wait_obe(); if (st != STATUS_SUCCESS) return st;

    io_out_byte(PORT_CMD, CMD_FAN);
    st = wait_ibe(); if (st != STATUS_SUCCESS) return st;

    io_out_byte(PORT_DATA, subcmd);
    st = wait_ibe(); if (st != STATUS_SUCCESS) return st;

    io_out_byte(PORT_DATA, arg);
    st = wait_ibe(); if (st != STATUS_SUCCESS) return st;

    st = wait_obf(); if (st != STATUS_SUCCESS) return st;

    result = io_in_byte(PORT_DATA) & 0xFF;
    return STATUS_SUCCESS;
}

/// Execute one fan mailbox transaction.
///
/// @param in [0] = Subcommand: 0x61 set fan 1, 0x62 set fan 2, 0x63 query
///           [1] = Argument: fan speed percent (0-100) for the set subcommands,
///                 or 0x01 read fan 1, 0x02 read fan 2, 0x03 restore automatic
///                 mode for the query subcommand
/// @param in_size Must be 2
/// @param out [0] = Reply byte from the EC. For set and restore: 0xAC on
///            success, 0xFA if unsupported. For reads: the fan speed percent.
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_fan, 2, 1) {
    new subcmd = in[0] & 0xFF;
    new arg    = in[1] & 0xFF;

    if (subcmd != SUBCMD_SET_FAN1 && subcmd != SUBCMD_SET_FAN2 && subcmd != SUBCMD_QUERY)
        return STATUS_ACCESS_DENIED;

    if (subcmd == SUBCMD_QUERY) {
        if (arg < QUERY_MIN || arg > QUERY_MAX)
            return STATUS_INVALID_PARAMETER;
    } else {
        if (arg > 100)
            return STATUS_INVALID_PARAMETER;
    }

    new result = 0;
    new NTSTATUS:st = mbey(subcmd, arg, result);
    if (st != STATUS_SUCCESS)
        return st;

    out[0] = result;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    return STATUS_SUCCESS;
}
