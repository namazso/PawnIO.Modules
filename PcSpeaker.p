//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  namazso <admin@namazso.eu>
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

// PC speaker, as driven by 8254 PIT counter 2 and the PPI port B latch.

const PORT_PIT_CH2 = 0x42;
const PORT_PIT_MODE = 0x43;
const PORT_PPI_B = 0x61;

/// PIT counter 2 gate.
const PPI_B_TIMER2_GATE = BIT(0);
/// Speaker data enable.
const PPI_B_SPEAKER_EN = BIT(1);
/// The only bits of port 0x61 this module will ever modify.
const PPI_B_WRITE_MASK = PPI_B_TIMER2_GATE | PPI_B_SPEAKER_EN;

// 8254 mode/command byte: bits 7:6 are the counter select field.
const PIT_SC_MASK = 0xC0;
const PIT_SC_CH2 = 0x80;
const PIT_SC_READBACK = 0xC0;

// Counter select bits of a read-back command, and its reserved bit.
const PIT_RB_RESERVED = BIT(0);
const PIT_RB_CNT0 = BIT(1);
const PIT_RB_CNT1 = BIT(2);
const PIT_RB_CNT2 = BIT(3);

// Command encoding, one command per cell:
//
//   63            56 55                                             0
//  +----------------+------------------------------------------------+
//  |     opcode     |                    payload                     |
//  +----------------+------------------------------------------------+
const CMD_OPCODE_SHIFT = 56;
const CMD_PAYLOAD_MASK = 0x00FFFFFFFFFFFFFF;

const OP_NOP = 0x00;
const OP_PIT_MODE = 0x01;
const OP_PIT_CH2_COUNT = 0x02;
const OP_PIT_CH2_WRITE = 0x03;
const OP_PIT_CH2_READ = 0x04;
const OP_PPI_WRITE = 0x05;
const OP_PPI_READ = 0x06;
const OP_STALL = 0x07;

const MAX_BATCH_COMMANDS = 64;

// The whole batch runs with interrupts disabled, so stalls are capped hard.
const MAX_STALL_PER_CMD_US = 50;
const MAX_STALL_PER_BATCH_US = 1000;

stock cmd_opcode(cmd) {
    return (cmd >>> CMD_OPCODE_SHIFT) & 0xFF;
}

stock cmd_payload(cmd) {
    return cmd & CMD_PAYLOAD_MASK;
}

/// Check whether a mode/command byte only affects PIT counter 2.
///
/// @param value Command byte destined for port 0x43
/// @return Whether the command may be issued
bool:is_pit_command_allowed(value) {
    // Mode set and counter latch for counter 2 share the same select field.
    if ((value & PIT_SC_MASK) == PIT_SC_CH2)
        return true;

    // Read-back is fine as long as counters 0 and 1 are left alone.
    if ((value & PIT_SC_MASK) == PIT_SC_READBACK)
        return (value & PIT_RB_CNT2) != 0
            && (value & (PIT_RB_CNT0 | PIT_RB_CNT1)) == 0
            && (value & PIT_RB_RESERVED) == 0;

    return false;
}

/// Validate a single command.
///
/// @param cmd The command cell
/// @param stall_budget Running total of requested stall time, updated in place
/// @return An NTSTATUS
NTSTATUS:validate_command(cmd, &stall_budget) {
    new op = cmd_opcode(cmd);
    new payload = cmd_payload(cmd);

    switch (op) {
        case OP_NOP, OP_PIT_CH2_COUNT, OP_PIT_CH2_WRITE, OP_PIT_CH2_READ, OP_PPI_WRITE, OP_PPI_READ: {
            // Payloads are masked down to the writable bits on execution, so
            // there is nothing left to reject.
        }
        case OP_PIT_MODE: {
            if (!is_pit_command_allowed(payload & 0xFF))
                return STATUS_ACCESS_DENIED;
        }
        case OP_STALL: {
            new us = payload & 0xFFFF;
            if (us > MAX_STALL_PER_CMD_US)
                return STATUS_INVALID_PARAMETER;
            stall_budget += us;
            if (stall_budget > MAX_STALL_PER_BATCH_US)
                return STATUS_INVALID_PARAMETER;
        }
        default: {
            return STATUS_INVALID_PARAMETER;
        }
    }

    return STATUS_SUCCESS;
}

/// Execute a single validated command.
///
/// @param cmd The command cell
/// @return The result cell for this command, 0 for commands that read nothing
execute_command(cmd) {
    new op = cmd_opcode(cmd);
    new payload = cmd_payload(cmd);

    switch (op) {
        case OP_PIT_MODE: {
            io_out_byte(PORT_PIT_MODE, payload & 0xFF);
        }
        case OP_PIT_CH2_COUNT: {
            io_out_byte(PORT_PIT_CH2, payload & 0xFF);
            io_out_byte(PORT_PIT_CH2, (payload >>> 8) & 0xFF);
        }
        case OP_PIT_CH2_WRITE: {
            io_out_byte(PORT_PIT_CH2, payload & 0xFF);
        }
        case OP_PIT_CH2_READ: {
            return io_in_byte(PORT_PIT_CH2);
        }
        case OP_PPI_WRITE: {
            new mask = (payload >>> 8) & PPI_B_WRITE_MASK;
            new value = payload & mask;
            new updated = (io_in_byte(PORT_PPI_B) & ~mask) | value;
            io_out_byte(PORT_PPI_B, updated);
            return updated;
        }
        case OP_PPI_READ: {
            return io_in_byte(PORT_PPI_B);
        }
        case OP_STALL: {
            microsleep2(payload & 0xFFFF);
        }
    }

    return 0;
}

/// Run a batch of speaker commands with interrupts disabled.
///
/// Each input cell is one command, `[63:56]` opcode and `[55:0]` payload:
///
/// | Opcode | Name           | Payload                  | Result             |
/// |--------|----------------|--------------------------|--------------------|
/// | `0x00` | NOP            | ignored                  | 0                  |
/// | `0x01` | PIT_MODE       | `[7:0]` command byte     | 0                  |
/// | `0x02` | PIT_CH2_COUNT  | `[15:0]` divisor         | 0                  |
/// | `0x03` | PIT_CH2_WRITE  | `[7:0]` byte             | 0                  |
/// | `0x04` | PIT_CH2_READ   | ignored                  | byte read          |
/// | `0x05` | PPI_WRITE      | `[15:8]` mask, `[7:0]` value | new port value |
/// | `0x06` | PPI_READ       | ignored                  | byte read          |
/// | `0x07` | STALL          | `[15:0]` microseconds    | 0                  |
///
/// `PIT_MODE` command bytes must select counter 2, either directly or as a
/// read-back naming counter 2 alone. `PIT_CH2_COUNT` writes the low byte then
/// the high byte; a divisor of 0 means 65536. `PPI_WRITE` is read-modify-write
/// and both mask and value are reduced to bits 0 and 1 first.
///
/// The batch is fully validated before any of it executes, so an invalid
/// command cannot leave the speaker half configured. Stalls are capped at
/// 50us each and 1000us per batch because interrupts are off throughout.
///
/// A HalMakeBeep equivalent for frequency F is four commands, where the
/// divisor is 1193167 / F:
///
///     0x0500_0000_0000_0300   // silence: clear gate and data
///     0x0100_0000_0000_00B6   // counter 2, lo/hi, mode 3, binary
///     0x0200_0000_0000_0000 | divisor
///     0x0500_0000_0000_0303   // set gate and data
///
/// and silencing it again is the first command on its own.
///
/// @param in One command per cell
/// @param in_size Command count, 1 to 64
/// @param out One result per command, in order
/// @param out_size Must be 0 to discard results, otherwise at least in_size
/// @return An NTSTATUS
DEFINE_IOCTL(ioctl_batch) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (in_size > MAX_BATCH_COMMANDS)
        return STATUS_INVALID_PARAMETER;
    if (out_size != 0 && out_size < in_size)
        return STATUS_BUFFER_TOO_SMALL;

    // Validate up front.
    new stall_budget = 0;
    for (new i = 0; i < in_size; i++) {
        new NTSTATUS:status = validate_command(in[i], stall_budget);
        if (!NT_SUCCESS(status)) {
            debug_print(''PcSpeaker: command %d rejected\n'', i);
            return status;
        }
    }

    new results[MAX_BATCH_COMMANDS];

    interrupts_disable();

    for (new i = 0; i < in_size; i++)
        results[i] = execute_command(in[i]);

    interrupts_enable();

    if (out_size != 0)
        copy(out, results, in_size, 0, 0, out_size);

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // Resolve KeStallExecutionProcessor now. OP_STALL runs with interrupts
    // disabled, which is no place for a symbol lookup.
    microsleep2(0);

    return STATUS_SUCCESS;
}
