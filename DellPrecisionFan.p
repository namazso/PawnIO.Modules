//  DellPrecisionFan - PawnIO module for per-fan control on Dell Precision 7820/7920 Tower.
//  Copyright (C) 2026
//  SPDX-License-Identifier: LGPL-2.1-or-later
//
//  Exposes the embedded-controller (Microchip MEC5105) host mailbox at I/O ports
//  0x910 (index/command) / 0x911 (data/status) to run ONLY the two fan-related
//  EC commands, so per-fan reads and overrides work from the OS under HVCI
//  (Memory Integrity) without a raw port-I/O driver such as WinRing0.
//
//  Protocol reverse-engineered from "7820T Embedded Controller v1.1.28"
//  (host ISR @d6464, cmd table @0xE0060). One transaction:
//    - wait until mailbox reg 0 (HOST_TO_EC) and reg 0x10 are both clear (idle),
//    - write up to 5 params to regs 0x12..0x16,
//    - write the command to reg 0, the EC consumes it and clears reg 0/0x10,
//    - read the response from regs 0x11..0x16.
//  This mirrors exactly the live-verified sequence in D:\bioss\tools\fan2e.ps1
//  (EcIoSmm fn 0x8c4 in BIOS 2.50.0 uses the same reg layout).
//
//  Allowed commands (everything fan2e needs; any other cmd is rejected):
//    0x10  read fan fault/absent bitmap
//    0x2E  per-fan: sub 0 status, 1 SET, 2 read RPM, 3 presence, 4 limits
//          (SET sub 1: mode 0x83 = target RPM, mode 3 = clear override/auto)
//
//  SAFETY: a 0x2E "sub 1" set suspends the EC's automatic thermal policy for ALL
//  fans until cleared (sub 1 / mode 3). This module is stateless; the caller
//  (fanpio.ps1) owns the watchdog and the always-restore-on-exit guarantee.

#include <pawnio.inc>

#define MBX_IDX   (0x910)   // EC host mailbox index / command port
#define MBX_DATA  (0x911)   // EC host mailbox data / status port

#define EC_CMD_FAULT  (0x10)
#define EC_CMD_FAN    (0x2E)

#define WAIT_ITERS    (20000)  // x 100us = 2s worst-case handshake cap
#define WAIT_STEP_US  (100)

// Read one mailbox register: select index on 0x910, read value on 0x911.
mbx_rd(idx) {
    io_out_byte(MBX_IDX, idx & 0xff);
    return io_in_byte(MBX_DATA) & 0xff;
}

// Write one mailbox register.
mbx_wr(idx, val) {
    io_out_byte(MBX_IDX, idx & 0xff);
    io_out_byte(MBX_DATA, val & 0xff);
}

// Block until the EC mailbox is idle (reg 0 and reg 0x10 both 0), or time out.
NTSTATUS:ec_wait_idle() {
    for (new i = 0; i < WAIT_ITERS; i++) {
        if (mbx_rd(0x00) == 0 && mbx_rd(0x10) == 0)
            return STATUS_SUCCESS;
        microsleep(WAIT_STEP_US);
    }
    return STATUS_IO_TIMEOUT;
}

// One full EC transaction. params[5] -> regs 0x12..0x16; cmd -> reg 0;
// response regs 0x11..0x16 -> resp[6].
NTSTATUS:ec_txn(cmd, const params[5], resp[6]) {
    new NTSTATUS:st = ec_wait_idle();
    if (st != STATUS_SUCCESS)
        return st;

    for (new k = 0; k < 5; k++)
        mbx_wr(0x12 + k, params[k]);

    mbx_wr(0x00, cmd & 0xff);

    st = ec_wait_idle();
    if (st != STATUS_SUCCESS)
        return st;

    for (new k = 0; k < 6; k++)
        resp[k] = mbx_rd(0x11 + k);

    return STATUS_SUCCESS;
}

/// Run one fan EC command.
///
/// @param in  [0] = EC command (must be 0x10 or 0x2E)
///            [1] = param0 -> reg 0x12 (0x2E: sub-op)
///            [2] = param1 -> reg 0x13 (0x2E: fan index 0..8)
///            [3] = param2 -> reg 0x14 (0x2E: mode)
///            [4] = param3 -> reg 0x15 (0x2E: value low byte)
///            [5] = param4 -> reg 0x16 (0x2E: value high byte)
/// @param out [0..5] = response regs 0x11..0x16
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_fan_cmd, 6, 6) {
    new cmd = in[0] & 0xff;
    if (cmd != EC_CMD_FAULT && cmd != EC_CMD_FAN)
        return STATUS_INVALID_PARAMETER;

    new params[5];
    for (new k = 0; k < 5; k++)
        params[k] = in[1 + k];

    new resp[6];
    new NTSTATUS:st = ec_txn(cmd, params, resp);
    if (st != STATUS_SUCCESS)
        return st;

    for (new k = 0; k < 6; k++)
        out[k] = resp[k];

    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    new CPUArch:arch = get_arch();
    debug_print("DellPrecisionFan loaded (arch %d)", _:arch);
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    debug_print("DellPrecisionFan unloaded");
    return STATUS_SUCCESS;
}
