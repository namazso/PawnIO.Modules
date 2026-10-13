//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Daniel Clark
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

#define GIGABYTE_ID_SIV        0
#define GIGABYTE_ID_LID        1

#define GIGABYTE_SMI_SIV       0xB364
#define GIGABYTE_SMI_LID       0xB464
#define GIGABYTE_SMI_PORT      0xB2

/// Query a Gigabyte motherboard firmware ID.
///
/// @param type GIGABYTE_ID_SIV or GIGABYTE_ID_LID
/// @param value Returned firmware ID
/// @return An NTSTATUS
NTSTATUS:gigabyte_get_id(type, &value)
{
    // SMI register layout:
    //  0 = RAX
    //  1 = RCX
    //  2 = RDX
    //  3 = RBX
    //  4 = unused
    //  5 = RBP
    //  6 = RSI
    //  7 = RDI
    //  8 = R8
    //  9 = R9
    // 10 = R10
    // 11 = R11
    // 12 = R12
    // 13 = R13
    // 14 = R14
    // 15 = R15
    new regs[16];

    for (new i = 0; i < 16; i++)
        regs[i] = 0;

    if (type == GIGABYTE_ID_SIV)
        regs[0] = GIGABYTE_SMI_SIV;
    else if (type == GIGABYTE_ID_LID)
        regs[0] = GIGABYTE_SMI_LID;
    else
        return STATUS_INVALID_PARAMETER;

    // Gigabyte's firmware interface expects RDX = 0xB2.
    regs[2] = GIGABYTE_SMI_PORT;

    // Preserve the current RFLAGS value.
    smi(regs, -1, 0);

    // Firmware returns status in RAX and the ID in RBX.
    if (regs[0] != 0)
        return STATUS_UNSUCCESSFUL;

    value = regs[3];

    return STATUS_SUCCESS;
}

/// Query a Gigabyte SIV or LID.
///
/// Input:
///   0 = SIV
///   1 = LID
///
/// Output:
///   Firmware ID
DEFINE_IOCTL_SIZED(ioctl_get_id, 1, 1)
{
    return gigabyte_get_id(in[0], out[0]);
}

NTSTATUS:main()
{
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
