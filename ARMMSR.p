//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  namazso <admin@namazso.eu>
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

#define MSR_MIDR_EL1 0xD5380000

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_MIDR_EL1:
            return true;
        default:
            return false;
    }
    return false;
}

/// Read MSR.
///
/// @param in [0] = MSR reading instruction bytes
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
forward NTSTATUS:ioctl_read_msr(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_read_msr(in[], in_size, out[], out_size) {
    if (in_size != 1 || out_size != 1)
        return STATUS_INVALID_PARAMETER;

    new msr = in[0] & 0xFFFFFFFF;

    if (!is_allowed_msr_read(msr))
        return STATUS_ACCESS_DENIED;
        
    new value = 0;
    new NTSTATUS:status = msr_read(msr, value);

    out[0] = value;

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_A64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
