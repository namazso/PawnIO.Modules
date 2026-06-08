//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026 Gen Li
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

// https://github.com/torvalds/linux/blob/master/drivers/hwmon/via-cputemp.c
#define MSR_ZX_VID		0x00000198
#define MSR_ZX_TEMP		0x00001423

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_ZX_VID, MSR_ZX_TEMP:
            return true;
        default:
            return false;
    }
    return false;
}

/// Read MSR.
///
/// @param in [0] = MSR
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_msr, 1, 1) {
    new msr = in[0] & 0xFFFFFFFF;

    if (!is_allowed_msr_read(msr))
        return STATUS_ACCESS_DENIED;

    new value = 0;
    new NTSTATUS:status = msr_read(msr, value);

    out[0] = value;

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_VIA)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    if (family != 7)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
