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

#define MSR_IA32_PACKAGE_THERM_STATUS		0x000001b1
#define MSR_IA32_PERF_STATUS		0x00000198
#define MSR_IA32_TEMPERATURE_TARGET	0x000001a2
#define MSR_IA32_THERM_STATUS		0x0000019c

#define MSR_PKG_POWER_LIMIT		0x00000610
#define MSR_PKG_ENERGY_STATUS		0x00000611
#define MSR_PKG_PERF_STATUS		0x00000613
#define MSR_PKG_POWER_INFO		0x00000614

#define MSR_DRAM_POWER_LIMIT		0x00000618
#define MSR_DRAM_ENERGY_STATUS		0x00000619
#define MSR_DRAM_PERF_STATUS		0x0000061b
#define MSR_DRAM_POWER_INFO		0x0000061c

#define MSR_PP0_POWER_LIMIT		0x00000638
#define MSR_PP0_ENERGY_STATUS		0x00000639
#define MSR_PP0_POLICY			0x0000063a
#define MSR_PP0_PERF_STATUS		0x0000063b

#define MSR_PP1_POWER_LIMIT		0x00000640
#define MSR_PP1_ENERGY_STATUS		0x00000641
#define MSR_PP1_POLICY			0x00000642

#define MSR_PLATFORM_ENERGY_STATUS	0x0000064D

#define MSR_RAPL_POWER_UNIT		0x00000606

#define MSR_PLATFORM_INFO		0x000000ce

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_IA32_PACKAGE_THERM_STATUS, MSR_IA32_PERF_STATUS, MSR_IA32_TEMPERATURE_TARGET, MSR_IA32_THERM_STATUS,
            MSR_PKG_POWER_LIMIT, MSR_PKG_ENERGY_STATUS, MSR_PKG_PERF_STATUS, MSR_PKG_POWER_INFO,
            MSR_DRAM_POWER_LIMIT, MSR_DRAM_ENERGY_STATUS, MSR_DRAM_PERF_STATUS, MSR_DRAM_POWER_INFO,
            MSR_PP0_POWER_LIMIT, MSR_PP0_ENERGY_STATUS, MSR_PP0_POLICY, MSR_PP0_PERF_STATUS,
            MSR_PP1_POWER_LIMIT, MSR_PP1_ENERGY_STATUS, MSR_PP1_POLICY,
            MSR_PLATFORM_ENERGY_STATUS, MSR_RAPL_POWER_UNIT, MSR_PLATFORM_INFO:
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
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    new vendor[4];
    cpuid(0, 0, vendor);
    if (!is_intel(vendor))
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
