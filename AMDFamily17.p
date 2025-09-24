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
//  Lesser General Public License for more detaiNTSTATUS:ls.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  SPDX-License-Identifier: LGPL-2.1-or-later

#include <pawnio.inc>

#define MSR_AMD64_PATCH_LEVEL       (0x0000008B)
#define MSR_CORE_ENERGY_STAT        (0xC001029A)
#define MSR_HARDWARE_PSTATE_STATUS  (0xC0010293)
#define MSR_PKG_ENERGY_STAT         (0xC001029B)
#define MSR_PSTATE_STATUS           (0xC0010063)
#define MSR_PSTATE_0                (0xC0010064)
#define MSR_PSTATE_1                (0xC0010065)
#define MSR_PSTATE_2                (0xC0010066)
#define MSR_PSTATE_3                (0xC0010067)
#define MSR_PSTATE_4                (0xC0010068)
#define MSR_PSTATE_5                (0xC0010069)
#define MSR_PSTATE_6                (0xC001006A)
#define MSR_PSTATE_7                (0xC001006B)
#define MSR_PWR_UNIT                (0xC0010299)
#define MSR_MPERF_RO                (0xC00000E7)
#define MSR_APERF_RO                (0xC00000E8)
#define MSR_K7_EVNTSEL0             (0xc0010000)
#define MSR_K7_PERFCTR0             (0xc0010004)
#define MSR_K7_HWCR                 (0xC0010015)
#define MSR_K10_COFVID_STATUS       (0xC0010071)

#define SMN_INDEX_OFFSET	(0x60)
#define SMN_DATA_OFFSET		(0x64)

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_AMD64_PATCH_LEVEL, MSR_CORE_ENERGY_STAT, MSR_HARDWARE_PSTATE_STATUS,
             MSR_PKG_ENERGY_STAT, MSR_PSTATE_STATUS, MSR_PSTATE_0, MSR_PSTATE_1,
             MSR_PSTATE_2, MSR_PSTATE_3, MSR_PSTATE_4, MSR_PSTATE_5, MSR_PSTATE_6,
             MSR_PSTATE_7, MSR_PWR_UNIT, MSR_MPERF_RO, MSR_APERF_RO,MSR_K7_EVNTSEL0,
             MSR_K7_PERFCTR0, MSR_K7_HWCR, MSR_K10_COFVID_STATUS:
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

/// Read SMN.
///
/// @param in [0] = Offset
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_read_smn, 1, 1) {
    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;
    if ((didvid & 0xFFFF) != 0x1022)
        return STATUS_NOT_SUPPORTED;

    status = pci_config_write_dword(0, 0, 0, SMN_INDEX_OFFSET, in[0]);
    if (!NT_SUCCESS(status))
        return status;
    status = pci_config_read_dword(0, 0, 0, SMN_DATA_OFFSET, out[0]);
    if (!NT_SUCCESS(status))
        return status;
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    debug_print(''AMDFamily17: family: %x model: %x\n'', family, model);

    if (family < 0x17 || family > 0x1A)
        return STATUS_NOT_SUPPORTED;
    
    return STATUS_SUCCESS;
}
