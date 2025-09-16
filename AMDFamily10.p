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

#define PCI_BUS 0
#define PCI_BASE_DEVICE 24
#define PCI_MISCCTL_FUNCTION 3

#define AMD_VID 0x1022

#define MSR_K7_HWCR			0xc0010015

#define MSR_K7_EVNTSEL0			0xc0010000
#define MSR_K7_PERFCTR0			0xc0010004

#define MSR_K10_COFVID_STATUS 0xc0010071

#define MSR_AMD_PSTATE_DEF_BASE		0xc0010064

#define CSTATES_IO_PORT 0xCD6

#define MISCCTL_CLOCK_POWER_TIMING_CTL_0 0xD4
#define MISCCTL_REPORTED_TEMPERATURE_CTL 0xA4

#define SMU_REPORTED_TEMP_CTRL_OFFSET 0xD8200CA4

new g_family;
new g_model;

new g_cstates_io_offset;

resolve_cstates_io_offset() {
    // we only read from PCI so no need for locking

    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0, 20, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return;
    if (didvid == 0x780B1022 || didvid == 0x790B1022) {
        g_cstates_io_offset = 0x9C;
        return;
    } else if (didvid == 0x43851002) {
        new rev;
        status = pci_config_read_dword(0, 20, 0, 0, rev);
        if (!NT_SUCCESS(status))
            return;
        g_cstates_io_offset = rev & 0xFF < 0x40 ? 0xB3 : 0x9C;
        return;
    }
}

/// Get CState residency.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0..1] = CState residency of C0 and C1 respectively
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @warning This should probably have an associated mutant? But LHM doesn't have one...
DEFINE_IOCTL_SIZED(ioctl_read_cstate_residency, 0, 2) {
    if (g_cstates_io_offset == 0)
        return STATUS_NOT_SUPPORTED;
    for (new i = 0; i < 2; ++i) {
        io_out_byte(CSTATES_IO_PORT, g_cstates_io_offset + i);
        out[i] = io_in_byte(CSTATES_IO_PORT + 1);
    }
    return STATUS_SUCCESS;
}

bool:is_allowed_miscctl_offset(didvid, offset) {
    switch (didvid) {
        case 0x12031022, 0x13031022, 0x17031022, 0x16031022, 0x14031022,
            0x141D1022, 0x15731022, 0x15B31022, 0x15331022, 0x15831022:
            return offset == 0 || offset == MISCCTL_CLOCK_POWER_TIMING_CTL_0 || offset == MISCCTL_REPORTED_TEMPERATURE_CTL;
        default:
            return false;
    }
    return false;
}

/// Read from a MISCCTL offset.
///
/// @param in [0] = CPU index, [1] = Offset
/// @param in_size Must be 2
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_miscctl, 2, 1) {
    new cpu_idx = in[0];
    new offset = in[1];

    new didvid;
    new NTSTATUS:status = pci_config_read_dword(PCI_BUS, PCI_BASE_DEVICE + cpu_idx, PCI_MISCCTL_FUNCTION, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;

    if (!is_allowed_miscctl_offset(didvid, offset))
        return STATUS_ACCESS_DENIED;
    
    new value;
    status = pci_config_read_dword(PCI_BUS, PCI_BASE_DEVICE + cpu_idx, PCI_MISCCTL_FUNCTION, offset, value);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = value;

    return STATUS_SUCCESS;
}

/// Read from SMU.
///
/// @param in [0] = Offset
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_read_smu, 1, 1) {
    new model_group = g_model & 0xF0;
    if (g_family != 0x15 || (model_group != 0x60 && model_group != 0x70))
        return STATUS_NOT_SUPPORTED;

    new offset = in[0];

    if (offset != SMU_REPORTED_TEMP_CTRL_OFFSET)
        return STATUS_ACCESS_DENIED;

    new NTSTATUS:status = pci_config_write_dword(PCI_BUS, 0, 0, 0xB8, offset);
    if (!NT_SUCCESS(status))
        return status;
    
    new value;
    status = pci_config_read_dword(PCI_BUS, 0, 0, 0xBC, value);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = value;
    
    return STATUS_SUCCESS;
}

/// Measure TSC multiplier.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Performance event count during a tick, [1] = COFVID status
/// @param out_size Must be 2
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_measure_tsc_multiplier, 0, 2) {
    new ras[4];
    cpuid(0x80000007, 0, ras);

    // core performance boost
    new has_cpb = !!((ras[3] >>> 9) & 1);

    interrupts_disable();

    // unset CPB
    new hwcr;
    msr_read(MSR_K7_HWCR, hwcr);
    if (has_cpb)
        msr_write(MSR_K7_HWCR, hwcr | (1 << 25));

    // Back up EVNTSEL0 and PERFCTR0
    new old_eventsel0, old_perfctr0;
    msr_read(MSR_K7_EVNTSEL0, old_eventsel0);
    msr_read(MSR_K7_PERFCTR0, old_perfctr0);

    // start counting kernel events
    msr_write(MSR_K7_EVNTSEL0, (1 << 22) | (1 << 16) | 0x76);
    msr_write(MSR_K7_PERFCTR0, 0);

    new tick_old = get_tick_count();
    new tick_start = tick_old + 1;
    new tick_end = tick_start + 1;

    // wait for next tick
    while (get_tick_count() < tick_start) {}

    // read current event ctr
    new ctr_start;
    msr_read(MSR_K7_PERFCTR0, ctr_start);
    
    // wait for next tick
    while (get_tick_count() < tick_end) {}
    
    // read current event ctr
    new ctr_end;
    msr_read(MSR_K7_PERFCTR0, ctr_end);

    // read COFVID status
    new cofvid;
    msr_read(MSR_K10_COFVID_STATUS, cofvid);

    // restore old EVENTSEL0 / PERFCTR0
    msr_write(MSR_K7_EVNTSEL0, old_eventsel0);
    msr_write(MSR_K7_PERFCTR0, old_perfctr0);

    // restore old HWCR
    msr_write(MSR_K7_HWCR, hwcr);

    interrupts_enable();

    // usermode can figure out the stats from these two
    out[0] = ctr_end - ctr_start;
    out[1] = cofvid;

    return STATUS_SUCCESS;
}

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_K10_COFVID_STATUS:
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

    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    debug_print(''AMDFamily10: family: %x model: %x\n'', family, model);

    if (family < 0x10 || family > 0x16)
        return STATUS_NOT_SUPPORTED;

    g_family = family;
    g_model = model;
    
    resolve_cstates_io_offset();

    return STATUS_SUCCESS;
}
