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
    new status = pci_config_read_dword(0, 20, 0, 0, didvid);
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

// This should probably have an associated mutant?
forward ioctl_read_cstate_residency(in[], in_size, out[], out_size);
public ioctl_read_cstate_residency(in[], in_size, out[], out_size) {
    if (in_size != 0 || out_size != 2)
        return STATUS_INVALID_PARAMETER;
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

forward ioctl_read_miscctl(in[], in_size, out[], out_size);
public ioctl_read_miscctl(in[], in_size, out[], out_size) {
    if (in_size != 2 || out_size != 1)
        return STATUS_INVALID_PARAMETER;

    new cpu_idx = in[0];
    new offset = in[1];

    new didvid;
    new status = pci_config_read_dword(PCI_BUS, PCI_BASE_DEVICE + cpu_idx, PCI_MISCCTL_FUNCTION, 0, didvid);
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

/// WARNING: You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
forward ioctl_read_smu(in[], in_size, out[], out_size);
public ioctl_read_smu(in[], in_size, out[], out_size) {
    if (in_size != 1 || out_size != 1)
        return STATUS_INVALID_PARAMETER;
    
    new model_group = g_model & 0xF0;
    if (g_family != 0x15 || (model_group != 0x60 && model_group != 0x70))
        return STATUS_NOT_SUPPORTED;

    new offset = in[0];

    if (offset != SMU_REPORTED_TEMP_CTRL_OFFSET)
        return STATUS_ACCESS_DENIED;

    new status = pci_config_write_dword(PCI_BUS, 0, 0, 0xB8, offset);
    if (!NT_SUCCESS(status))
        return status;
    
    new value;
    status = pci_config_read_dword(PCI_BUS, 0, 0, 0xBC, value);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = value;
    
    return STATUS_SUCCESS;
}

// KUSER_SHARED_DATA->TickCountQuad
get_tick_count() {
    new value;
    virtual_read_qword(0xFFFFF78000000000 + 0x0320, value);
    return value;
}

forward ioctl_measure_tsc_multiplier(in[], in_size, out[], out_size);
public ioctl_measure_tsc_multiplier(in[], in_size, out[], out_size) {
    if (out_size != 2)
        return STATUS_INVALID_PARAMETER;

    new ras[4];
    cpuid(0x80000007, 0, ras);

    // core performance boost
    new has_cpb = !!((ras[3] >> 9) & 1);

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
    while (get_tick_count() != tick_start) {}

    // read current event ctr
    new ctr_start;
    msr_read(MSR_K7_PERFCTR0, ctr_start);
    
    // wait for next tick
    while (get_tick_count() != tick_end) {}
    
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

forward ioctl_read_msr(in[], in_size, out[], out_size);
public ioctl_read_msr(in[], in_size, out[], out_size) {
    if (in_size != 1 || out_size != 1)
        return STATUS_INVALID_PARAMETER;

    new msr = in[0] & 0xFFFFFFFF;

    if (!is_allowed_msr_read(msr))
        return STATUS_ACCESS_DENIED;
        
    new value = 0;
    new status = msr_read(msr, value);

    out[0] = value;

    return status;
}

main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    new vendor[4];
    cpuid(0, 0, vendor);
    if (!is_amd(vendor))
        return STATUS_NOT_SUPPORTED;

    new procinfo[4];
    cpuid(1, 0, procinfo);

    new extended[4];
    cpuid(0x80000001, 0, extended);

    new family = ((procinfo[0] & 0x0FF00000) >> 20) + ((procinfo[0] & 0x0F00) >> 8);
    new model = ((procinfo[0] & 0x0F0000) >> 12) + ((procinfo[0] & 0xF0) >> 4);
    //new stepping = procinfo[0] & 0x0F;
    new pkg_type = (extended[1] >> 28) & 0xFF;

    debug_print(''AMDFamily10: family: %x model: %x pkg_type: %x\n'', family, model, pkg_type);

    if (family < 0x10 || family > 0x16)
        return STATUS_NOT_SUPPORTED;

    g_family = family;
    g_model = model;
    
    resolve_cstates_io_offset();

    return STATUS_SUCCESS;
}
