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

#define AMD_VID 0x1022

#define MISCELLANEOUS_CONTROL_FUNCTION 3
#define MISCELLANEOUS_CONTROL_DID 0x1103

#define THERMTRIP_STATUS_REGISTER 0xE4

#define MSR_K7_FID_VID_STATUS		0xc0010042

new g_model;

bool:is_allowed_msr_read(msr) {
    switch (msr) {
        case MSR_K7_FID_VID_STATUS:
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

/// Read THERMTRIP status register.
///
/// @param in [0] = CPU index, [1] = Core index
/// @param in_size Must be 2
/// @param out THERMTRIP status register
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
forward NTSTATUS:ioctl_get_thermtrip(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_get_thermtrip(in[], in_size, out[], out_size) {
    if (in_size < 2 || out_size < 1)
        return STATUS_INVALID_PARAMETER;

    new cpu_idx = in[0];
    if (cpu_idx > 1)
        return STATUS_INVALID_PARAMETER;
    new core_idx = in[1];
    if (core_idx > 1)
        return STATUS_INVALID_PARAMETER;

    new device = PCI_BASE_DEVICE + cpu_idx;
    
    new didvid;
    new NTSTATUS:status = pci_config_read_dword(PCI_BUS, device, MISCELLANEOUS_CONTROL_FUNCTION, 0, didvid);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;
    
    if ((didvid & 0xFFFF) != AMD_VID)
        return STATUS_NOT_SUPPORTED;
    
    if ((didvid >> 16) != MISCELLANEOUS_CONTROL_DID)
        return STATUS_NOT_SUPPORTED;
    
    new sel_cpu = g_model < 40 ? (core_idx ? 4 : 0) : (core_idx ? 0 : 4);
    status = pci_config_write_dword(PCI_BUS, device, MISCELLANEOUS_CONTROL_FUNCTION, THERMTRIP_STATUS_REGISTER, sel_cpu);
    if (!NT_SUCCESS(status))
        return status;
    
    new thermtrip;
    status = pci_config_read_dword(PCI_BUS, device, MISCELLANEOUS_CONTROL_FUNCTION, THERMTRIP_STATUS_REGISTER, thermtrip);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = thermtrip;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
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

    debug_print(''AMDFamily0F: family: %x model: %x pkg_type: %x\n'', family, model, pkg_type);

    if (family != 0x0F)
        return STATUS_NOT_SUPPORTED;

    g_model = model;

    return STATUS_SUCCESS;
}
