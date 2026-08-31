//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Gen Li
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

// PawnIO Intel MCHBAR Driver

#define PCI_VENDOR_ID_INTEL 0x8086

#define MCHBAR_BASE_REG_LOW     0x48
#define MCHBAR_BASE_REG_HIGH    0x4C

#define MCHBAREN        0x01

new g_mchbar_addr = 0;
new g_mchbar_size = 0;
new VA:g_mchbar_va = NULL;

// https://github.com/torvalds/linux/blob/master/arch/x86/include/asm/intel-family.h
const CodeName: {
    CPU_UNKNOWN = -1,
    CPU_SANDYBRIDGE,
    CPU_SANDYBRIDGE_X,
    CPU_IVYBRIDGE,
    CPU_IVYBRIDGE_X,
    CPU_HASWELL,
    CPU_HASWELL_X,
    CPU_HASWELL_L,
    CPU_HASWELL_G,
    CPU_BROADWELL,
    CPU_BROADWELL_G,
    CPU_BROADWELL_X,
    CPU_BROADWELL_D,
    CPU_SKYLAKE_L,
    CPU_SKYLAKE,
    CPU_SKYLAKE_X,
    CPU_KABYLAKE_L,
    CPU_KABYLAKE,
    CPU_COMETLAKE,
    CPU_COMETLAKE_L,
    CPU_CANNONLAKE_L,
    CPU_ICELAKE_X,
    CPU_ICELAKE_D,
    CPU_ICELAKE,
    CPU_ICELAKE_L,
    CPU_ICELAKE_NNPI,
    CPU_ROCKETLAKE,
    CPU_TIGERLAKE_L,
    CPU_TIGERLAKE,
    CPU_ALDERLAKE,
    CPU_ALDERLAKE_L,
    CPU_RAPTORLAKE,
    CPU_RAPTORLAKE_P,
    CPU_RAPTORLAKE_S,
    CPU_METEORLAKE,
    CPU_METEORLAKE_L,
    CPU_ARROWLAKE_H,
    CPU_ARROWLAKE,
    CPU_ARROWLAKE_U,
    CPU_LUNARLAKE_M,
    CPU_PANTHERLAKE_L,
    CPU_WILDCATLAKE_L,
    CPU_NOVALAKE,
    CPU_NOVALAKE_L,
};

CodeName:get_code_name(family, model) {
    switch ((family << 8) | model) {
        case 0x062A:
            return CPU_SANDYBRIDGE;
        case 0x062D:
            return CPU_SANDYBRIDGE_X;
        case 0x063A:
            return CPU_IVYBRIDGE;
        case 0x063E:
            return CPU_IVYBRIDGE_X;
        case 0x063C:
            return CPU_HASWELL;
        case 0x063F:
            return CPU_HASWELL_X;
        case 0x0645:
            return CPU_HASWELL_L;
        case 0x0646:
            return CPU_HASWELL_G;
        case 0x063D:
            return CPU_BROADWELL;
        case 0x0647:
            return CPU_BROADWELL_G;
        case 0x064F:
            return CPU_BROADWELL_X;
        case 0x0656:
            return CPU_BROADWELL_D;
        case 0x064E:
            return CPU_SKYLAKE_L;
        case 0x065E:
            return CPU_SKYLAKE;
        case 0x0655:
            return CPU_SKYLAKE_X;
        case 0x068E:
            return CPU_KABYLAKE_L;
        case 0x069E:
            return CPU_KABYLAKE;
        case 0x06A5:
            return CPU_COMETLAKE;
        case 0x06A6:
            return CPU_COMETLAKE_L;
        case 0x0666:
            return CPU_CANNONLAKE_L;
        case 0x066A:
            return CPU_ICELAKE_X;
        case 0x066C:
            return CPU_ICELAKE_D;
        case 0x067D:
            return CPU_ICELAKE;
        case 0x067E:
            return CPU_ICELAKE_L;
        case 0x069D:
            return CPU_ICELAKE_NNPI;
        case 0x06A7:
            return CPU_ROCKETLAKE;
        case 0x068C:
            return CPU_TIGERLAKE_L;
        case 0x068D:
            return CPU_TIGERLAKE;
        case 0x0697:
            return CPU_ALDERLAKE;
        case 0x069A:
            return CPU_ALDERLAKE_L;
        case 0x06B7:
            return CPU_RAPTORLAKE;
        case 0x06BA:
            return CPU_RAPTORLAKE_P;
        case 0x06BF:
            return CPU_RAPTORLAKE_S;
        case 0x06AC:
            return CPU_METEORLAKE;
        case 0x06AA:
            return CPU_METEORLAKE_L;
        case 0x06C5:
            return CPU_ARROWLAKE_H;
        case 0x06C6:
            return CPU_ARROWLAKE;
        case 0x06B5:
            return CPU_ARROWLAKE_U;
        case 0x06BD:
            return CPU_LUNARLAKE_M;
        case 0x06CC:
            return CPU_PANTHERLAKE_L;
        case 0x06D5:
            return CPU_WILDCATLAKE_L;
        case 0x1801:
            return CPU_NOVALAKE;
        case 0x1803:
            return CPU_NOVALAKE_L;

        default:
            return CPU_UNKNOWN;
    }
    return CPU_UNKNOWN;
}

NTSTATUS:mchbar_init(CodeName:code_name) {
    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;
    if (didvid & 0xFFFF != PCI_VENDOR_ID_INTEL)
        return STATUS_NOT_SUPPORTED;

    new base_lo = 0;
    new base_hi = 0;

    status = pci_config_read_dword(0, 0, 0, MCHBAR_BASE_REG_LOW, base_lo);
    if (!NT_SUCCESS(status))
        return status;
    if (!(base_lo & MCHBAREN))
        return STATUS_NOT_SUPPORTED;
    status = pci_config_read_dword(0, 0, 0, MCHBAR_BASE_REG_HIGH, base_hi);
    if (!NT_SUCCESS(status))
        return status;

    g_mchbar_addr = ((base_hi & 0xFFFFFFFF) << 32) | (base_lo & 0xFFFFFFFF);
    g_mchbar_addr &= 0x3FFFFFF8000;
    if (g_mchbar_addr == 0)
        return STATUS_NOT_SUPPORTED;

    if (code_name >= CPU_TIGERLAKE_L)
        g_mchbar_size = 0x20000; // 128KB
    else if (code_name >= CPU_ICELAKE_X)
        g_mchbar_size = 0x10000; // 64KB
    else
        g_mchbar_size = 0x8000;  // 32KB

    g_mchbar_va = io_space_map(g_mchbar_addr, g_mchbar_size);
    if (g_mchbar_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    return STATUS_SUCCESS;
}

/// Read a dword from mchbar.
///
/// @param in [0] = offset
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_dword, 1, 1) {
    new offset = in[0];

    if (offset < 0 || offset >= g_mchbar_size)
        return STATUS_ACCESS_DENIED;
    if (offset & 0x3)
        return STATUS_ACCESS_DENIED;

    new value = 0;
    new NTSTATUS:status = virtual_read_dword(g_mchbar_va + offset, value);

    out[0] = value;
    return status;
}

/// Read a qword from mchbar.
///
/// @param in [0] = offset
/// @param in_size Must be 1
/// @param out [0] = Value read
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_qword, 1, 1) {
    new offset = in[0];

    if (offset < 0 || offset >= g_mchbar_size)
        return STATUS_ACCESS_DENIED;
    if (offset & 0x7)
        return STATUS_ACCESS_DENIED;

    new value = 0;
    new NTSTATUS:status = virtual_read_qword(g_mchbar_va + offset, value);

    out[0] = value;
    return status;
}

/// Get MCHBAR address
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Address
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_mchbar_addr, 0, 1) {
    out[0] = _:g_mchbar_addr;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    debug_print(''IntelMCHBAR: family: %x model: %x\n'', family, model);

    new CodeName:code_name = get_code_name(family, model);
    if (code_name == CPU_UNKNOWN)
        return STATUS_NOT_SUPPORTED;

    return mchbar_init(code_name);
}

public NTSTATUS:unload() {
    if (g_mchbar_va != NULL) {
        io_space_unmap(g_mchbar_va, g_mchbar_size);
        g_mchbar_va = NULL;
    }
    return STATUS_SUCCESS;
}
