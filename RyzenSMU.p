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

const CodeName: {
    CPU_Undefined = -1,
    CPU_Colfax,
    CPU_Renoir,
    CPU_Picasso,
    CPU_Matisse,
    CPU_Threadripper,
    CPU_CastlePeak,
    CPU_RavenRidge,
    CPU_RavenRidge2,
    CPU_SummitRidge,
    CPU_PinnacleRidge,
    CPU_Rembrandt,
    CPU_Vermeer,
    CPU_Vangogh,
    CPU_Cezanne,
    CPU_Milan,
    CPU_Dali,
    CPU_Raphael,
    CPU_GraniteRidge,
    CPU_Naples,
    CPU_FireFlight,
    CPU_Rome,
    CPU_Chagall,
    CPU_Lucienne,
    CPU_Phoenix,
    CPU_Phoenix2,
    CPU_Mendocino,
    CPU_Genoa,
    CPU_StormPeak,
    CPU_DragonRange,
    CPU_Mero,
    CPU_HawkPoint,
    CPU_StrixPoint,
    CPU_StrixHalo,
    CPU_KrackanPoint,
    CPU_KrackanPoint2,
    CPU_Turin,
    CPU_TurinD,
    CPU_Bergamo,
    CPU_ShimadaPeak,
};

const SMUStatus: {
    SMU_Busy = 0x00,
    SMU_OK = 0x01,
    SMU_CmdRejectedBusy = 0xFC,
    SMU_CmdRejectedPrereq = 0xFD,
    SMU_UnknownCmd = 0xFE,
    SMU_Failed = 0xFF
};

NTSTATUS:smu_status_to_nt(SMUStatus:s) {
    switch (s) {
        case SMU_OK:
            return STATUS_SUCCESS;
        case SMU_CmdRejectedBusy:
            return STATUS_DEVICE_BUSY;
        case SMU_CmdRejectedPrereq:
            return STATUS_INTERNAL_ERROR;
        case SMU_UnknownCmd:
            return STATUS_INVALID_INFO_CLASS;
        case SMU_Failed:
            return STATUS_UNSUCCESSFUL;
    }
    return STATUS_NOT_IMPLEMENTED;
}

CodeName:get_code_name(family, model, pkg_type) {
    switch ((family << 8) | model) {
        case 0x1701:
        {
            if (pkg_type == 3) { // socket SP3
                return CPU_Naples;
            } else if (pkg_type == 7) { // socket TRX
                return CPU_Threadripper; // Whitehaven
            }
            return CPU_SummitRidge;
        }
        case 0x1708:
            return (pkg_type == 3 || pkg_type == 7) ? CPU_Colfax : CPU_PinnacleRidge;
        case 0x1711:
            return CPU_RavenRidge;
        case 0x1718:
            return pkg_type == 7 ? CPU_RavenRidge2 : CPU_Picasso;
        case 0x1720:
            return CPU_Dali;
        case 0x1731:
            return pkg_type == 7 ? CPU_CastlePeak : CPU_Rome;
        case 0x1750:
            return CPU_FireFlight; // Subor Z+, CPUID 0x00850F00
        case 0x1760:
            return CPU_Renoir;
        case 0x1768:
            return CPU_Lucienne;
        case 0x1771:
            return CPU_Matisse;
        case 0x1790, 0x1791:
            return CPU_Vangogh;
        case 0x1798:
            return CPU_Mero;
        case 0x17A0:
            return CPU_Mendocino;
        // Family 19h
        case 0x1900, 0x1901:
            return CPU_Milan;
        case 0x1908:
            return CPU_Chagall;
        case 0x1911:
            return CPU_Genoa;
        case 0x1918:
            return CPU_StormPeak;
        case 0x1920, 0x1921:
            return CPU_Vermeer;
        case 0x1944:
            return CPU_Rembrandt;
        case 0x1950:
            return CPU_Cezanne;
        case 0x1961:
            return pkg_type == 1 ? CPU_DragonRange : CPU_Raphael;
        case 0x1974, 0x1975:
            return CPU_Phoenix;
        case 0x1978:
            return CPU_Phoenix2;
        case 0x197C:
            return CPU_HawkPoint;
        // Family 1Ah
        case 0x1A02:
            return CPU_Turin;
        case 0x1A08:
            return CPU_ShimadaPeak;
        case 0x1A11:
            return CPU_TurinD;
        case 0x1A20, 0x1A24:
            return CPU_StrixPoint;
        case 0x1A44:
            return CPU_GraniteRidge;
        case 0x1A60:
            return CPU_KrackanPoint;
        case 0x1A68:
            return CPU_KrackanPoint2;
        case 0x1A70:
            return CPU_StrixHalo;
        case 0x1AA0:
            return CPU_Bergamo;
        default:
            return CPU_Undefined;
    }
    return CPU_Undefined;
}

#define ADDRINFO[.cmd, .rsp, .args]

new const k_addrinfo[][ADDRINFO] = [
    [ 0x3B10524, 0x3B10570, 0x3B10A40 ],
    [ 0x3B1051C, 0x3B10568, 0x3B10590 ],
    [ 0x3B10A20, 0x3B10A80, 0x3B10A88 ],
    [ 0x3B10924, 0x3B10970, 0x3B10A40 ],
];

new const k_addridx[] = [
    /* Colfax        = */  1,
    /* Renoir        = */  2,
    /* Picasso       = */  2,
    /* Matisse       = */  0,
    /* Threadripper  = */  1,
    /* CastlePeak    = */  0,
    /* RavenRidge    = */  2,
    /* RavenRidge2   = */  2,
    /* SummitRidge   = */  1,
    /* PinnacleRidge = */  1,
    /* Rembrandt     = */  2,
    /* Vermeer       = */  0,
    /* Vangogh       = */  2,
    /* Cezanne       = */  2,
    /* Milan         = */  0,
    /* Dali          = */  2,
    /* Raphael       = */  0,
    /* GraniteRidge  = */  0,
    /* Naples        = */  1,
    /* FireFlight    = */  2,
    /* Rome          = */  0,
    /* Chagall       = */  0,
    /* Lucienne      = */  2,
    /* Phoenix       = */  2,
    /* Phoenix2      = */  2,
    /* Mendocino     = */  2,
    /* Genoa         = */  0,
    /* StormPeak     = */  0,
    /* DragonRange   = */  0,
    /* Mero          = */  2,
    /* HawkPoint     = */  2,
    /* StrixPoint    = */  2,
    /* StrixHalo     = */  2,
    /* KrackanPoint  = */  2,
    /* KrackanPoint2 = */  2,
    /* Turin         = */  0,
    /* TurinD        = */  0,
    /* Bergamo       = */  0,
    /* ShimadaPeak   = */  3,
];

const SMU_PCI_ADDR_REG = 0xC4;
const SMU_PCI_DATA_REG = 0xC8;
const SMU_REQ_MAX_ARGS = 6;
const SMU_RETRIES_MAX = 8096;

NTSTATUS:read_reg(addr, &data) {
    new NTSTATUS:status = pci_config_write_dword(0, 0, 0, SMU_PCI_ADDR_REG, addr);
    if (NT_SUCCESS(status)) {
        status = pci_config_read_dword(0, 0, 0, SMU_PCI_DATA_REG, data);
    }
    return status;
}

NTSTATUS:write_reg(addr, data) {
    new NTSTATUS:status = pci_config_write_dword(0, 0, 0, SMU_PCI_ADDR_REG, addr);
    if (NT_SUCCESS(status)) {
        status = pci_config_write_dword(0, 0, 0, SMU_PCI_DATA_REG, data);
    }
    return status;
}

new CodeName:g_code_name = CPU_Undefined;

NTSTATUS:send_command(msg, args[SMU_REQ_MAX_ARGS]) {
    new addrinfo_idx = k_addridx[g_code_name];
    new addr_cmd = k_addrinfo[addrinfo_idx].cmd;
    new addr_rsp = k_addrinfo[addrinfo_idx].rsp;
    new addr_args = k_addrinfo[addrinfo_idx].args;

    new value = 0;
    new NTSTATUS:status = STATUS_SUCCESS;

    // Step 1: Wait until the RSP register is non-zero.
    for (new i = 0; i < SMU_RETRIES_MAX; ++i) {
        status = read_reg(addr_rsp, value);
        if (!NT_SUCCESS(status))
            return status;
        if (value != 0)
            break;
    }
    // Step 1.b: A command is still being processed meaning a new command cannot be issued.
    if (value == 0)
        return STATUS_DEVICE_BUSY;

    // Step 2: Write zero (0) to the RSP register
    status = write_reg(addr_rsp, 0);
    if (!NT_SUCCESS(status))
        return status;

    // Step 3: Write the argument(s) into the argument register(s)
    for (new i = 0; i < SMU_REQ_MAX_ARGS; ++i) {
        status = write_reg(addr_args + 4 * i, args[i]);
        if (!NT_SUCCESS(status))
            return status;
    }

    // Step 4: Write the message Id into the Message ID register
    status = write_reg(addr_cmd, msg);
    if (!NT_SUCCESS(status))
        return status;

    // Step 5: Wait until the Response register is non-zero.
    for (new i = 0; i < SMU_RETRIES_MAX; ++i) {
        status = read_reg(addr_rsp, value);
        if (!NT_SUCCESS(status))
            return status;
        if (value != 0)
            break;
    }
    if (value == 0)
        return STATUS_IO_TIMEOUT;

    // Step 6: If the Response register contains OK, then SMU has finished processing  the message.
    status = smu_status_to_nt(SMUStatus:value);
    if (!NT_SUCCESS(status))
        return status;

    for (new i = 0; i < SMU_REQ_MAX_ARGS; ++i) {
        status = read_reg(addr_args + 4 * i, args[i]);
        if (!NT_SUCCESS(status))
            return status;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:send_command2(msg, &a1=0, &a2=0, &a3=0, &a4=0, &a5=0, &a6=0) {
    new args[SMU_REQ_MAX_ARGS];
    args[0] = a1;
    args[1] = a2;
    args[2] = a3;
    args[3] = a4;
    args[4] = a5;
    args[5] = a6;
    new NTSTATUS:status = send_command(msg, args);
    a1 = args[0];
    a2 = args[1];
    a3 = args[2];
    a4 = args[3];
    a5 = args[4];
    a6 = args[5];
    return status;
}

NTSTATUS:get_pm_table_version(&version) {
    switch (g_code_name) {
        case CPU_SummitRidge, CPU_Naples, CPU_PinnacleRidge, CPU_Colfax, CPU_Threadripper:
            return send_command2(0x0d, version);
        case CPU_Dali, CPU_Picasso, CPU_RavenRidge, CPU_RavenRidge2, CPU_FireFlight:
            return send_command2(0x0c, version);
        case CPU_Matisse, CPU_Vermeer, CPU_CastlePeak, CPU_Rome, CPU_Chagall, CPU_Milan:
            return send_command2(0x08, version);
        case CPU_Renoir, CPU_Rembrandt, CPU_Cezanne, CPU_Mero, CPU_Vangogh, CPU_Phoenix,
             CPU_Phoenix2, CPU_HawkPoint, CPU_Mendocino, CPU_StrixHalo, CPU_StrixPoint, CPU_KrackanPoint,
             CPU_Lucienne, CPU_KrackanPoint2:
            return send_command2(0x06, version);
        case CPU_Raphael, CPU_Genoa, CPU_StormPeak, CPU_DragonRange, CPU_GraniteRidge, CPU_Bergamo, 
             CPU_Turin, CPU_TurinD, CPU_ShimadaPeak:
            return send_command2(0x05, version);
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

NTSTATUS:transfer_table_to_dram() {
    new three = 3;
    switch (g_code_name) {
        case CPU_SummitRidge, CPU_Naples, CPU_PinnacleRidge, CPU_Colfax, CPU_Threadripper:
            return send_command2(0x0a);
        case CPU_Raphael, CPU_Genoa, CPU_StormPeak, CPU_DragonRange, CPU_GraniteRidge, CPU_Bergamo, 
             CPU_Turin, CPU_TurinD, CPU_ShimadaPeak:
            return send_command2(0x03);
        case CPU_Matisse, CPU_Vermeer, CPU_CastlePeak, CPU_Rome, CPU_Chagall, CPU_Milan:
            return send_command2(0x05);
        case CPU_Renoir, CPU_Rembrandt, CPU_Cezanne, CPU_Mero, CPU_Vangogh, CPU_Phoenix,
             CPU_Phoenix2, CPU_HawkPoint, CPU_Mendocino, CPU_StrixHalo, CPU_StrixPoint, CPU_KrackanPoint,
             CPU_Lucienne, CPU_KrackanPoint2:
            return send_command2(0x65, three);
        case CPU_Dali, CPU_Picasso, CPU_RavenRidge, CPU_RavenRidge2, CPU_FireFlight:
            return send_command2(0x3d, three);
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

NTSTATUS:get_pm_table_base(&base) {
    base = 0;
    new class;
    new fn[3];
    switch (g_code_name) {
        case CPU_Raphael, CPU_Genoa, CPU_StormPeak, CPU_DragonRange, CPU_GraniteRidge, CPU_Bergamo, 
             CPU_Turin, CPU_TurinD, CPU_ShimadaPeak: {
            fn[0] = 0x04;
            class = 1;
        }
        case CPU_Matisse, CPU_Vermeer, CPU_CastlePeak, CPU_Rome, CPU_Chagall, CPU_Milan: {
            fn[0] = 0x06;
            class = 1;
        }
        case CPU_Renoir, CPU_Rembrandt, CPU_Cezanne, CPU_Mero, CPU_Vangogh, CPU_Phoenix,
             CPU_Phoenix2, CPU_HawkPoint, CPU_Mendocino, CPU_StrixHalo, CPU_StrixPoint, CPU_KrackanPoint,
             CPU_Lucienne, CPU_KrackanPoint2: {
            fn[0] = 0x66;
            class = 1;
        }
        case CPU_Colfax, CPU_PinnacleRidge, CPU_SummitRidge, CPU_Naples, CPU_Threadripper: {
            fn[0] = 0x0b;
            fn[1] = 0x0c;
            class = 2;
        }
        case CPU_Dali, CPU_Picasso, CPU_RavenRidge, CPU_RavenRidge2, CPU_FireFlight: {
            fn[0] = 0x0a;
            fn[1] = 0x3d;
            fn[2] = 0x0b;
            class = 3;
        }
        default:
            return STATUS_NOT_SUPPORTED;
    }
    new args[6];
    new NTSTATUS:status = STATUS_SUCCESS;
    switch (class) {
        case 1: {
            args[0] = 1;
            args[1] = 1;
            status = send_command(fn[0], args);
            if (!NT_SUCCESS(status))
                return status;
            base = ((args[1] << 32) | args[0]);
            return STATUS_SUCCESS;
        }
        case 2: {
            status = send_command(fn[0], args);
            if (!NT_SUCCESS(status))
                return status;
            args[0] = 0;
            status = send_command(fn[1], args);
            if (!NT_SUCCESS(status))
                return status;
            base = args[0];
            return STATUS_SUCCESS;
        }
        case 3: {
            args[0] = 3;
            status = send_command(fn[0], args);
            if (!NT_SUCCESS(status))
                return status;

            args[0] = 3;
            status = send_command(fn[2], args);
            if (!NT_SUCCESS(status))
                return status;

            base = args[0];
            return STATUS_SUCCESS;
        }
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

new g_table_base;

NTSTATUS:check_smu_register_range(cmd) {
    if (cmd < 0 || cmd > 0xFFFFFFFF) return STATUS_NOT_SUPPORTED;

    // Check for range:

    // 1. 0x3B10*** (0x3B10000 – 0x3B10FFF) SMU Mailboxes
    if ((cmd & 0xFFFFF000) == 0x3B10000) return STATUS_SUCCESS;

    // 2. 0x130000*0 (0x13000000 - 0x130000F0) SMU Mailboxes on Pre-Ryzen
    if ((cmd & 0xFFFFFFF0) == (0x13000000)) return STATUS_SUCCESS;

    // 3. 0x56***-0x5A*** (0x56000 – 0x5AFFF) SMU SVI2 Planes
    if (cmd >= 0x56000 && cmd <= 0x5AFFF) return STATUS_SUCCESS;

    // 4. 0x6F*** (0x6F000 – 0x6FFFF) SMU Extended SVI2 Planes
    if ((cmd & 0xFFFFF000) == 0x6F000) return STATUS_SUCCESS;

    // If not in expected range
    return STATUS_NOT_SUPPORTED;
}

/// Resolve physical memory table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Version, [1] = Table base
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_resolve_pm_table, 0, 2) {
    new version;
    new NTSTATUS:status = get_pm_table_version(version);
    if (!NT_SUCCESS(status))
        return status;
    debug_print(''RyzenSMU: PM Table Version: %x\n'', version);
    new table_base;
    status = get_pm_table_base(table_base);
    if (!NT_SUCCESS(status))
        return status;
    debug_print(''RyzenSMU: PM Table Base: %x\n'', table_base);

    g_table_base = table_base;

    out[0] = version;
    out[1] = table_base;

    return STATUS_SUCCESS;
}

/// Update physical memory table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_update_pm_table, 0, 0) {
    return transfer_table_to_dram();
}

/// Read physical memory table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out Table contents
/// @param out_size How much of the table to read
/// @return An NTSTATUS
DEFINE_IOCTL(ioctl_read_pm_table) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (!g_table_base)
        return STATUS_DEVICE_NOT_READY;
    new read_count = min(out_size, PAGE_SIZE / 8);
    new read_size = read_count * 8;
    new VA:va = io_space_map(g_table_base, read_size);
    new NTSTATUS:status = STATUS_SUCCESS;
    if (va) {
        new read;
        for (new i = 0; i < read_count; ++i) {
            status = virtual_read_qword(va + i * 8, read);
            if (!NT_SUCCESS(status))
                break;
            out[i] = read;
        }

        io_space_unmap(va, read_size);
    } else {
        status = STATUS_COMMITMENT_LIMIT;
    }

    return status;
}

/// Get CPU Codename integer.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Code name integer
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_code_name, 0, 1) {
    out[0] = _:g_code_name;
    return STATUS_SUCCESS;
}

/// Get SMU version.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Version
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_get_smu_version, 0, 1) {
    new args[6];
    args[0] = 1;
    new NTSTATUS:status = send_command(0x02, args);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = args[0];
    return STATUS_SUCCESS;
}

/// Read SMU register via PCI config space.
///
/// @param in [0] = Smu register address to read
/// @param in_size Must be 1
/// @param out [0] = Value read from the register
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_read_smu_register, 1, 1) {
    // Validate input
    new NTSTATUS:status = check_smu_register_range(in[0]);
    if (!NT_SUCCESS(status))
        return status;

    new data;
    status = read_reg(in[0], data);
    if (!NT_SUCCESS(status))
        return status;

    // Write out
    out[0] = data;
    return STATUS_SUCCESS;
}

/// Write value to SMU register via PCI config space.
///
/// @param in [0] = Smu register address to write
/// @param in [1] = Value to write
/// @param in_size Must be 2
/// @param out None
/// @param out_size Must be 0
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_write_smu_register, 2, 0) {
    // Validate input
    new NTSTATUS:status = check_smu_register_range(in[0]);
    if (!NT_SUCCESS(status))
        return status;

    status = write_reg(in[0], in[1]);
    if (!NT_SUCCESS(status))
        return status;

    return STATUS_SUCCESS;
}

/// Send SMU command.
///
/// @param in [0] = Smu command, [1-6] = Smu arguments
/// @param in_size Must be 7
/// @param out [0-5] = Smu arguments
/// @param out_size Must be 6
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_send_smu_command, 7, 6) {
    if ((in[0] & ~0xFF) != 0)
        return STATUS_NOT_SUPPORTED;

    new args[6];
    args[0] = in[1];
    args[1] = in[2];
    args[2] = in[3];
    args[3] = in[4];
    args[4] = in[5];
    args[5] = in[6];
    new NTSTATUS:status = send_command(in[0], args);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = args[0];
    out[1] = args[1];
    out[2] = args[2];
    out[3] = args[3];
    out[4] = args[4];
    out[5] = args[5];
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    new extended[4];
    cpuid(0x80000001, 0, extended);

    new pkg_type = (extended[1] >>> 28) & 0xFF;

    debug_print(''RyzenSMU: family: %x model: %x pkg_type: %x\n'', family, model, pkg_type);

    if (family != 0x17 && family != 0x19 && family != 0x1A)
        return STATUS_NOT_SUPPORTED;

    new CodeName:code_name = get_code_name(family, model, pkg_type);
    if (code_name == CPU_Undefined)
        return STATUS_NOT_SUPPORTED;

    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;

    debug_print(''RyzenSMU: code_name: %x vid: %x did: %x\n'', _:code_name, didvid & 0xFFFF, (didvid >>> 16) & 0xFFFF);

    // sanity check that it's something AMD
    if (didvid & 0xFFFF != 0x1022)
        return STATUS_NOT_SUPPORTED;

    if (k_addridx[code_name] == -1)
        return STATUS_NOT_SUPPORTED;

    g_code_name = code_name;

    return STATUS_SUCCESS;
}
