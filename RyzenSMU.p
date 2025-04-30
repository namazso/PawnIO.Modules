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
    CPU_GraniteRidge
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
            return pkg_type == 7 ? CPU_Threadripper : CPU_SummitRidge;
        case 0x1708:
            return pkg_type == 7 ? CPU_Colfax : CPU_PinnacleRidge;
        case 0x1711:
            return CPU_RavenRidge;
        case 0x1718:
            return pkg_type == 7 ? CPU_RavenRidge2 : CPU_Picasso;
        case 0x1720:
            return CPU_Dali;
        case 0x1731:
            return CPU_CastlePeak;
        case 0x1760:
            return CPU_Renoir;
        case 0x1771:
            return CPU_Matisse;
        case 0x1790:
            return CPU_Vangogh;
        case 0x1900:
            return CPU_Milan;
        case 0x1920, 0x1921:
            return CPU_Vermeer;
        case 0x1940:
            return CPU_Rembrandt;
        case 0x1950:
            return CPU_Cezanne;
        case 0x1961:
            return CPU_Raphael;
        case 0x1A44:
            return CPU_GraniteRidge;
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
    /* Rembrandt     = */ -1,
    /* Vermeer       = */  0,
    /* Vangogh       = */ -1,
    /* Cezanne       = */ -1,
    /* Milan         = */ -1,
    /* Dali          = */  2,
    /* Raphael       = */  0,
    /* GraniteRidge  = */  0,
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
        case CPU_RavenRidge, CPU_Picasso:
            return send_command2(0x0c, version);
        case CPU_Matisse, CPU_Vermeer:
            return send_command2(0x08, version);
        case CPU_Renoir:
            return send_command2(0x06, version);
        case CPU_Raphael, CPU_GraniteRidge:
            return send_command2(0x05, version);
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

NTSTATUS:transfer_table_to_dram() {
    new three = 3;
    switch (g_code_name) {
        case CPU_Raphael, CPU_GraniteRidge:
            return send_command2(0x03);
        case CPU_Matisse, CPU_Vermeer:
            return send_command2(0x05);
        case CPU_Renoir:
            return send_command2(0x65, three);
        case CPU_Picasso, CPU_RavenRidge, CPU_RavenRidge2:
            return send_command2(0x06, three);
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
        case CPU_Raphael, CPU_GraniteRidge: {
            fn[0] = 0x04;
            class = 1;
        }
        case CPU_Vermeer, CPU_Matisse, CPU_CastlePeak: {
            fn[0] = 0x06;
            class = 1;
        }
        case CPU_Renoir: {
            fn[0] = 0x66;
            class = 1;
        }
        case CPU_Colfax, CPU_PinnacleRidge: {
            fn[0] = 0x0b;
            fn[1] = 0x0c;
            class = 2;
        }
        case CPU_Dali, CPU_Picasso, CPU_RavenRidge, CPU_RavenRidge2: {
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
            new low;

            // ## low

            args[0] = 3;
            status = send_command(fn[0], args);
            if (!NT_SUCCESS(status))
                return status;

            args[0] = 3;
            status = send_command(fn[2], args);
            if (!NT_SUCCESS(status))
                return status;

            low = args[0];

            // ## high

            args[0] = 3;
            status = send_command(fn[1], args);
            if (!NT_SUCCESS(status))
                return status;

            args[0] = 5;
            status = send_command(fn[0], args);
            if (!NT_SUCCESS(status))
                return status;

            args[0] = 5;
            status = send_command(fn[2], args);
            if (!NT_SUCCESS(status))
                return status;

            base = low | (args[0] << 32);
            return STATUS_SUCCESS;
        }
        default:
            return STATUS_NOT_SUPPORTED;
    }
    return STATUS_NOT_SUPPORTED;
}

new g_table_base;

/// Resolve physical memory table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Version, [1] = Table base
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_PCI" mutant before calling this
forward NTSTATUS:ioctl_resolve_pm_table(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_resolve_pm_table(in[], in_size, out[], out_size) {
    if (out_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
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
forward NTSTATUS:ioctl_update_pm_table(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_update_pm_table(in[], in_size, out[], out_size) {
    return transfer_table_to_dram();
}

/// Read physical memory table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
forward NTSTATUS:ioctl_read_pm_table(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_read_pm_table(in[], in_size, out[], out_size) {
    if (!g_table_base)
        return STATUS_DEVICE_NOT_READY;
    new read_count = _min(out_size, PAGE_SIZE / 8);
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
forward NTSTATUS:ioctl_get_code_name(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_get_code_name(in[], in_size, out[], out_size) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

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
forward NTSTATUS:ioctl_get_smu_version(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_get_smu_version(in[], in_size, out[], out_size) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new args[6];
    args[0] = 1;
    new NTSTATUS:status = send_command(0x02, args);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = args[0];
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

    debug_print(''RyzenSMU: family: %x model: %x pkg_type: %x\n'', family, model, pkg_type);

    if (family != 0x17 && family != 0x19)
        return STATUS_NOT_SUPPORTED;

    new CodeName:code_name = get_code_name(family, model, pkg_type);
    if (code_name == CPU_Undefined)
        return STATUS_NOT_SUPPORTED;

    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;

    debug_print(''RyzenSMU: code_name: %x vid: %x did: %x\n'', _:code_name, didvid & 0xFFFF, (didvid >> 16) & 0xFFFF);

    // sanity check that it's something AMD
    if (didvid & 0xFFFF != 0x1022)
        return STATUS_NOT_SUPPORTED;

    if (k_addridx[code_name] == -1)
        return STATUS_NOT_SUPPORTED;

    g_code_name = code_name;

    return STATUS_SUCCESS;
}
