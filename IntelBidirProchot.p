//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Celso Naemen <180766906+celsonaemen@users.noreply.github.com>
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

#define MSR_POWER_CTL                    0x000001FC
#define MSR_IA32_THERM_STATUS            0x0000019C
#define MSR_PACKAGE_THERM_STATUS         0x000001B1

#define ENABLE_BIDIR_PROCHOT             (1 << 0)

new g_original_power_ctl = 0;
new bool:g_original_saved = false;

NTSTATUS:set_bidir_prochot_response(bool:enabled, &readback) {
    new current = 0;
    new NTSTATUS:status = msr_read(MSR_POWER_CTL, current);
    if (!NT_SUCCESS(status))
        return status;

    new requested = enabled
        ? current | ENABLE_BIDIR_PROCHOT
        : current & ~ENABLE_BIDIR_PROCHOT;

    if (requested != current) {
        status = msr_write(MSR_POWER_CTL, requested);
        if (!NT_SUCCESS(status))
            return status;
    }

    status = msr_read(MSR_POWER_CTL, readback);
    if (!NT_SUCCESS(status))
        return status;

    if (((readback & ENABLE_BIDIR_PROCHOT) != 0) != enabled)
        return STATUS_UNSUCCESSFUL;

    return STATUS_SUCCESS;
}

/// Read the current bi-directional PROCHOT response and thermal status.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = current response state (0 = ignore external input, 1 = respond)
///            [1] = response state saved when the module loaded
///            [2] = raw MSR_POWER_CTL value
///            [3] = raw IA32_THERM_STATUS value for the executing core
///            [4] = raw IA32_PACKAGE_THERM_STATUS value
/// @param out_size Must be 5
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_bidir_prochot_status, 0, 5) {
    new status = 0;
    new NTSTATUS:result = msr_read(MSR_POWER_CTL, status);
    if (!NT_SUCCESS(result))
        return result;

    new core_thermal = 0;
    result = msr_read(MSR_IA32_THERM_STATUS, core_thermal);
    if (!NT_SUCCESS(result))
        return result;

    new package_thermal = 0;
    result = msr_read(MSR_PACKAGE_THERM_STATUS, package_thermal);
    if (!NT_SUCCESS(result))
        return result;

    out[0] = (status & ENABLE_BIDIR_PROCHOT) != 0;
    out[1] = (g_original_power_ctl & ENABLE_BIDIR_PROCHOT) != 0;
    out[2] = status;
    out[3] = core_thermal;
    out[4] = package_thermal;
    return STATUS_SUCCESS;
}

/// Enable or disable the CPU response to an externally asserted PROCHOT input.
///
/// This changes only ENABLE_BIDIR_PROCHOT in MSR_POWER_CTL. It does not disable
/// the processor's internal thermal monitor or THERMTRIP protection.
///
/// @param in [0] = requested response state (0 = ignore, 1 = respond)
/// @param in_size Must be 1
/// @param out [0] = verified response state
///            [1] = verified raw MSR_POWER_CTL value
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @warning Disabling the response removes protection requested by other platform components.
DEFINE_IOCTL_SIZED(ioctl_set_bidir_prochot, 1, 2) {
    if (in[0] != 0 && in[0] != 1)
        return STATUS_INVALID_PARAMETER;

    new readback = 0;
    new NTSTATUS:status = set_bidir_prochot_response(in[0] != 0, readback);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = (readback & ENABLE_BIDIR_PROCHOT) != 0;
    out[1] = readback;
    return STATUS_SUCCESS;
}

/// Restore the bi-directional PROCHOT response state saved at module load.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = verified response state
///            [1] = verified raw MSR_POWER_CTL value
/// @param out_size Must be 2
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_restore_bidir_prochot, 0, 2) {
    if (!g_original_saved)
        return STATUS_DEVICE_NOT_READY;

    new readback = 0;
    new NTSTATUS:status = set_bidir_prochot_response(
        (g_original_power_ctl & ENABLE_BIDIR_PROCHOT) != 0,
        readback);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = (readback & ENABLE_BIDIR_PROCHOT) != 0;
    out[1] = readback;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();
    if (cpu_fms_family(fms) != 0x06)
        return STATUS_NOT_SUPPORTED;

    new NTSTATUS:status = msr_read(MSR_POWER_CTL, g_original_power_ctl);
    if (!NT_SUCCESS(status))
        return status;

    g_original_saved = true;
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (!g_original_saved)
        return STATUS_SUCCESS;

    new readback = 0;
    new NTSTATUS:status = set_bidir_prochot_response(
        (g_original_power_ctl & ENABLE_BIDIR_PROCHOT) != 0,
        readback);
    if (NT_SUCCESS(status))
        g_original_saved = false;

    return status;
}
