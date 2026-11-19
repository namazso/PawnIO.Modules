// SPDX-License-Identifier: LGPL-2.1-or-later
// Based on the fixed-window, read-only pattern in PawnIO.Modules/XiaomiTm2424Fan.p.
// Copyright (C) 2026 CpuPowerTempOverlay contributors (original pattern).
// Y740 adaptation: 2026. Experimental; hardware reads have not been validated.
//
// Reference: this 81UH machine's BVCN16WW DSDT:
// ECMM = SystemMemory(0xFC7E0800, 0x1000).
// GZ08/GZ09/GZ10: offsets 0x13/0x14/0x15, each multiplied by 100 by WMAA.
// GZ44: offset 0x16, low two bits: 2=quiet, 0=balanced, 1=performance.
// CTMP: offset 0xB0; SKTC: offset 0xB4.
// This module exposes NO writes, configurable addresses, or mailbox commands.

#include <pawnio.inc>

#define Y740_LPC_DIDVID 0xA30D8086
#define Y740_LPC_SUBSYS 0x380517AA
#define Y740_EC_BASE 0x00000000FC7E0800
#define Y740_EC_SIZE 0xC0

new VA:g_ec_mem = NULL;

/// Read fixed Y740 telemetry bytes without writing to the controller.
/// @param in Unused; must be empty.
/// @param in_size Must be zero.
/// @param out CPU C, GPU C, fan1 RPM, fan2 RPM, firmware max RPM, raw mode bits.
/// @param out_size Must be six.
/// @return NTSTATUS; no output values are published if any native read fails.
DEFINE_IOCTL_SIZED(ioctl_read_telemetry, 0, 6) {
    if (in_size != 0)
        return STATUS_INVALID_PARAMETER;
    if (g_ec_mem == NULL)
        return STATUS_DEVICE_NOT_READY;

    new values[6];
    new offsets[6] = [0xB0, 0xB4, 0x13, 0x14, 0x15, 0x16];
    for (new i = 0; i < sizeof values; ++i) {
        new value = 0;
        new NTSTATUS:status = virtual_read_byte(g_ec_mem + offsets[i], value);
        if (!NT_SUCCESS(status))
            return status;
        values[i] = value;
    }
    // 0xFF is the unavailable/decode-failure sentinel seen in prior EC probes.
    for (new i = 0; i < sizeof values; ++i) {
        if (values[i] == 0xFF)
            return STATUS_DEVICE_DATA_ERROR;
    }
    out[0] = values[0];
    out[1] = values[1];
    out[2] = values[2] * 100;
    out[3] = values[3] * 100;
    out[4] = values[4] * 100;
    out[5] = values[5] & 3;
    return STATUS_SUCCESS;
}

/// Return the fixed, hardware-guarded platform and memory-window identity.
/// @param in Unused; must be empty.
/// @param in_size Must be zero.
/// @param out LPC PCI vendor/device, LPC subsystem, fixed physical base, size.
/// @param out_size Must be four.
/// @return NTSTATUS.
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 4) {
    if (in_size != 0)
        return STATUS_INVALID_PARAMETER;
    if (g_ec_mem == NULL)
        return STATUS_DEVICE_NOT_READY;
    out[0] = Y740_LPC_DIDVID;
    out[1] = Y740_LPC_SUBSYS;
    out[2] = Y740_EC_BASE;
    out[3] = Y740_EC_SIZE;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;
    new value = 0;
    new NTSTATUS:status = pci_config_read_dword(0, 0x1F, 0, 0, value);
    if (!NT_SUCCESS(status))
        return status;
    if (value != Y740_LPC_DIDVID)
        return STATUS_NO_SUCH_DEVICE;
    status = pci_config_read_dword(0, 0x1F, 0, 0x2C, value);
    if (!NT_SUCCESS(status))
        return status;
    if (value != Y740_LPC_SUBSYS)
        return STATUS_NO_SUCH_DEVICE;
    g_ec_mem = io_space_map(Y740_EC_BASE, Y740_EC_SIZE);
    return g_ec_mem == NULL ? STATUS_INSUFFICIENT_RESOURCES : STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_ec_mem != NULL) {
        io_space_unmap(g_ec_mem, Y740_EC_SIZE);
        g_ec_mem = NULL;
    }
    return STATUS_SUCCESS;
}
