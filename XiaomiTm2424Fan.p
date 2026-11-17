//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  CpuPowerTempOverlay contributors
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

// Read-only fan tachometers for the Xiaomi Book Pro 14 (TM2424).
//
// MIFS v2 firmware does not expose fan speeds through its WMI interface. The
// two tachometers are little-endian u16 RPM values in the EC shared-memory
// window described by the system DSDT as the Q_EC "ERAM" region.

#define PCI_BUS_LPC                    0x00
#define PCI_DEVICE_LPC                 0x1F
#define PCI_FUNCTION_LPC               0x00
#define PCI_CFG_VENDOR_DEVICE          0x00
#define PCI_CFG_SUBSYSTEM              0x2C

// Intel LPC/eSPI E402 and the Xiaomi TM2424 subsystem identifier. Requiring
// both prevents the fixed EC window from being mapped on unrelated machines.
#define TM2424_LPC_DIDVID              0xE4028086
#define TM2424_LPC_SUBSYS              0x24241D72

#define TM2424_EC_MEM_BASE             0x00000000FE0B0300
#define TM2424_EC_MEM_SIZE             0x100
#define TM2424_FAN_LEFT                0x69
#define TM2424_FAN_RIGHT               0x6B

new VA:g_ec_mem = NULL;

/// Read both raw fan tachometers without writing to the EC.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = left fan RPM; [1] = right fan RPM
/// @param out_size Must be 2
/// @return An NTSTATUS
/// @note A value of 0 means that the corresponding fan is stopped.
DEFINE_IOCTL_SIZED(ioctl_read_fans, 0, 2) {
    if (g_ec_mem == NULL)
        return STATUS_DEVICE_NOT_READY;

    new left_rpm = 0;
    new right_rpm = 0;
    new NTSTATUS:status = virtual_read_word(
        g_ec_mem + TM2424_FAN_LEFT, left_rpm);
    if (!NT_SUCCESS(status))
        return status;

    status = virtual_read_word(g_ec_mem + TM2424_FAN_RIGHT, right_rpm);
    if (!NT_SUCCESS(status))
        return status;

    out[0] = left_rpm;
    out[1] = right_rpm;
    return STATUS_SUCCESS;
}

/// Return the platform identity and mapped read-only EC window.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = (device ID << 16) | PCI vendor ID
///            [1] = (subsystem ID << 16) | subsystem vendor ID
///            [2] = physical EC shared-memory base
///            [3] = mapped size in bytes
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 4) {
    if (g_ec_mem == NULL)
        return STATUS_DEVICE_NOT_READY;

    out[0] = TM2424_LPC_DIDVID;
    out[1] = TM2424_LPC_SUBSYS;
    out[2] = TM2424_EC_MEM_BASE;
    out[3] = TM2424_EC_MEM_SIZE;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    if (get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;

    new value = 0;
    new NTSTATUS:status = pci_config_read_dword(
        PCI_BUS_LPC, PCI_DEVICE_LPC, PCI_FUNCTION_LPC,
        PCI_CFG_VENDOR_DEVICE, value);
    if (!NT_SUCCESS(status))
        return status;
    if (value != TM2424_LPC_DIDVID)
        return STATUS_NO_SUCH_DEVICE;

    status = pci_config_read_dword(
        PCI_BUS_LPC, PCI_DEVICE_LPC, PCI_FUNCTION_LPC,
        PCI_CFG_SUBSYSTEM, value);
    if (!NT_SUCCESS(status))
        return status;
    if (value != TM2424_LPC_SUBSYS)
        return STATUS_NO_SUCH_DEVICE;

    g_ec_mem = io_space_map(TM2424_EC_MEM_BASE, TM2424_EC_MEM_SIZE);
    return g_ec_mem == NULL ? STATUS_INSUFFICIENT_RESOURCES : STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_ec_mem != NULL) {
        io_space_unmap(g_ec_mem, TM2424_EC_MEM_SIZE);
        g_ec_mem = NULL;
    }

    return STATUS_SUCCESS;
}
