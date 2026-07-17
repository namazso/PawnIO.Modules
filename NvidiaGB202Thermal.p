// PawnIO module for NVIDIA GB202 thermal registers observed in HWMonitor 1.65.1.
// Copyright (C) 2026 Gonzalo Duque de Blas (@GDuqueB)
// SPDX-License-Identifier: LGPL-2.1-or-later

#include <pawnio.inc>

#define PCI_VENDOR_ID_NVIDIA       0x10DE
#define PCI_DEVICE_ID_RTX_5090     0x2B85

#define PCI_CFG_BAR0_LOW           0x10
#define PCI_CFG_BAR0_HIGH          0x14
#define PCI_BAR_IO_SPACE           0x01
#define PCI_BAR_MEM_TYPE_MASK      0x06
#define PCI_BAR_MEM_TYPE_64BIT     0x04
#define PCI_BAR_MEM_ADDR_MASK      0xFFFFFFF0

// All six thermal channels fit in this single 4 KiB page of BAR0.
#define THERMAL_PAGE_OFFSET        0x00AD0000
#define THERMAL_PAGE_SIZE          0x1000
#define THERMAL_CHANNELS_OFFSET    0x0A90
#define THERMAL_CHANNEL_COUNT      6

new VA:g_thermal_page_va = NULL;
new g_pci_bdf = 0;
new g_vid_did = 0;

NTSTATUS:nvidia_thermal_init(bus, device, function) {
    if (bus < 0 || bus > 0xFF || device < 0 || device > 0x1F || function < 0 || function > 7)
        return STATUS_INVALID_PARAMETER;

    new vid_did = 0;
    new NTSTATUS:status = pci_config_read_dword(bus, device, function, 0, vid_did);
    if (!NT_SUCCESS(status))
        return status;
    if ((vid_did & 0xFFFF) != PCI_VENDOR_ID_NVIDIA)
        return STATUS_NOT_SUPPORTED;
    if (((vid_did >> 16) & 0xFFFF) != PCI_DEVICE_ID_RTX_5090)
        return STATUS_NOT_SUPPORTED;

    new base_lo = 0;
    new base_hi = 0;
    status = pci_config_read_dword(bus, device, function, PCI_CFG_BAR0_LOW, base_lo);
    if (!NT_SUCCESS(status))
        return status;
    if (base_lo == 0 || base_lo == 0xFFFFFFFF || (base_lo & PCI_BAR_IO_SPACE))
        return STATUS_NOT_SUPPORTED;

    new bar_type = base_lo & PCI_BAR_MEM_TYPE_MASK;
    if (bar_type != 0 && bar_type != PCI_BAR_MEM_TYPE_64BIT)
        return STATUS_NOT_SUPPORTED;

    if (bar_type == PCI_BAR_MEM_TYPE_64BIT) {
        status = pci_config_read_dword(bus, device, function, PCI_CFG_BAR0_HIGH, base_hi);
        if (!NT_SUCCESS(status))
            return status;
    }

    new bar0 = ((base_hi & 0xFFFFFFFF) << 32) | (base_lo & PCI_BAR_MEM_ADDR_MASK);
    if (bar0 == 0)
        return STATUS_NOT_SUPPORTED;

    new thermal_page_pa = bar0 + THERMAL_PAGE_OFFSET;
    new VA:thermal_page_va = io_space_map(thermal_page_pa, THERMAL_PAGE_SIZE);
    if (thermal_page_va == NULL)
        return STATUS_INSUFFICIENT_RESOURCES;

    g_thermal_page_va = thermal_page_va;
    g_pci_bdf = (bus << 16) | (device << 8) | function;
    g_vid_did = vid_did;
    return STATUS_SUCCESS;
}

/// Validate an RTX 5090 at the supplied PCI address and map its thermal page.
///
/// @param in [0] = (bus << 16) | (device << 8) | function
/// @param in_size Must be 1
/// @param out Unused
/// @param out_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_init, 1, 0) {
    new bdf = in[0];
    if (g_thermal_page_va != NULL) {
        if (bdf == g_pci_bdf)
            return STATUS_SUCCESS;
        return STATUS_DEVICE_BUSY;
    }

    new bus = (bdf >> 16) & 0xFF;
    new device = (bdf >> 8) & 0x1F;
    new function = bdf & 0x07;
    if (bdf != ((bus << 16) | (device << 8) | function))
        return STATUS_INVALID_PARAMETER;
    return nvidia_thermal_init(bus, device, function);
}

/// Read the six contiguous thermal channels used by HWMonitor 1.65.1 on GB202.
///
/// HWMonitor 1.65.1 treats bit 30 as the validity flag and decodes a valid
/// sample as (raw & 0xFFFF) / 256.0 degrees Celsius. Decoding remains the
/// responsibility of the user-mode consumer; this module only returns raw data.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out Raw DWORDs from BAR0 + 0x00AD0A90 through 0x00AD0AA4.
///            Channel 0 at 0x00AD0A90 is the Hot Spot value displayed by
///            HWMonitor 1.65.1; channels 1-5 are additional thermal channels.
/// @param out_size Must be 6
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_thermal, 0, 6) {
    if (g_thermal_page_va == NULL)
        return STATUS_DEVICE_NOT_READY;

    for (new channel = 0; channel < THERMAL_CHANNEL_COUNT; channel++) {
        new raw = 0;
        new NTSTATUS:status = virtual_read_dword(
            g_thermal_page_va + THERMAL_CHANNELS_OFFSET + channel * 4,
            raw
        );
        if (!NT_SUCCESS(status))
            return status;
        out[channel] = raw;
    }
    return STATUS_SUCCESS;
}

/// Return the exact PCI identity and BAR0-relative page mapped by this module.
///
/// @param in Unused
/// @param in_size Must be 0
/// @param out [0] = (device ID << 16) | vendor ID
///            [1] = (bus << 16) | (device << 8) | function
///            [2] = BAR0-relative offset of the mapped thermal page
///            [3] = mapped size
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 4) {
    if (g_thermal_page_va == NULL)
        return STATUS_DEVICE_NOT_READY;

    out[0] = g_vid_did;
    out[1] = g_pci_bdf;
    out[2] = THERMAL_PAGE_OFFSET;
    out[3] = THERMAL_PAGE_SIZE;
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_thermal_page_va != NULL) {
        io_space_unmap(g_thermal_page_va, THERMAL_PAGE_SIZE);
        g_thermal_page_va = NULL;
    }
    g_pci_bdf = 0;
    g_vid_did = 0;
    return STATUS_SUCCESS;
}
