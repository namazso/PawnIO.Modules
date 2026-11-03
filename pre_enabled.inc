// CloudMatrix review candidate, 2026-10-15. SPDX-License-Identifier: LGPL-2.1-or-later
// This is unsigned source, not an installed or hardware-validated interface.
// Requires the same external SMBus mutex as ioctl_smbus_xfer.
// Caller must retain its existing SPD-only operation/restore restrictions.

/// Transfer only when PCI decoding is already enabled, with phase observations.
/// @param in Exactly 14 cells: version 1, nonzero nonce, expected BDF,
/// vendor/device DWORD, legacy input length 4..9, nine legacy input cells.
/// Unused legacy cells must be zero. See tests/pre-enabled/README.md for the complete contract.
/// @param in_size Must be 14.
/// @param out Exactly 21 cells: identity, independent phase statuses/values,
/// matching-post-observations flag, result length and five legacy result cells.
/// @param out_size Must be 21.
/// @return STATUS_SUCCESS means the envelope exists, NOT transfer success.
/// Each attempted phase has a separate status; callers must validate all phases.
/// @warning Requires the external SMBus mutex over the complete operation.
/// No fallback to ioctl_smbus_xfer is allowed after an observation failure.
DEFINE_IOCTL_SIZED(ioctl_xfer_pre_enabled, 14, 21) {
    if (in[0] != 1 || in[1] == 0 || in[4] < 4 || in[4] > 9)
        return STATUS_INVALID_PARAMETER;
    if (in[3] < 0 || in[3] > 0xffffffff)
        return STATUS_INVALID_PARAMETER;
    for (new i = in[4]; i < 9; i++) {
        if (in[5 + i] != 0)
            return STATUS_INVALID_PARAMETER;
    }
    // The transfer implementation retains the controller's existing address,
    // protocol and direction validation. This API adds no arbitrary PCI target.
    if (in[5] < 0 || in[5] > I2C_SMBUS_ADDR_MAX ||
        (in[6] != I2C_SMBUS_READ && in[6] != I2C_SMBUS_WRITE))
        return STATUS_INVALID_PARAMETER;

    new bus = OBSERVED_PCI_BUS;
    new device = OBSERVED_PCI_DEVICE;
    new function = OBSERVED_PCI_FUNCTION;
    new bdf = (bus << 8) | (device << 3) | function;
    if (in[2] != bdf)
        return STATUS_INVALID_PARAMETER;

    for (new i; i < 21; i++)
        out[i] = 0;
    out[0] = 1;
    out[1] = in[1];
    out[2] = bdf;
    out[3] = OBSERVED_PCI_MASK;
    // Nonattempted phases have explicit failure sentinels and clear flags.
    out[5] = _:STATUS_DEVICE_NOT_READY;
    out[7] = _:STATUS_DEVICE_NOT_READY;
    out[9] = _:STATUS_DEVICE_NOT_READY;
    out[10] = _:STATUS_DEVICE_NOT_READY;
    out[12] = _:STATUS_DEVICE_NOT_READY;

    new ids;
    out[4] |= 1;
    new NTSTATUS:status = pci_config_read_dword(bus, device, function, 0, ids);
    out[5] = _:status;
    if (!NT_SUCCESS(status))
        return STATUS_SUCCESS;
    out[6] = ids;
    if (ids != in[3])
        return STATUS_SUCCESS;

    new original;
    out[4] |= 2;
    status = pci_config_read_word(bus, device, function, PCICMD, original);
    out[7] = _:status;
    if (!NT_SUCCESS(status))
        return STATUS_SUCCESS;
    out[8] = original;
    if ((original & OBSERVED_PCI_MASK) == 0)
        return STATUS_SUCCESS;

    new request[9];
    new result[5];
    for (new i; i < 9; i++)
        request[i] = in[5 + i];
    for (new i; i < 5; i++)
        result[i] = 0;
    out[4] |= 4;
    new NTSTATUS:primary = smbus_xfer_impl(request, in[4], result, 5, true);
    out[9] = _:primary;

    // Independent observations run even when the main transfer failed.
    out[4] |= 8;
    new NTSTATUS:identity_status = pci_config_read_dword(bus, device, function, 0, ids);
    out[10] = _:identity_status;
    if (NT_SUCCESS(identity_status))
        out[11] = ids;

    new final_command;
    out[4] |= 16;
    new NTSTATUS:command_status = pci_config_read_word(bus, device, function, PCICMD, final_command);
    out[12] = _:command_status;
    if (NT_SUCCESS(command_status))
        out[13] = final_command;

    // No PCI Command write occurred on this path. This checks observations;
    // it does not claim atomic exclusion of firmware or other kernel clients.
    out[14] = NT_SUCCESS(identity_status) && ids == in[3] &&
        NT_SUCCESS(command_status) && final_command == original;
    if (NT_SUCCESS(primary)) {
        out[15] = 5;
        for (new i; i < 5; i++)
            out[16 + i] = result[i];
    }
    return STATUS_SUCCESS;
}
