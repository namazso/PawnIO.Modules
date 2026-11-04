// SPDX-License-Identifier: LGPL-2.1-or-later
// Candidate for maintainer review, not enabled or loaded on hardware.
// Explicit opt-in recovery for the observed ASUS 1043:88EF / Intel 8086:7F23.
// The caller MUST hold Global\Access_SMBUS.HTP.Method across this call.
// A quiet HSTS byte does NOT prove BIOS/ACPI has abandoned its semaphore.
// Thus this cannot be enabled automatically based on a timeout alone.
// No command/address/data/PCI control registers are written here.

#define I801_RECOVERY_ACK 0x4938303152454356
#define I801_RECOVERY_QUIET_MS 250

new bool:i801_recovery_attempted = false;

NTSTATUS:i801_startup_recovery(ack, &result, &last_hststs)
{
    result = 0;
    last_hststs = -1;
    if (ack != I801_RECOVERY_ACK)
        return STATUS_ACCESS_DENIED;
    if (i801_recovery_attempted)
        return STATUS_ACCESS_DENIED;

    new ids;
    new NTSTATUS:status = pci_config_read_dword(pci_addr[0], pci_addr[1], pci_addr[2], 0x00, ids);
    if (!NT_SUCCESS(status))
        return status;
    if (ids != 0x7F238086)
        return STATUS_NOT_SUPPORTED;
    status = pci_config_read_dword(pci_addr[0], pci_addr[1], pci_addr[2], 0x2C, ids);
    if (!NT_SUCCESS(status))
        return status;
    if (ids != 0x88EF1043)
        return STATUS_NOT_SUPPORTED;

    // Do not change memory decoding or power state as part of recovery.
    status = pci_config_read_word(pci_addr[0], pci_addr[1], pci_addr[2], PCICMD, ids);
    if (!NT_SUCCESS(status))
        return status;
    if (!(ids & PCICMD_MMBIT))
        return STATUS_DEVICE_NOT_READY;

    i801_recovery_attempted = true;
    new deadline = get_tick_count() + I801_RECOVERY_QUIET_MS;
    new bool:waiting = true;
    do {
        status = virtual_read_byte(SMBHSTSTS, last_hststs);
        if (!NT_SUCCESS(status))
            return status;

        if (!(last_hststs & SMBHSTSTS_INUSE_STS)) {
            // This read acquired the semaphore. Release only our own bit.
            status = virtual_write_byte(SMBHSTSTS, SMBHSTSTS_INUSE_STS);
            if (!NT_SUCCESS(status))
                return status;
            result = 1; // No forced recovery was needed.
            return STATUS_SUCCESS;
        }

        // Only INUSE may be set. Busy, pending completion, error, ALERT,
        // BYTE_DONE, inaccessible MMIO (0xFF), or changing status -> refuse.
        if (last_hststs != SMBHSTSTS_INUSE_STS)
            return STATUS_DEVICE_BUSY;
        waiting = get_tick_count() < deadline;
        if (waiting)
            microsleep(1000);
    } while (waiting);

    // Authorized forced semaphore clear, never a controller KILL/reset.
    status = virtual_write_byte(SMBHSTSTS, SMBHSTSTS_INUSE_STS);
    if (!NT_SUCCESS(status))
        return status;
    result = 2; // Clear issued; caller must still acquire normally afterwards.
    return STATUS_SUCCESS;
}

/// Explicit experimental recovery of persistent i801 INUSE on 8086:7F23/1043:88EF.
///
/// Normal initialization and transfers never call this operation. It permits
/// one attempt per module instance after platform/decode checks pass. No caller
/// application is privileged; the board gate limits hardware scope only.
/// A read returning INUSE=0 acquires the semaphore and releases our own bit.
/// Otherwise, only a byte equal to 0x40 throughout a 250 ms polling interval
/// permits a forced write-one-to-clear of INUSE. Other status bits refuse it.
///
/// @param in [0] = I801_RECOVERY_ACK (0x4938303152454356), acknowledgement of the
///                forced-semaphore-clear risk; this is not authentication.
/// @param in_size Must be 1.
/// @param out [0] = 0 on failure, 1 when our normally acquired semaphore was
///                 released, 2 when a forced clear was issued.
///            [1] = Last sampled HSTS byte, or -1 if no sample was attempted.
///                  Ignore both outputs on failure; failed native reads need
///                  not leave a valid sample. Result 2 is not proof of recovery.
/// @param out_size Must be 2.
/// @return STATUS_SUCCESS on a completed release; STATUS_ACCESS_DENIED for an
///         incorrect acknowledgement or repeated attempt; STATUS_NOT_SUPPORTED
///         for other hardware; STATUS_DEVICE_NOT_READY if MMIO decode is off;
///         STATUS_DEVICE_BUSY if additional status bits are set while INUSE is
///         held; otherwise the underlying PCI/MMIO failure status.
/// @warning The caller must hold "\\BaseNamedObjects\\Access_SMBUS.HTP.Method"
///          throughout this call and must reacquire the hardware semaphore via
///          the normal transfer API afterwards. This module cannot verify the
///          Windows mutex; BIOS/ACPI and other drivers may not honor it.
/// @warning Quiet status does not prove abandoned ownership. Another owner can
///          retain INUSE without HOST_BUSY and can race the final status sample.
///          This candidate is unvalidated on hardware and must not be enabled
///          for unattended startup without an accepted coexistence policy.
/// @note Module reload resets the attempt limit. No controller KILL, address,
///       data, PCI-control or power-state writes are performed by this ioctl.
DEFINE_IOCTL_SIZED(ioctl_startup_recovery, 1, 2) {
    return i801_startup_recovery(in[0], out[0], out[1]);
}
