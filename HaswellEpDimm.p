// SPDX-License-Identifier: LGPL-2.1-or-later
// Copyright (C) 2026 Resource Monitor contributors.
// This file is distributed under the GNU Lesser General Public License,
// version 2.1 or (at your option) any later version. See COPYING.
//
// Restricted, read-only Haswell-EP DIMM thermal snapshot.
// Register definitions: Intel Xeon E5 v3 Datasheet Volume 2, sections
// 2.1.13, 2.1.15-16, 2.3.2, 2.4.2, and 2.4.8.
// The module does not perform SMBus transactions, map memory, write PCI/MSR
// registers, enable polling, reset a controller, or modify fan controls.
//
// This unsigned source/build must be reviewed and signed by a signer trusted
// by the official PawnIO driver before normal hardware execution is possible.
// Do not change Windows/PawnIO security settings to load it.
#include <pawnio.inc>

#define ABI_VERSION 1
#define SNAPSHOT_CELLS 66
#define FIXED_BUS 0xFF
#define DEVICE_COUNT 10

// Fixed PCI topology observed on this single-socket E5-2696 v3 machine.
// 0..3: per-channel thermal functions.
// 4..7: corresponding per-channel DIMM technology functions.
// 8..9: IMC control/status functions, one per memory controller.
new const device_numbers[DEVICE_COUNT] = [20, 20, 23, 23, 19, 19, 22, 22, 19, 22];
new const function_numbers[DEVICE_COUNT] = [0, 1, 0, 1, 2, 3, 2, 3, 0, 0];
new const expected_ids[DEVICE_COUNT] = [
    0x2FB08086, 0x2FB18086, 0x2FD08086, 0x2FD18086,
    0x2FAA8086, 0x2FAB8086, 0x2F6A8086, 0x2F6B8086,
    0x2FA88086, 0x2F688086
];
new bool:ready = false;
new cpu_fms = 0;

// Internal helper only: neither a device index nor offset comes from callers.
// Keep its allowlist even though all call sites use fixed register constants.
bool:allowed_offset(index, offset) {
    if (index < 0 || index >= DEVICE_COUNT)
        return false;
    if (offset == 0)
        return true;
    if (index < 4) {
        switch (offset) {
            case 0x108, 0x150, 0x154, 0x158:
                return true;
        }
    } else if (index < 8) {
        switch (offset) {
            case 0x80, 0x84, 0x88:
                return true;
        }
    } else {
        switch (offset) {
            case 0x180, 0x188, 0x18C:
                return true;
        }
    }
    return false;
}

NTSTATUS:read_fixed(index, offset, &value) {
    if (!allowed_offset(index, offset))
        return STATUS_ACCESS_DENIED;
    new raw = 0;
    new NTSTATUS:status = pci_config_read_dword(
        FIXED_BUS, device_numbers[index], function_numbers[index], offset, raw);
    if (!NT_SUCCESS(status))
        return status;
    raw &= 0xFFFFFFFF;
    if (raw == 0xFFFFFFFF)
        return STATUS_DEVICE_NOT_READY;
    value = raw;
    return STATUS_SUCCESS;
}

NTSTATUS:check_fixed_devices() {
    for (new index = 0; index < DEVICE_COUNT; ++index) {
        new raw = 0;
        new NTSTATUS:status = read_fixed(index, 0, raw);
        if (!NT_SUCCESS(status))
            return status;
        if (raw != expected_ids[index])
            return STATUS_NO_SUCH_DEVICE;
    }
    return STATUS_SUCCESS;
}

fixed_bdf(index) {
    return (FIXED_BUS << 16) | (device_numbers[index] << 8) | function_numbers[index];
}

/// Obtain a fixed, read-only Haswell-EP DIMM thermal snapshot.
///
/// @param in Unused; must be empty. No addresses or offsets are accepted.
/// @param in_size Must be 0.
/// @param out Exactly 66 cells; PCI dwords are zero-extended to 64 bits.
/// @param out_size Must be 66.
/// @return STATUS_SUCCESS, or a failing NTSTATUS. Discard all output on failure.
///
/// @par ABI version 1
/// Cells 0..1: ABI version, CPU FMS (family<<16 | model<<8 | stepping).
/// Two controller records, base = 2 + mc*8 (mc = 0,1):
/// +0 BDF, +1 VID/DID, +2 SMBCNTL before, +3 SMBSTAT before,
/// +4 poll timer before, +5 SMBCNTL after, +6 SMBSTAT after, +7 timer after.
/// Four channel records, base = 18 + channel*12:
/// +0 thermal BDF, +1 thermal VID/DID, +2 DIMMMTR BDF, +3 DIMMMTR VID/DID,
/// +4 CHN_TEMP_CFG before, +5 DIMMMTR0, +6 DIMMTEMPSTAT0,
/// +7 DIMMMTR1, +8 DIMMTEMPSTAT1, +9 DIMMMTR2, +10 DIMMTEMPSTAT2,
/// +11 CHN_TEMP_CFG after. Channels are MC0/CH0, MC0/CH1, MC1/CH0, MC1/CH1.
/// BDF encoding is (bus<<16)|(device<<8)|function. VID/DID is (DID<<16)|VID.
///
/// @warning Raw registers are not validated temperatures. Hosts must validate
/// populated DDR3 slots (DIMMMTR bits14=1,20=0), direct CLTT source and write
/// lock (CHN_TEMP_CFG bits29=1,28=1), active polling (SMBCNTL bit8), matching
/// TSOD-present bits, no reset (SMBCNTL bit10), and no SMBSTAT bit29 error.
/// Compare configuration before/after, reject unavailable/default samples,
/// and clear cached values on failed acquisition. Temperature uses the low
/// byte, range 0..127 C. A first 85 C is ambiguous until independently verified.
/// The poll timer is not a sample sequence number or conversion timestamp.
///
/// @note No register is written, no SMBus transaction is initiated, and no
/// hardware polling configuration is changed. Signed-module hardware testing
/// and comparison with an independent sensor reader are still outstanding.
DEFINE_IOCTL_SIZED(ioctl_read_snapshot, 0, SNAPSHOT_CELLS) {
    // DEFINE_IOCTL_SIZED does not enforce a zero-size input, so do it here.
    if (in_size != 0 || out_size != SNAPSHOT_CELLS)
        return STATUS_INVALID_PARAMETER;
    if (!ready)
        return STATUS_DEVICE_NOT_READY;
    new NTSTATUS:status = check_fixed_devices();
    if (!NT_SUCCESS(status))
        return status;
    for (new i = 0; i < SNAPSHOT_CELLS; ++i)
        out[i] = 0;
    out[0] = ABI_VERSION;
    out[1] = cpu_fms;

    // Take both controller status/configuration snapshots before any channel.
    for (new mc = 0; mc < 2; ++mc) {
        new base = 2 + mc * 8;
        new index = 8 + mc;
        out[base] = fixed_bdf(index);
        out[base + 1] = expected_ids[index];
        status = read_fixed(index, 0x188, out[base + 2]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(index, 0x180, out[base + 3]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(index, 0x18C, out[base + 4]);
        if (!NT_SUCCESS(status)) return status;
    }

    for (new channel = 0; channel < 4; ++channel) {
        new base = 18 + channel * 12;
        new mtr_index = 4 + channel;
        out[base] = fixed_bdf(channel);
        out[base + 1] = expected_ids[channel];
        out[base + 2] = fixed_bdf(mtr_index);
        out[base + 3] = expected_ids[mtr_index];
        status = read_fixed(channel, 0x108, out[base + 4]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(mtr_index, 0x80, out[base + 5]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(channel, 0x150, out[base + 6]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(mtr_index, 0x84, out[base + 7]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(channel, 0x154, out[base + 8]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(mtr_index, 0x88, out[base + 9]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(channel, 0x158, out[base + 10]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(channel, 0x108, out[base + 11]);
        if (!NT_SUCCESS(status)) return status;
    }

    // Read status first, then configuration: any observed stop/error invalidates
    // the host sample. No event bits are cleared by this read-only module.
    for (new mc = 0; mc < 2; ++mc) {
        new base = 2 + mc * 8;
        new index = 8 + mc;
        status = read_fixed(index, 0x180, out[base + 6]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(index, 0x188, out[base + 5]);
        if (!NT_SUCCESS(status)) return status;
        status = read_fixed(index, 0x18C, out[base + 7]);
        if (!NT_SUCCESS(status)) return status;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    ready = false;
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;
    cpu_fms = get_cpu_fms();
    if (cpu_fms_family(cpu_fms) != 6 || cpu_fms_model(cpu_fms) != 0x3F)
        return STATUS_NOT_SUPPORTED;
    new NTSTATUS:status = check_fixed_devices();
    if (!NT_SUCCESS(status))
        return status;
    ready = true;
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    ready = false;
    return STATUS_SUCCESS;
}
