// Fixed Intel host-bridge capability reader; research candidate.
// SPDX-License-Identifier: LGPL-2.1-or-later
#include <pawnio.inc>

#define MAGIC 0x004d434150494432
#define WORDS 32
#define CAP_A 0xE4
#define CAP_B 0xE8
#define CAP_C 0xEC
#define CAP_E 0xF0

new g_sequence;
new bool:g_active;
new g_vendor_device;

put_dword(out[WORDS], offset, value) {
    // Each output cell is eight bytes. Advance the byte position, not the cell.
    for (new byte_index; byte_index < 4; byte_index++) {
        new position = offset + byte_index;
        out[16 + position / 8] |= ((value >>> (byte_index * 8)) & 255) << ((position % 8) * 8);
    }
}

NTSTATUS:identity(&value) {
    new NTSTATUS:status = pci_config_read_dword(0, 0, 0, 0, value);
    if (!NT_SUCCESS(status)) return status;
    if ((value & 0xffff) != 0x8086 || (value >>> 16) == 0) return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}

/// Read the four fixed Intel host-bridge capability dwords at 00:00.0.
///
/// This operation has no PCI/MMIO writes, arbitrary offsets, or tuning controls.
/// Raw values have generation-specific meanings; the module does not decode them.
///
/// @param in [0] sequence (starts at 1 and increments), [1..3] must be zero.
/// @param in_size Must be 4 cells.
/// @param out [0] magic, [1] schema 1, [2..5] input echo, [6] primary NTSTATUS,
///            [7] cleanup status (zero; no selector state is modified),
///            [8] completion-confirmed, [9] stage, [10] vendor/device,
///            [11] BDF (zero), [12..13] identity before/after, [14] payload
///            length (16), [15] reserved; [16..17] packed A/B/C/E dwords.
///            All remaining cells are zero; failure discards the entire payload.
/// @param out_size Must be 32 cells (256 bytes).
/// @return STATUS_SUCCESS means a receipt was returned. The caller must check
///         the embedded primary status, identity equality, and complete shape.
DEFINE_IOCTL_SIZED(ioctl_capid, 4, WORDS) {
    new sequence = in[0];
    if (g_active || sequence != g_sequence + 1 || sequence > 0xffffffff ||
        in[1] != 0 || in[2] != 0 || in[3] != 0)
        return STATUS_INVALID_PARAMETER;

    for (new i; i < WORDS; i++) out[i] = 0;
    g_sequence = sequence;
    g_active = true;

    new stage = 1;
    new before, after;
    new NTSTATUS:primary = identity(before);
    if (NT_SUCCESS(primary) && before != g_vendor_device)
        primary = STATUS_DEVICE_CONFIGURATION_ERROR;

    new values[4];
    new offsets[4] = [CAP_A, CAP_B, CAP_C, CAP_E];
    if (NT_SUCCESS(primary)) {
        stage = 10;
        for (new i; i < 4 && NT_SUCCESS(primary); i++)
            primary = pci_config_read_dword(0, 0, 0, offsets[i], values[i]);
    }
    if (NT_SUCCESS(primary)) {
        primary = identity(after);
        if (NT_SUCCESS(primary) && after != before)
            primary = STATUS_DEVICE_CONFIGURATION_ERROR;
    }

    out[0] = MAGIC;
    out[1] = 1;
    out[2] = sequence;
    out[6] = _:primary;
    out[8] = 1;
    out[9] = stage;
    out[10] = before;
    out[12] = before;
    out[13] = after;
    out[14] = 16;
    if (NT_SUCCESS(primary)) {
        for (new i; i < 4; i++) put_dword(out, i * 4, values[i]);
    }

    g_active = false;
    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    return g_active ? STATUS_DEVICE_BUSY : STATUS_SUCCESS;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;
    return identity(g_vendor_device);
}
