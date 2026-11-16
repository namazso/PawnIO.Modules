// SPDX-License-Identifier: LGPL-2.1-or-later
// Independently authored fixed AMD Family1Ah Model44h host BCLK report reader.
// Source-only candidate. NOT signed, loaded, or approved for hardware use.
#pragma semicolon 1
#include <pawnio.inc>

const HB_CELLS = 32;
const HB_MASK32 = 0xFFFFFFFF;
const HB_C4 = 0xC4;
const HB_C8 = 0xC8;
const HB_REPORT = 0x6C;
const HB_CAPTURE_READY = (1 << 6);

hb_put(out[HB_CELLS], index, value) {
    out[index] = value;
    out[3] |= (1 << index);
}

hb_fail(out[HB_CELLS], status, stage) {
    if (out[2] == 0) {
        out[1] = status & HB_MASK32;
        out[2] = stage;
    }
}

bool:hb_read(out[HB_CELLS], bus, offset, field, stage) {
    out[4]++;
    new value = 0;
    new NTSTATUS:status = pci_config_read_dword(bus, 0, 0, offset, value);
    hb_put(out, 30, (_:status) & HB_MASK32);
    if (status != STATUS_SUCCESS) {
        hb_fail(out, _:status, stage);
        return false;
    }
    // Retain a completed raw cell before rejecting an impossible wide DWORD.
    hb_put(out, field, value);
    if (value < 0 || value > HB_MASK32) {
        hb_fail(out, _:STATUS_DATA_ERROR, stage);
        return false;
    }
    return true;
}

bool:hb_write_c4(out[HB_CELLS], value, stage) {
    out[4]++;
    new NTSTATUS:status = pci_config_write_dword(0, 0, 0, HB_C4, value);
    hb_put(out, 30, (_:status) & HB_MASK32);
    if (status != STATUS_SUCCESS) {
        hb_fail(out, _:status, stage);
        return false;
    }
    return true;
}

bool:hb_identity(out[HB_CELLS], bus, first, stage) {
    if (!hb_read(out, bus, 0, first, stage))
        return false;
    new initial_id = out[first];
    if (!hb_read(out, bus, 8, first + 1, stage + 1)
        || !hb_read(out, bus, 0x0C, first + 2, stage + 2)
        || !hb_read(out, bus, 0, first, stage + 3))
        return false;
    if (out[first] != initial_id) {
        hb_fail(out, _:STATUS_DEVICE_NOT_READY, stage + 4);
        return false;
    }
    // Separate defensive host policy: vendor AMD, class0600, Type0 header.
    if ((out[first] & 0xFFFF) != 0x1022 || out[first] == HB_MASK32
        || (out[first + 1] >> 16) != 0x0600
        || ((out[first + 2] >> 16) & 0x7F) != 0) {
        hb_fail(out, _:STATUS_NOT_SUPPORTED, stage + 3);
        return false;
    }
    return true;
}

bool:hb_same_initial(out[HB_CELLS], stage) {
    if (!hb_identity(out, 0, 11, stage))
        return false;
    if (out[11] != out[8] || out[12] != out[9] || out[13] != out[10]) {
        hb_fail(out, _:STATUS_DEVICE_NOT_READY, stage + 3);
        return false;
    }
    return true;
}

bool:hb_discovery(out[HB_CELLS], bool:after) {
    new first = after ? 27 : 25;
    new field = after ? 16 : 15;
    new stage = after ? 60 : 20;
    new shift = after ? 3 : 0;
    new bool:dirty = false;
    new bool:good = false;
    for (new once = 1; once != 0; once = 0) {
        if (!hb_same_initial(out, stage))
            break;
        if (!hb_read(out, 0, HB_C4, first, stage + 3))
            break;
        // A failed write may have taken effect: cleanup is still mandatory.
        dirty = true;
        out[5] |= (1 << shift);
        if (!hb_write_c4(out, out[14], stage + 4))
            break;
        if (!hb_read(out, 0, HB_C8, field, stage + 5))
            break;
        good = true;
    }
    if (dirty) {
        // Never restore a captured selector into an unknown/replaced endpoint.
        if (!hb_same_initial(out, stage + 6))
            return false;
        out[5] |= (2 << shift);
        if (!hb_write_c4(out, out[first], stage + 9)
            || !hb_read(out, 0, HB_C4, first + 1, stage + 10))
            return false;
        if (out[first + 1] != out[first]) {
            hb_fail(out, _:STATUS_DEVICE_NOT_READY, stage + 11);
            return false;
        }
        out[5] |= (4 << shift);
    }
    return good;
}

bool:hb_cpu(out[HB_CELLS], bool:after) {
    new stage = after ? 90 : 1;
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_AMD) {
        hb_fail(out, _:STATUS_NOT_SUPPORTED, stage);
        return false;
    }
    new fms = get_cpu_fms();
    hb_put(out, after ? 7 : 6, fms);
    if (cpu_fms_family(fms) != 0x1A || cpu_fms_model(fms) != 0x44
        || (after && out[7] != out[6])) {
        hb_fail(out, _:STATUS_NOT_SUPPORTED, stage);
        return false;
    }
    return true;
}

hb_capture(out[HB_CELLS]) {
    for (new i = 0; i < HB_CELLS; i++)
        out[i] = 0;
    hb_put(out, 0, 1);
    out[1] = (_:STATUS_DEVICE_NOT_READY) & HB_MASK32;
    for (new once = 1; once != 0; once = 0) {
        if (!hb_cpu(out, false) || !hb_identity(out, 0, 8, 10))
            break;
        new did = out[8] >> 16;
        hb_put(out, 14, (did == 0x1507 || did == 0x1122) ? 0x13B10054 : 0x13B10044);
        if (!hb_discovery(out, false))
            break;
        new segment = 0;
        new bus = 0;
        if (out[15] != HB_MASK32 && (out[15] & (1 << 8)) && (out[15] & 0xFF)) {
            segment = (out[15] >> 16) & 0xFFFF;
            bus = out[15] & 0xFF;
        }
        hb_put(out, 17, segment);
        hb_put(out, 18, bus);
        if (segment != 0) {
            hb_fail(out, _:STATUS_NOT_SUPPORTED, 40);
            break;
        }
        // A decoded remap is never silently truncated or replaced by bus0.
        // Fail closed if the exact endpoint cannot pass the typed host policy.
        if (!hb_identity(out, bus, 19, 41))
            break;
        if (bus != 0)
            out[5] |= (1 << 7);
        if (!hb_read(out, bus, HB_REPORT, 29, 50))
            break;
        if (!hb_discovery(out, true))
            break;
        if (out[16] != out[15] || out[27] != out[25]) {
            hb_fail(out, _:STATUS_DEVICE_NOT_READY, 80);
            break;
        }
        if (!hb_identity(out, bus, 22, 81))
            break;
        if (out[22] != out[19] || out[23] != out[20] || out[24] != out[21]) {
            hb_fail(out, _:STATUS_DEVICE_NOT_READY, 84);
            break;
        }
        if (!hb_same_initial(out, 85) || !hb_cpu(out, true))
            break;
        if ((out[5] & 0x3F) != 0x3F) {
            hb_fail(out, _:STATUS_DEVICE_NOT_READY, 95);
            break;
        }
        out[5] |= HB_CAPTURE_READY;
        out[1] = 0;
        out[2] = 0;
    }
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;
    new fms = get_cpu_fms();
    if (cpu_fms_family(fms) != 0x1A || cpu_fms_model(fms) != 0x44)
        return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}

/// Caller holds \\BaseNamedObjects\\Access_PCI for this entire call.
/// Cooperative serialization only; no firmware/SMM or hardware-atomic claim.
/// STATUS_SUCCESS means diagnostics copied, NOT an available BCLK value.
/// Require inner status0, capture-ready+cleanup flags, all required presence
/// bits and host-side typed report decoder. Missing marker has no fallback.
/// A VM/native abort means cleanup is UNKNOWN, regardless of partial cells.
DEFINE_IOCTL_SIZED(ioctl_read_host_bclk_report, 0, 32) {
    // The sized macro deliberately ignores a zero expected input size.
    // Enforce both lengths explicitly before ANY native/CPU/PCI operation.
    if (in_size != 0 || out_size != HB_CELLS)
        return STATUS_INVALID_PARAMETER;
    hb_capture(out);
    return STATUS_SUCCESS;
}
