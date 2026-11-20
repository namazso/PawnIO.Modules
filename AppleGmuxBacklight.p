// SPDX-License-Identifier: LGPL-2.1-or-later
#include <pawnio.inc>
#include <registry.inc>

const GMUX_VERSION_MAJOR = 0x704;
const GMUX_VERSION_MINOR = 0x705;
const GMUX_VERSION_RELEASE = 0x706;
const GMUX_MAXIMUM = 0x770;
const GMUX_BRIGHTNESS = 0x774;
const BRIGHTNESS_MASK = 0x00FFFFFF;
const EXPECTED_MAXIMUM = 142711;

bool:matches(const value[], const expected[]) {
    for (new i = 0; ; i++) {
        if (value[i] != expected[i])
            return false;
        if (expected[i] == 0)
            return true;
    }
}

bool:is_supported_mac() {
    if (!reg_resolve_imports())
        return false;

    new path[] = ''\\Registry\\Machine\\SYSTEM\\CurrentControlSet\\Control\\SystemInformation'';
    new manufacturer[64], model[64], length;
    if (reg_query_sz(path, ''SystemManufacturer'', manufacturer, sizeof manufacturer, length) != STATUS_SUCCESS)
        return false;
    if (reg_query_sz(path, ''SystemProductName'', model, sizeof model, length) != STATUS_SUCCESS)
        return false;

    return matches(manufacturer, ''Apple Inc.'') && matches(model, ''MacBookPro8,3'');
}

bool:has_expected_gmux() {
    return io_in_byte(GMUX_VERSION_MAJOR) == 1
        && io_in_byte(GMUX_VERSION_MINOR) == 9
        && io_in_byte(GMUX_VERSION_RELEASE) == 36
        && (io_in_dword(GMUX_MAXIMUM) & BRIGHTNESS_MASK) == EXPECTED_MAXIMUM;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64 || get_cpu_vendor() != CpuVendor_Intel)
        return STATUS_NOT_SUPPORTED;
    if (!is_supported_mac() || !has_expected_gmux())
        return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}

/// out[0] = current brightness; out[1] = maximum brightness.
DEFINE_IOCTL_SIZED(ioctl_read_brightness, 0, 2) {
    if (!has_expected_gmux())
        return STATUS_DEVICE_NOT_READY;

    new current = io_in_dword(GMUX_BRIGHTNESS) & BRIGHTNESS_MASK;
    if (current > EXPECTED_MAXIMUM)
        return STATUS_DEVICE_HARDWARE_ERROR;

    out[0] = current;
    out[1] = EXPECTED_MAXIMUM;
    return STATUS_SUCCESS;
}

/// in[0] = brightness from 10% to 100%; out[0] = verified readback.
DEFINE_IOCTL_SIZED(ioctl_set_brightness, 1, 1) {
    new target = in[0];
    if (target < EXPECTED_MAXIMUM / 10 || target > EXPECTED_MAXIMUM)
        return STATUS_INVALID_PARAMETER;
    if (!has_expected_gmux())
        return STATUS_DEVICE_NOT_READY;

    io_out_dword(GMUX_BRIGHTNESS, target);
    out[0] = io_in_dword(GMUX_BRIGHTNESS) & BRIGHTNESS_MASK;
    return out[0] == target ? STATUS_SUCCESS : STATUS_DEVICE_HARDWARE_ERROR;
}
