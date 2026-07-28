//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Valve LEDs driver
//  Copyright (C) 2026 Adam Honse <calcprogrammer1@gmail.com>
//  Based on Linux leds-valve.c, Copyright (C) 2025 Valve Corporation
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
#include <registry.inc>

// Valve LEDs driver constants
const NUM_LEDS                      = 17
const NUM_COMPONENTS                = 3

const BIOS_CTRL_ENABLE_BIT          = BIT(0)
const BRIGHTNESS_CTRL_SCALE_BIT     = BIT(0)
const BRIGHTNESS_CTRL_PWRBTN_BIT    = BIT(1)

const PORT_EC_CMD                   = 0x6c
const EC_CMD_COMMIT_SETTINGS        = 0xc6

const PORT_BASE                     = 0xde8
const PORT_BIOS_LED_CTRL            = (0xde8 - PORT_BASE)
const PORT_INTENSITY_STARTUP        = (0xde9 - PORT_BASE)
const PORT_BRIGHTNESS_STARTUP       = (0xdec - PORT_BASE)
const PORT_STRIP_ENABLE             = (0xdef - PORT_BASE)
const PORT_INTENSITY                = (0xe39 - PORT_BASE)
const PORT_MODE                     = (0xe6c - PORT_BASE)
const PORT_DELAY                    = (0xe6e - PORT_BASE)
const PORT_BREATH_OFFSET            = (0xe6f - PORT_BASE)
const PORT_BREATH_LEVEL             = (0xe70 - PORT_BASE)
const PORT_PATROL_NUM               = (0xe71 - PORT_BASE)
const PORT_COLOR_SHIFT              = (0xe75 - PORT_BASE)
const PORT_BRIGHTNESS_CTRL          = (0xe78 - PORT_BASE)
const PORT_BRIGHTNESS_SCALE         = (0xe79 - PORT_BASE)
const PORT_BRIGHTNESS_PWRBTN        = (0xe7a - PORT_BASE)
const NR_PORTS                      = (PORT_BRIGHTNESS_PWRBTN + 1)

const PORT_STRIDE                   = 3
#define LED_PORT(%0)                (PORT_INTENSITY + (%0) * PORT_STRIDE)

const EffectMode: {
    MODE_PATROL                     = 0,
    MODE_BREATH                     = 1,
    MODE_FACTORY                    = 2,
    MODE_NORMAL                     = 3,
    MODE_OFF                        = 4,
    MODE_RAINBOW                    = 5,
    MODE_DEMO                       = 6,
    MODE_MANUAL                     = 7,
};

/// Compare two NUL-terminated codepoint strings
///
/// @param str1 Unpacked string (one codepoint per cell)
/// @param str2 Unpacked string (one codepoint per cell)
/// @return 0 if equal, nonzero otherwise
stock str_eq(const str1[], const str2[]) {
    new i = 0;
    while (str1[i] != 0) {
        new c = str2[i];
        if (c == 0)
            return 1; // str2 ended before str1
        if (str1[i] != c)
            return 1; // mismatch
        i++;
    }
    // str1 ended; check that str2 also ended
    return str2[i] != 0 ? 1 : 0;
}

/// Send EC command
leds_send_ec_cmd(cmd) {
    io_out_byte(PORT_EC_CMD, cmd);
}

/// Check if we have persistence (BIOS control enabled)
leds_has_persistence() {
    new val = io_in_byte(PORT_BASE + PORT_BIOS_LED_CTRL);
    return (val & BIOS_CTRL_ENABLE_BIT) != 0;
}

/// Ensure persistence is enabled for settings that need to be saved
NTSTATUS:leds_ensure_persistence() {
    new val = io_in_byte(PORT_BASE + PORT_BIOS_LED_CTRL);
    if ((val & BIOS_CTRL_ENABLE_BIT) == 0) {
        io_out_byte(PORT_BASE + PORT_BIOS_LED_CTRL, val | BIOS_CTRL_ENABLE_BIT);
    }

    // Re-check after attempting to enable
    return leds_has_persistence() ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
}

/// Initialize the Valve LEDs module
///
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_init, 0, 0) {
    // Initialize brightness control
    new brightness_ctrl = io_in_byte(PORT_BASE + PORT_BRIGHTNESS_CTRL);
    brightness_ctrl |= BRIGHTNESS_CTRL_SCALE_BIT;
    io_out_byte(PORT_BASE + PORT_BRIGHTNESS_CTRL, brightness_ctrl);

    return STATUS_SUCCESS;
}

/// Get the enabled state
///
/// @param out [0] = Enabled (1) or disabled (0)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_enabled, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_STRIP_ENABLE);
    return STATUS_SUCCESS;
}

/// Set the enabled state
///
/// @param in [0] = Enabled (1) or disabled (0)
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_enabled, 1, 0) {
    new enabled = in[0] ? 1 : 0;
    io_out_byte(PORT_BASE + PORT_STRIP_ENABLE, enabled);
    return STATUS_SUCCESS;
}

/// Get the effect mode
///
/// @param out [0] = Effect mode (see EffectMode enum)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_effect, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_MODE);
    return STATUS_SUCCESS;
}

/// Set the effect mode
///
/// @param in [0] = Effect mode (see EffectMode enum)
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_effect, 1, 0) {
    new mode = in[0];
    if (mode < 0 || mode > 7)
        return STATUS_INVALID_PARAMETER;
    
    io_out_byte(PORT_BASE + PORT_MODE, mode);
    
    return STATUS_SUCCESS;
}

/// Get the startup brightness
///
/// @param out [0] = Startup brightness value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_brightness_startup, 0, 1) {
    if (leds_has_persistence()) {
        out[0] = io_in_byte(PORT_BASE + PORT_BRIGHTNESS_STARTUP);
    } else {
        out[0] = 56; // BRIGHTNESS_HALF
    }
    
    return STATUS_SUCCESS;
}

/// Set the startup brightness (persisted across boot)
///
/// @param in [0] = Brightness value (0-255)
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_brightness_startup, 1, 0) {
    new NTSTATUS:status = leds_ensure_persistence();
    if (status != STATUS_SUCCESS)
        return status;
    
    io_out_byte(PORT_BASE + PORT_BRIGHTNESS_STARTUP, in[0] & 0xFF);
    leds_send_ec_cmd(EC_CMD_COMMIT_SETTINGS);
    
    return STATUS_SUCCESS;
}

/// Get startup color (RGB)
///
/// @param out [0] = Red, [1] = Green, [2] = Blue
/// @param out_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_startup_color, 0, 3) {
    if (leds_has_persistence()) {
        out[0] = io_in_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 0);
        out[1] = io_in_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 1);
        out[2] = io_in_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 2);
    } else {
        // Default: pure blue
        out[0] = 0;
        out[1] = 0;
        out[2] = 255;
    }
    
    return STATUS_SUCCESS;
}

/// Set startup color (RGB, persisted across boot)
///
/// @param in [0] = Red, [1] = Green, [2] = Blue
/// @param in_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_startup_color, 3, 0) {
    new NTSTATUS:status = leds_ensure_persistence();
    if (status != STATUS_SUCCESS)
        return status;
    
    io_out_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 0, in[0] & 0xFF);
    io_out_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 1, in[1] & 0xFF);
    io_out_byte(PORT_BASE + PORT_INTENSITY_STARTUP + 2, in[2] & 0xFF);
    leds_send_ec_cmd(EC_CMD_COMMIT_SETTINGS);
    
    return STATUS_SUCCESS;
}

/// Get LED color (RGB)
///
/// @param in [0] = LED index (0-16)
/// @param in_size Must be 1
/// @param out [0] = Red, [1] = Green, [2] = Blue
/// @param out_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_led_color, 1, 3) {
    new led = in[0];
    if (led >= NUM_LEDS)
        return STATUS_INVALID_PARAMETER;
    
    for (new comp = 0; comp < NUM_COMPONENTS; comp++) {
        out[comp] = io_in_byte(PORT_BASE + LED_PORT(led) + comp);
    }
    
    return STATUS_SUCCESS;
}

/// Set LED color (RGB)
///
/// @param in [0] = LED index (0-16), [1] = Red, [2] = Green, [3] = Blue
/// @param in_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_led_color, 4, 0) {
    new led = in[0];
    if (led >= NUM_LEDS)
        return STATUS_INVALID_PARAMETER;
    
    io_out_byte(PORT_BASE + LED_PORT(led) + 0, in[1] & 0xFF);
    io_out_byte(PORT_BASE + LED_PORT(led) + 1, in[2] & 0xFF);
    io_out_byte(PORT_BASE + LED_PORT(led) + 2, in[3] & 0xFF);
    
    return STATUS_SUCCESS;
}

/// Get LED component count
///
/// @param in [0] = LED index (0-16)
/// @param in_size Must be 1
/// @param out [0] = Number of components (3)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_led_components, 1, 1) {
    new led = in[0];
    if (led >= NUM_LEDS)
        return STATUS_INVALID_PARAMETER;

    out[0] = NUM_COMPONENTS;
    return STATUS_SUCCESS;
}

/// Get LED number
///
/// @param out [0] = Number of LEDs (17)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_led_num, 0, 1) {
    out[0] = NUM_LEDS
    return STATUS_SUCCESS;
}

/// Get delay value
///
/// @param out [0] = Delay value (0-20)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_delay, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_DELAY);
    return STATUS_SUCCESS;
}

/// Set delay value
///
/// @param in [0] = Delay value (0-20)
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_delay, 1, 0) {    
    if (in[0] < 0 || in[0] > 20)
        return STATUS_INVALID_PARAMETER;
    
    io_out_byte(PORT_BASE + PORT_DELAY, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

/// Get breath offset
///
/// @param out [0] = Breath offset value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_breath_offset, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_BREATH_OFFSET);
    return STATUS_SUCCESS;
}

/// Set breath offset
///
/// @param in [0] = Breath offset value
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_breath_offset, 1, 0) {
    io_out_byte(PORT_BASE + PORT_BREATH_OFFSET, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

/// Get breath level
///
/// @param out [0] = Breath level value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_breath_level, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_BREATH_LEVEL);
    return STATUS_SUCCESS;
}

/// Set breath level
///
/// @param in [0] = Breath level value
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_breath_level, 1, 0) {
    io_out_byte(PORT_BASE + PORT_BREATH_LEVEL, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

/// Get patrol number
///
/// @param out [0] = Patrol number value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_patrol_num, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_PATROL_NUM);
    return STATUS_SUCCESS;
}

/// Set patrol number
///
/// @param in [0] = Patrol number value
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_patrol_num, 1, 0) {
    io_out_byte(PORT_BASE + PORT_PATROL_NUM, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

/// Get color shift
///
/// @param out [0] = Color shift value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_color_shift, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_COLOR_SHIFT);
    return STATUS_SUCCESS;
}

/// Set color shift
///
/// @param in [0] = Color shift value
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_color_shift, 1, 0) {
    io_out_byte(PORT_BASE + PORT_COLOR_SHIFT, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

/// Get brightness scale
///
/// @param out [0] = Brightness scale value (0-255)
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_brightness_scale, 0, 1) {
    out[0] = io_in_byte(PORT_BASE + PORT_BRIGHTNESS_SCALE);
    return STATUS_SUCCESS;
}

/// Set brightness scale
///
/// @param in [0] = Brightness scale value (0-255)
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_brightness_scale, 1, 0) {
    io_out_byte(PORT_BASE + PORT_BRIGHTNESS_SCALE, in[0] & 0xFF);
    return STATUS_SUCCESS;
}

NTSTATUS:main() {
    // Only supported on x86-64
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // Check support based on DMI info.  Only OEM:F7F or Valve:Fremont are supported.
    new bios_path[] = ''\\Registry\\Machine\\HARDWARE\\DESCRIPTION\\System\\BIOS'';

    new manufacturer[64];
    new mfg_len = 0;
    if (reg_query_sz(bios_path, ''SystemManufacturer'', manufacturer, sizeof manufacturer, mfg_len) != STATUS_SUCCESS)
        return STATUS_NOT_SUPPORTED;

    new product[64];
    new product_len = 0;
    if (reg_query_sz(bios_path, ''SystemProductName'', product, sizeof product, product_len) != STATUS_SUCCESS)
        return STATUS_NOT_SUPPORTED;

    if (((str_eq(manufacturer, ''OEM'') == 0) && (str_eq(product, ''F7F'') == 0))
     || ((str_eq(manufacturer, ''Valve'') == 0) && (str_eq(product, ''Fremont'') == 0)))
        return STATUS_SUCCESS;
    else
        return STATUS_NOT_SUPPORTED;
}
