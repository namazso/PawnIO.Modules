//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  M-Control Contributors
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

// Legacy system speaker module.
//
// System timer channel 2 generates the square wave. Bits 0 and 1 of system control
// port B connect that signal to the speaker. Other bits in port 0x61 are
// preserved because firmware and other legacy devices may use them.

#define SYSTEM_TIMER_CHANNEL_2_DATA              0x42
#define SYSTEM_TIMER_COMMAND                     0x43
#define SYSTEM_CONTROL_PORT_B           0x61

#define SYSTEM_TIMER_INPUT_HZ                    1193182
#define SYSTEM_TIMER_CHANNEL_2_SQUARE_WAVE       0xB6
#define SYSTEM_TIMER_CHANNEL_2_MODE_0            0xB0
#define SYSTEM_TIMER_CHANNEL_2_OUTPUT            0x20
#define SPEAKER_GATE_ENABLE             0x01
#define SPEAKER_DATA_ENABLE             0x02
#define SPEAKER_GATE_AND_DATA_ENABLE    0x03

// The presence probe runs with speaker data disabled, so the programmed
// 9 kHz system timer signal is not sent to the physical system speaker.
#define SPEAKER_PROBE_HZ                9000
#define SPEAKER_PROBE_SAMPLES           64
#define SPEAKER_PROBE_SAMPLE_US         10
#define SPEAKER_AUDIBLE_PROBE_US        200
#define SPEAKER_WAKE_RESET_US           20000
#define SPEAKER_WAKE_TOGGLE_US          50000

// Keep requests in the useful audible range and prevent a caller from holding
// a PawnIO request indefinitely.
#define SPEAKER_MIN_HZ                  20
#define SPEAKER_MAX_HZ                  20000
#define SPEAKER_MAX_DURATION_MS         60000

bool:is_valid_frequency(frequency) {
    return frequency >= SPEAKER_MIN_HZ && frequency <= SPEAKER_MAX_HZ;
}

bool:is_speaker_enabled() {
    return (io_in_byte(SYSTEM_CONTROL_PORT_B) &
        SPEAKER_GATE_AND_DATA_ENABLE) == SPEAKER_GATE_AND_DATA_ENABLE;
}

bool:check_speaker_control_port() {
    new original = io_in_byte(SYSTEM_CONTROL_PORT_B);
    new silent = original & ~SPEAKER_DATA_ENABLE;
    new gate_off = silent & ~SPEAKER_GATE_ENABLE;
    new gate_on = silent | SPEAKER_GATE_ENABLE;

    io_out_byte(SYSTEM_CONTROL_PORT_B, gate_off);
    new read_off = io_in_byte(SYSTEM_CONTROL_PORT_B);

    io_out_byte(SYSTEM_CONTROL_PORT_B, gate_on);
    new read_on = io_in_byte(SYSTEM_CONTROL_PORT_B);

    // A probe must never change the state owned by firmware or another
    // application after the check has completed.
    io_out_byte(SYSTEM_CONTROL_PORT_B, original);

    return (read_off & SPEAKER_GATE_AND_DATA_ENABLE) ==
            (gate_off & SPEAKER_GATE_AND_DATA_ENABLE) &&
        (read_on & SPEAKER_GATE_AND_DATA_ENABLE) ==
            (gate_on & SPEAKER_GATE_AND_DATA_ENABLE);
}

bool:check_system_timer_channel_2_activity(&transitions) {
    new original = io_in_byte(SYSTEM_CONTROL_PORT_B);
    new divisor = (SYSTEM_TIMER_INPUT_HZ + (SPEAKER_PROBE_HZ / 2)) /
        SPEAKER_PROBE_HZ;

    io_out_byte(SYSTEM_TIMER_COMMAND, SYSTEM_TIMER_CHANNEL_2_SQUARE_WAVE);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, divisor & 0xFF);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, (divisor >> 8) & 0xFF);

    // Enable only the system timer gate. Keeping speaker data disabled makes this an
    // electrically silent test of the legacy timer/speaker interface.
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        (original | SPEAKER_GATE_ENABLE) & ~SPEAKER_DATA_ENABLE);

    new previous = io_in_byte(SYSTEM_CONTROL_PORT_B) & SYSTEM_TIMER_CHANNEL_2_OUTPUT;
    transitions = 0;

    for (new i = 0; i < SPEAKER_PROBE_SAMPLES; i++) {
        microsleep(SPEAKER_PROBE_SAMPLE_US);
        new current = io_in_byte(SYSTEM_CONTROL_PORT_B) &
            SYSTEM_TIMER_CHANNEL_2_OUTPUT;
        if (current != previous)
            transitions++;
        previous = current;
    }

    io_out_byte(SYSTEM_CONTROL_PORT_B, original);
    return transitions > 0;
}

bool:is_speaker_interface_present() {
    // This verifies the legacy control port and system timer feedback path. PC
    // hardware provides no reliable way to sense whether a physical speaker
    // cone is actually connected.
    new transitions;
    return check_speaker_control_port() &&
        check_system_timer_channel_2_activity(transitions);
}

NTSTATUS:audible_speaker_probe() {
    if (is_speaker_enabled())
        return STATUS_DEVICE_BUSY;

    speaker_start(SPEAKER_PROBE_HZ);
    new NTSTATUS:status = microsleep(SPEAKER_AUDIBLE_PROBE_US);
    speaker_stop();
    return status;
}

NTSTATUS:wake_system_speaker() {
    if (is_speaker_enabled())
        return STATUS_DEVICE_BUSY;

    new original = io_in_byte(SYSTEM_CONTROL_PORT_B);

    // Close the speaker data path and system timer gate before resetting channel 2.
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        original & ~SPEAKER_GATE_AND_DATA_ENABLE);
    microsleep(SPEAKER_WAKE_RESET_US);

    // Put system timer channel 2 in mode 0 with a full 16-bit terminal count.
    io_out_byte(SYSTEM_TIMER_COMMAND, SYSTEM_TIMER_CHANNEL_2_MODE_0);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, 0x00);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, 0x00);
    microsleep(SPEAKER_WAKE_RESET_US);

    // Tickle only the timer gate; speaker data remains disabled and silent.
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        (original | SPEAKER_GATE_ENABLE) & ~SPEAKER_DATA_ENABLE);
    microsleep(SPEAKER_WAKE_TOGGLE_US);
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        original & ~SPEAKER_GATE_AND_DATA_ENABLE);
    microsleep(SPEAKER_WAKE_TOGGLE_US);

    io_out_byte(SYSTEM_CONTROL_PORT_B, original);
    return STATUS_SUCCESS;
}

speaker_stop() {
    new control = io_in_byte(SYSTEM_CONTROL_PORT_B);
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        control & ~SPEAKER_GATE_AND_DATA_ENABLE);
}

speaker_start(frequency) {
    // Round to the nearest integer divisor. The accepted frequency range
    // guarantees that the result fits in the system timer's 16-bit counter.
    new divisor = (SYSTEM_TIMER_INPUT_HZ + (frequency / 2)) / frequency;

    io_out_byte(SYSTEM_TIMER_COMMAND, SYSTEM_TIMER_CHANNEL_2_SQUARE_WAVE);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, divisor & 0xFF);
    io_out_byte(SYSTEM_TIMER_CHANNEL_2_DATA, (divisor >> 8) & 0xFF);

    new control = io_in_byte(SYSTEM_CONTROL_PORT_B);
    io_out_byte(
        SYSTEM_CONTROL_PORT_B,
        control | SPEAKER_GATE_AND_DATA_ENABLE);
}

/// Start a continuous tone through the legacy system speaker.
///
/// @param in [0] = frequency in hertz (20 through 20000)
/// @param in_size Must be 1
/// @return An NTSTATUS
/// @note Call ioctl_stop when the tone is no longer needed.
DEFINE_IOCTL_SIZED(ioctl_start, 1, 0) {
    new frequency = in[0];
    if (!is_valid_frequency(frequency))
        return STATUS_INVALID_PARAMETER;

    speaker_start(frequency);
    return STATUS_SUCCESS;
}

/// Stop a tone started by this module.
///
/// @param in Unused
/// @param in_size Must be 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_stop, 0, 0) {
    speaker_stop();
    return STATUS_SUCCESS;
}

/// Read the raw system-control port B byte (diagnostic step 1).
///
/// @param in Unused
/// @param out [0] = current value of port 0x61
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_read_control, 0, 1) {
    out[0] = io_in_byte(SYSTEM_CONTROL_PORT_B);
    return STATUS_SUCCESS;
}

/// Test whether the speaker control bits can be toggled and read back
/// (diagnostic step 2).
///
/// @param in Unused
/// @param out [0] = 1 if the round-trip test passed; otherwise 0
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_test_control_port, 0, 1) {
    if (is_speaker_enabled()) {
        out[0] = 0;
        return STATUS_DEVICE_BUSY;
    }

    out[0] = check_speaker_control_port();
    return STATUS_SUCCESS;
}

/// Test system timer channel 2 feedback while keeping speaker data disabled
/// (diagnostic step 3).
///
/// @param in Unused
/// @param out [0] = number of observed system timer output transitions; zero means fail
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_test_system_timer_channel_2, 0, 1) {
    if (is_speaker_enabled()) {
        out[0] = 0;
        return STATUS_DEVICE_BUSY;
    }

    new transitions;
    check_system_timer_channel_2_activity(transitions);
    out[0] = transitions;
    return STATUS_SUCCESS;
}

/// Emit a very short 9 kHz tone for user-observed verification
/// (diagnostic step 4).
///
/// @param in Unused
/// @param out Unused
/// @return An NTSTATUS
/// @note Software cannot determine whether this probe was physically audible.
DEFINE_IOCTL_SIZED(ioctl_test_audible_probe, 0, 0) {
    return audible_speaker_probe();
}

/// Reset and toggle the legacy speaker interface without emitting a tone.
///
/// @param in Unused
/// @param out Unused
/// @return An NTSTATUS
/// @note Returns STATUS_DEVICE_BUSY rather than modifying an active tone.
DEFINE_IOCTL_SIZED(ioctl_wake, 0, 0) {
    return wake_system_speaker();
}

/// Check whether the legacy system speaker interface is present and responsive.
///
/// @param in Unused
/// @param out [0] = 1 if port 0x61 and system timer channel 2 pass the probe; otherwise 0
/// @return An NTSTATUS
/// @note This cannot prove that a physical speaker cone is connected.
DEFINE_IOCTL_SIZED(ioctl_check_presence, 0, 1) {
    // Do not overwrite system timer channel 2 while another caller has the speaker on.
    // ioctl_is_beep_enabled lets the caller distinguish this case from absence.
    if (is_speaker_enabled()) {
        out[0] = 0;
        return STATUS_DEVICE_BUSY;
    }

    out[0] = is_speaker_interface_present();
    return STATUS_SUCCESS;
}

/// Check whether both speaker-enable bits are latched on.
///
/// @param in Unused
/// @param out [0] = 1 if the speaker gate and data bits are both enabled;
///                  otherwise 0
/// @return An NTSTATUS
/// @note The caller decides whether an enabled tone is intentional or stuck.
DEFINE_IOCTL_SIZED(ioctl_is_beep_enabled, 0, 1) {
    out[0] = is_speaker_enabled();
    return STATUS_SUCCESS;
}

/// Play a tone synchronously through the legacy system speaker.
///
/// @param in [0] = frequency in hertz (20 through 20000)
///            [1] = duration in milliseconds (1 through 60000)
/// @param in_size Must be 2
/// @return An NTSTATUS
/// @note The call does not return until the duration has elapsed.
DEFINE_IOCTL_SIZED(ioctl_beep, 2, 0) {
    new frequency = in[0];
    new duration_ms = in[1];

    if (!is_valid_frequency(frequency) ||
        duration_ms < 1 || duration_ms > SPEAKER_MAX_DURATION_MS)
        return STATUS_INVALID_PARAMETER;

    speaker_start(frequency);
    new NTSTATUS:status = microsleep(duration_ms * 1000);
    speaker_stop();
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    // Never leave the speaker enabled if a client unloads the module without
    // first calling ioctl_stop.
    speaker_stop();
    return STATUS_SUCCESS;
}
