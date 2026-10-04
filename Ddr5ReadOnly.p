// SPDX-License-Identifier: LGPL-2.1-or-later
// Copyright (C) 2026 Hardware Tray contributors.
// Intel i801 register layout/transaction sequence adapted from PawnIO.Modules
// SmbusI801.p, Copyright (C) 2025 Steve-Tech, LGPL-2.1-or-later, commit
// 52a7e536dff3e53c96917a28caac5e0fa6510696. See COPYING in that project.
// Candidate module: NOT signed, NOT approved for installation on real hardware.
#include <pawnio.inc>

new VA:controller;
new pci_function = -1;
bool:allowed_read(address, direction, register, protocol);
NTSTATUS:read_register(address, register, protocol, &result);

// This is the only exported hardware operation. There is no generic SMBus,
// EEPROM, MSR, PCI-write, physical-memory-write or module-loading interface.
// Check full-width cells before any truncation or hardware/native operation.
/// Read one allowlisted DDR5 SPD5118 live register through an Intel i801.
/// @param in [address, direction (=1), register, protocol (=2 byte / =3 word)]
/// @param in_size Exactly four 64-bit cells; write payloads are rejected.
/// @param out One zero-extended byte/word; valid only on STATUS_SUCCESS.
/// @param out_size Exactly one 64-bit cell.
/// @return STATUS_ACCESS_DENIED for non-read/allowlist violations.
/// @warning Hold the global Access_SMBUS.HTP.Method mutex for the entire scan.
DEFINE_IOCTL(ioctl_ddr5_read) {
    if (in_size != 4 || out_size != 1)
        return STATUS_INVALID_PARAMETER;
    if (!allowed_read(in[0], in[1], in[2], in[3]))
        return STATUS_ACCESS_DENIED;
    out[0] = 0;
    return read_register(in[0], in[2], in[3], out[0]);
}

bool:allowed_read(address, direction, register, protocol) {
    if (address < 0x50 || address > 0x57 || direction != 1)
        return false;
    switch (register) {
        case 0x00, 0x03, 0x31: return protocol == 3;
        case 0x05, 0x0b, 0x1a: return protocol == 2;
    }
    return false;
}

NTSTATUS:main() {
    // Intel client platforms only, bus 0 device 31 function 4 or 3.
    // No PCI writes: disabled controller, I2C mode or memory decode => unavailable.
    new value;
    for (new function = 4; function >= 3; --function) {
        if (!NT_SUCCESS(pci_config_read_word(0, 31, function, 0, value)) || value != 0x8086)
            continue;
        if (!NT_SUCCESS(pci_config_read_word(0, 31, function, 0x0a, value)) || value != 0x0c05)
            continue;
        pci_function = function;
        break;
    }
    if (pci_function == -1)
        return STATUS_NOT_SUPPORTED;
    if (!NT_SUCCESS(pci_config_read_word(0, 31, pci_function, 4, value)) || (value & 2) == 0)
        return STATUS_NOT_SUPPORTED;
    if (!NT_SUCCESS(pci_config_read_byte(0, 31, pci_function, 0x40, value)) || (value & 5) != 1)
        return STATUS_NOT_SUPPORTED;
    if (!NT_SUCCESS(pci_config_read_qword(0, 31, pci_function, 0x10, value)) || (value & 1) != 0)
        return STATUS_NOT_SUPPORTED;
    new bar_type = value & 6;
    if (bar_type == 0)
        value &= 0xffffffff;
    else if (bar_type != 4)
        return STATUS_NOT_SUPPORTED;
    value &= 0xffffffffffffff00;
    if (value == 0)
        return STATUS_NOT_SUPPORTED;
    controller = io_space_map(value, 0x18);
    return controller == NULL ? STATUS_INSUFFICIENT_RESOURCES : STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (controller != NULL) {
        io_space_unmap(controller, 0x18);
        controller = NULL;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:read_register(address, register, protocol, &result) {
    // Defense in depth: even internal callers can request only allowlisted reads.
    if (!allowed_read(address, 1, register, protocol))
        return STATUS_ACCESS_DENIED;
    if (controller == NULL)
        return STATUS_DEVICE_NOT_READY;
    new config;
    if (!NT_SUCCESS(pci_config_read_word(0, 31, pci_function, 4, config)) || (config & 2) == 0)
        return STATUS_NOT_SUPPORTED;
    if (!NT_SUCCESS(pci_config_read_byte(0, 31, pci_function, 0x40, config)) || (config & 5) != 1)
        return STATUS_NOT_SUPPORTED;

    new host_status, old_control, auxiliary;
    new NTSTATUS:status = virtual_read_byte(controller, host_status);
    if (!NT_SUCCESS(status)) return status;
    if (host_status & 0x40) // Already INUSE: the claim belongs to another client.
        return STATUS_DEVICE_BUSY;
    if (host_status & 1) {
        // Our status read acquired INUSE, but a transaction was already running.
        // Release only our claim, preserving the other transaction's status flags.
        virtual_write_byte(controller, 0x40);
        return STATUS_DEVICE_BUSY;
    }

    // Reading INUSE_STS claims the controller on i801. Release only after our claim.
    status = virtual_read_byte(controller + 2, old_control);
    if (!NT_SUCCESS(status)) goto release;
    if (old_control & 0x42) { status = STATUS_DEVICE_BUSY; goto release; }
    status = virtual_read_byte(controller + 13, auxiliary);
    if (!NT_SUCCESS(status)) goto release;
    // Do not change controller PEC/block-buffer settings to make a read possible.
    if (auxiliary & 3) { status = STATUS_NOT_SUPPORTED; goto release; }
    status = virtual_write_byte(controller, host_status & 0x9e);
    if (!NT_SUCCESS(status)) goto release;
    // Always set the bus direction bit to READ. No data-byte write exists here.
    status = virtual_write_byte(controller + 4, (address << 1) | 1);
    if (!NT_SUCCESS(status)) goto release;
    status = virtual_write_byte(controller + 3, register);
    if (!NT_SUCCESS(status)) goto release;
    status = virtual_write_byte(controller + 2, (protocol == 3 ? 0x0c : 0x08) | 0x40);
    if (!NT_SUCCESS(status)) goto restore;

    new deadline = get_tick_count() + 80;
    new attempts = 0;
    do {
        microsleep(100);
        status = virtual_read_byte(controller, host_status);
        if (!NT_SUCCESS(status)) goto stop_own;
        if ((host_status & 1) == 0 && (host_status & 0x1e) != 0)
            break;
    } while (++attempts < 800 && get_tick_count() < deadline);
    if ((host_status & 1) || (host_status & 0x1e) == 0) {
        status = STATUS_IO_TIMEOUT;
        goto stop_own;
    }
    if (host_status & 0x1c) { status = STATUS_IO_DEVICE_ERROR; goto restore; }
    if (protocol == 3)
        status = virtual_read_word(controller + 5, result);
    else
        status = virtual_read_byte(controller + 5, result);
    goto restore;

stop_own:
    // Abort only the transaction started above; never used on initial busy state.
    virtual_write_byte(controller + 2, 2);
    microsleep(1000);
    virtual_write_byte(controller + 2, 0);
restore:
    virtual_write_byte(controller + 2, old_control);
release:
    // Controller handshake/status registers only, not peripheral configuration.
    virtual_write_byte(controller, 0xde);
    return status;
}
