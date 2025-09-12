//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  Steve-Tech <me@stevetech.au>
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

// PawnIO PIIX4 Driver
// Many parts of this was ported from the Linux kernel codebase.
// See https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/i2c/busses/i2c-piix4.c

#define PCI_VENDOR_ID_AMD					0x1022
#define PCI_DEVICE_ID_AMD_KERNCZ_SMBUS		0x790b

/*
 * Data for SMBus Messages
 */
#define I2C_SMBUS_BLOCK_MAX	32	/* As specified in SMBus standard */

#define SMBUS_LEN_SENTINEL (I2C_SMBUS_BLOCK_MAX + 1)

/* i2c_smbus_xfer read or write markers */
#define I2C_SMBUS_READ	1
#define I2C_SMBUS_WRITE	0

/* SMBus transaction types (size parameter in the above functions)
   Note: these no longer correspond to the (arbitrary) PIIX4 internal codes! */
#define I2C_SMBUS_QUICK			    0
#define I2C_SMBUS_BYTE			    1
#define I2C_SMBUS_BYTE_DATA		    2
#define I2C_SMBUS_WORD_DATA		    3
#define I2C_SMBUS_PROC_CALL		    4
#define I2C_SMBUS_BLOCK_DATA	    5
#define I2C_SMBUS_I2C_BLOCK_BROKEN  6
#define I2C_SMBUS_BLOCK_PROC_CALL   7		/* SMBus 2.0 */
#define I2C_SMBUS_I2C_BLOCK_DATA    8

/* Other settings */
#define  ENABLE_INT9	0

/* PIIX4 constants */
#define PIIX4_QUICK			0x00
#define PIIX4_BYTE			0x04
#define PIIX4_BYTE_DATA		0x08
#define PIIX4_WORD_DATA		0x0C
#define PIIX4_BLOCK_DATA	0x14

#define PIIX4_ASF_PROC_CALL	0x10
#define PIIX4_ASF_BLOCK_PROC_CALL	0x18

#define SB800_PIIX4_PORT_IDX_KERNCZ		0x02
#define SB800_PIIX4_PORT_IDX_MASK_KERNCZ	0x18
#define SB800_PIIX4_PORT_IDX_SHIFT_KERNCZ	3

#define SB800_PIIX4_FCH_PM_ADDR			0xFED80300
#define SB800_PIIX4_FCH_PM_SIZE			8

/* PIIX4 SMBus address offsets */
#define SMBHSTSTS	(0x00 + piix4_smba)
#define SMBHSLVSTS	(0x01 + piix4_smba)
#define SMBHSTCNT	(0x02 + piix4_smba)
#define SMBHSTCMD	(0x03 + piix4_smba)
#define SMBHSTADD	(0x04 + piix4_smba)
#define SMBHSTDAT0	(0x05 + piix4_smba)
#define SMBHSTDAT1	(0x06 + piix4_smba)
#define SMBBLKDAT	(0x07 + piix4_smba)
#define SMBSLVCNT	(0x08 + piix4_smba)
#define SMBSHDWCMD	(0x09 + piix4_smba)
#define SMBSLVEVT	(0x0A + piix4_smba)
#define SMBSLVDAT	(0x0C + piix4_smba)
#define SMBTIMING	(0x0E + piix4_smba)

/* PCI Address Constants */
#define PCICMD		0x004

#define PCICMD_IOBIT	0x01

// A 32 byte block read at 10MHz is 32.5ms, 64ms should be plenty
#define MAX_TIMEOUT 64

#define PIIX4_PCI_BUS      0x00
#define PIIX4_PCI_DEVICE   0x14
#define PIIX4_PCI_FUNCTION 0x00

new addresses[] = [0x0B00, 0x0B20];
new piix4_smba = 0x0B00;

NTSTATUS:piix4_init()
{
    new NTSTATUS:status;

    // Check that a PCI device at 00:14.0 exists and is an AMD SMBus controller
    new dev_vid;
    status = pci_config_read_word(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, 0x00, dev_vid);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;

    if (dev_vid != PCI_VENDOR_ID_AMD)
        return STATUS_NOT_SUPPORTED;

    new dev_did;
    status = pci_config_read_word(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, 0x02, dev_did);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;

    if (dev_did != PCI_DEVICE_ID_AMD_KERNCZ_SMBUS)
        return STATUS_NOT_SUPPORTED;

    // Check that the device supports MMIO
    new dev_rev;
    status = pci_config_read_byte(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, 0x08, dev_rev);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;

    // This could potentially be lowered to 0x49, since MMIO looks to be supported,
    // but it needs testing. Also Linux doesn't use MMIO below revision 0x51.
    if (dev_rev < 0x51)
        return STATUS_NOT_SUPPORTED;

    // Map MMIO space
    new VA:addr = io_space_map(SB800_PIIX4_FCH_PM_ADDR, SB800_PIIX4_FCH_PM_SIZE);
    if (addr == NULL) {
        debug_print(''Failed to map MMIO space\n'');
        return STATUS_NOT_SUPPORTED;
    }

    // Check SMBus is enabled
    new smba_en_lo;
    status = virtual_read_byte(addr, smba_en_lo);
    if (!NT_SUCCESS(status)) {
        status = STATUS_NOT_SUPPORTED;
        goto unmap;
    }

    if (!(smba_en_lo & 0x10)) {
        debug_print(''SMBus Host Controller not enabled!\n'');
        status = STATUS_NOT_SUPPORTED;
        goto unmap;
    }

    // Check the SMBus IO Base is 0xB00
    new smba_en_hi;
    status = virtual_read_byte(addr + 1, smba_en_hi);
    if (!NT_SUCCESS(status)) {
        status = STATUS_NOT_SUPPORTED;
        goto unmap;
    }

    if ((smba_en_hi << 8) != addresses[0]) {
        debug_print(''SMBus Host Controller address is not default! (0x%x)\n'', smba_en_hi << 8);
        status = STATUS_NOT_IMPLEMENTED;
        goto unmap;
    }

unmap:
    // Unmap MMIO space
    io_space_unmap(addr, SB800_PIIX4_FCH_PM_SIZE);
    if (!NT_SUCCESS(status))
        return status;

    // I'm fairly certain at least one bit in the status register should be 0
    // Especially since there's 3 reserved bits in the status register
    // So if it is 0xFF we can assume the device is not present
    if (io_in_byte(SMBHSTSTS) == 0xff)
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
NTSTATUS:piix4_busy_check()
{
    new temp;

    /* Make sure the SMBus host is ready to start transmitting */
    if ((temp = io_in_byte(SMBHSTSTS)) != 0x00) {
        debug_print(''SMBus busy (%x). Resetting...\n'', temp);
        io_out_byte(SMBHSTSTS, temp);
        if ((temp = io_in_byte(SMBHSTSTS)) != 0x00) {
            debug_print(''Failed! (%x)\n'', temp);
            return STATUS_DEVICE_BUSY;
        } else {
            debug_print(''Successful!\n'');
        }
    }
    return STATUS_SUCCESS;
}

NTSTATUS:piix4_transaction(size)
{
    new NTSTATUS:status = STATUS_SUCCESS;
    new temp;
    new timing = io_in_byte(SMBTIMING);

    /* start the transaction by setting bit 6 */
    io_out_byte(SMBHSTCNT, io_in_byte(SMBHSTCNT) | 0x040);

    // Don't wait more than MAX_TIMEOUT ms for the transaction to complete
    new deadline = get_tick_count() + MAX_TIMEOUT;

    // 10 start bits (start + slave address + rd/wr + ack)
    // 9 bits per byte (byte + ack)
    // From datasheet: 'Frequency = 66Mhz/(SmBusTiming * 4)' (we flip the division for the period)
    microsleep2(((10 + (9 * size)) * timing * 4) / 66);
    do {
        // Only check for result once per clock cycle
        // Also allows for 1 stop bit
        microsleep2((timing * 4) / 66);
        temp = io_in_byte(SMBHSTSTS);
    } while ((get_tick_count() < deadline) && (temp & 0x01));

    if (temp == 0x02) {
        // Reset the status flags
        io_out_byte(SMBHSTSTS, temp);
        goto check_reset;
    }

    /* If the SMBus is still busy, we give up */
    if (temp & 0x01) {
        debug_print(''SMBus Timeout!\n'');
        status = STATUS_IO_TIMEOUT;
    }

    if (temp & 0x10) {
        status = STATUS_IO_DEVICE_ERROR;
        debug_print(''Error: Failed bus transaction\n'');
    }

    if (temp & 0x08) {
        status = STATUS_IO_DEVICE_ERROR;
        debug_print(''Bus collision! SMBus may be locked until next hard reset. (sorry!)\n'');
        /* Clock stops and target is stuck in mid-transmission */
    }

    if (temp & 0x04 || temp & 0x02 == 0) {
        status = STATUS_NO_SUCH_DEVICE;
        debug_print(''Error: no response!\n'');
    }

    // Reset the status flags
    if (temp != 0x00)
        io_out_byte(SMBHSTSTS, temp);

check_reset:
    if ((temp = io_in_byte(SMBHSTSTS)) != 0x00) {
        debug_print(''Failed reset at end of transaction (%x)\n'', temp);
    }
    // debug_print(''Transaction (post): CNT=%x, CMD=%x, ADD=%x, DAT0=%x, DAT1=%x\n'',
    //             io_in_byte(SMBHSTCNT), io_in_byte(SMBHSTCMD), io_in_byte(SMBHSTADD),
    //             io_in_byte(SMBHSTDAT0), io_in_byte(SMBHSTDAT1));
    return status;
}

NTSTATUS:piix4_access_simple(addr, read_write, command, hstcmd, in, &out)
{
    new NTSTATUS:status, protocol, size = hstcmd;

    status = piix4_busy_check();
    if (!NT_SUCCESS(status))
        return status;

    switch (hstcmd) {
    case I2C_SMBUS_QUICK:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            protocol = PIIX4_QUICK;
        }
    case I2C_SMBUS_BYTE:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTCMD, command);
            protocol = PIIX4_BYTE;
        }
    case I2C_SMBUS_BYTE_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTDAT0, in);
            protocol = PIIX4_BYTE_DATA;
            size += read_write;
        }
    case I2C_SMBUS_WORD_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE) {
                io_out_byte(SMBHSTDAT0, in);
                io_out_byte(SMBHSTDAT1, in >> 8);
            }
            protocol = PIIX4_WORD_DATA;
            size += read_write;
        }
    default:
        {
            debug_print(''Unsupported transaction %d\n'', hstcmd);
            return STATUS_NOT_SUPPORTED;
        }
    }

    io_out_byte(SMBHSTCNT, (protocol & 0x1C) + (ENABLE_INT9 & 1));

    status = piix4_transaction(size);
    if (!NT_SUCCESS(status))
        return status;

    if ((read_write == I2C_SMBUS_WRITE) || (protocol == PIIX4_QUICK))
        return STATUS_SUCCESS;

    switch (protocol) {
    case PIIX4_BYTE, PIIX4_BYTE_DATA:
        out = io_in_byte(SMBHSTDAT0);
    case PIIX4_WORD_DATA:
        out = io_in_byte(SMBHSTDAT0) + (io_in_byte(SMBHSTDAT1) << 8);
    }
    return STATUS_SUCCESS;
}

NTSTATUS:piix4_access_block(addr, read_write, command, hstcmd, in[33], out[33])
{
    if (read_write == I2C_SMBUS_READ)
        out[0] = SMBUS_LEN_SENTINEL; // Mark block length as invalid
    else if (in[0] < 1 || in[0] > I2C_SMBUS_BLOCK_MAX)
        return STATUS_INVALID_PARAMETER;

    new NTSTATUS:status, protocol;
    // We don't know the return size, so lets just wait the minimum amount of time
    // write lacks repeated address
    new size = 2 + read_write;

    status = piix4_busy_check();
    if (!NT_SUCCESS(status))
        return status;

    switch (hstcmd) {
    case I2C_SMBUS_BLOCK_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE) {
                new len = in[0];
                size += len;
                io_out_byte(SMBHSTDAT0, len);
                io_in_byte(SMBHSTCNT);    /* Reset SMBBLKDAT */
                for (new i = 1; i <= len; i++)
                    io_out_byte(SMBBLKDAT, in[i]);
            }
            protocol = PIIX4_BLOCK_DATA;
        }
    default:
        {
            debug_print(''Unsupported transaction %d\n'', hstcmd);
            return STATUS_NOT_SUPPORTED;
        }
    }

    io_out_byte(SMBHSTCNT, (protocol & 0x1C) + (ENABLE_INT9 & 1));

    status = piix4_transaction(size);
    if (!NT_SUCCESS(status))
        return status;

    if (read_write == I2C_SMBUS_WRITE)
        return STATUS_SUCCESS;

    switch (protocol) {
    case PIIX4_BLOCK_DATA:
        {
            new len = io_in_byte(SMBHSTDAT0);
            if (len <= 0 || len > I2C_SMBUS_BLOCK_MAX)
                return STATUS_DEVICE_PROTOCOL_ERROR;
            out[0] = len;
            io_in_byte(SMBHSTCNT);    /* Reset SMBBLKDAT */
            for (new i = 0; i < len; i++)
                out[1 + i] = io_in_byte(SMBBLKDAT);
        }
    }
    return STATUS_SUCCESS;
}

NTSTATUS:piix4_port_sel_primary(port, &old_port)
{
    new NTSTATUS:status = STATUS_SUCCESS;

    static port_to_reg[] = [0b00, 0b00, 0b01, 0b10, 0b11];
    static reg_to_port[] = [0, 2, 3, 4];

    // Not exactly needed, but probably a good idea to check
    status = piix4_busy_check();
    if (!NT_SUCCESS(status))
        return status;

    // Map MMIO space
    new VA:addr = io_space_map(SB800_PIIX4_FCH_PM_ADDR, SB800_PIIX4_FCH_PM_SIZE);
    if (addr == NULL) {
        debug_print(''Failed to map MMIO space\n'');
        return STATUS_IO_DEVICE_ERROR;
    }

    // Read the current port
    new smba_en_lo;
    status = virtual_read_byte(addr + SB800_PIIX4_PORT_IDX_KERNCZ, smba_en_lo);
    if (!NT_SUCCESS(status)) {
        debug_print(''Failed to read port %d\n'', port);
        status = STATUS_IO_DEVICE_ERROR;
        goto unmap;
    }

    new reg = (smba_en_lo & SB800_PIIX4_PORT_IDX_MASK_KERNCZ) >> SB800_PIIX4_PORT_IDX_SHIFT_KERNCZ;
    old_port = reg_to_port[reg];
    // Return the current port if the given port is -1
    if (port == -1)
        goto unmap;

    new new_port = port_to_reg[port];

    // Set the new port
    new val = (smba_en_lo & ~SB800_PIIX4_PORT_IDX_MASK_KERNCZ) | (new_port << SB800_PIIX4_PORT_IDX_SHIFT_KERNCZ);
    if (val != smba_en_lo) {
        status = virtual_write_byte(addr + SB800_PIIX4_PORT_IDX_KERNCZ, val);
        if (!NT_SUCCESS(status)) {
            debug_print(''Failed to set port %d\n'', port);
            status = STATUS_IO_DEVICE_ERROR;
            goto unmap;
        }
    }

unmap:
    // Unmap MMIO space
    io_space_unmap(addr, SB800_PIIX4_FCH_PM_SIZE);
    return status;
}

NTSTATUS:piix4_port_sel(port, &old_port) {
    if (piix4_smba == addresses[1]) {
        // Current port is aux

        old_port = 1;
        if (port == 1 || port == -1) {
            return STATUS_SUCCESS; // Nothing to do
        } else {
            new old_port_tmp;
            new NTSTATUS:status = piix4_port_sel_primary(port, old_port_tmp);
            if (NT_SUCCESS(status)) {
                piix4_smba = addresses[0]; // Switch to primary as we plan to return success
            }
            return status;
        }
    } else {
        // Current port is primary

        if (port == 1) {
            new NTSTATUS:status = piix4_port_sel_primary(-1, old_port);
            if (NT_SUCCESS(status)) {
                piix4_smba = addresses[1]; // Switch to auxilary as we plan to return success
            }
            return status;
        } else {
            return piix4_port_sel_primary(port, old_port);
        }
    }
}

/// Select I2C port on the chipset.
///
/// @param in [0] = Port or -1 for no change
/// @param in_size Must be 1
/// @param out [0] = Previous port
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
/// @warning Changing to port 2, 3, or 4 may break other software expecting to be using port 0
/// @note Port 1 uses a different base address, and should not break other software expecting another port
/// @note Ports 3 and 4 are marked as reserved in the datasheet, use at your own risk
DEFINE_IOCTL_SIZED(ioctl_piix4_port_sel, 1, 1) {
    new new_port = in[0];
    new old_port = -1;

    new NTSTATUS:status = piix4_port_sel(new_port, old_port);

    out[0] = old_port;

    return status;
}

/// Identify the SMBus controller.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Type of the SMBus controller, [1] = I/O Base address, [2] = PCI Identifiers
/// @param out_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 3) {
    new NTSTATUS:status;

    out[0] = CHAR5_CONST('P', 'I', 'I', 'X', '4');

    out[1] = piix4_smba;

    // Read the PCI vendor/device ID
    new pci_ids;
    status = pci_config_read_dword(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, 0x00, pci_ids);
    if (!NT_SUCCESS(status))
        return status;

    // Read the PCI subsystem vendor/device ID
    new pci_subsys_ids;
    status = pci_config_read_dword(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, 0x2C, pci_subsys_ids);
    if (!NT_SUCCESS(status))
        return status;

    out[2] = pci_ids | (pci_subsys_ids << 32);

    return STATUS_SUCCESS;
}

/// Set the SMBus clock frequency.
///
/// @param in [0] = Frequency in Hz or -1 for no change
/// @param in_size Must be 1
/// @param out [0] = Previous frequency in Hz
/// @param out_size Must be 1
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
/// @warning This may cause issues with SMBus devices that expect a specific frequency
/// @note The default frequency for PIIX4 is 93750Hz (93.75kHz) but may be overridden by the BIOS
/// @note The minimum frequency configureable for PIIX4 is ~64705Hz (~64.705kHz)
/// @note The SMBus 1.0 specification allows for frequencies between 10kHz and 100kHz
/// @note The SMBus 3.0 specification allows for frequencies between 10kHz and 1MHz
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_clock_freq, 1, 1) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new new_freq = in[0];
    new new_timing = -1;

    if (new_freq != -1) {
        // SMBus 3.0 spec allows for frequencies between 10KHz and 1MHz.
        // Check if the frequency is valid
        if (new_freq < 10000 || new_freq > 1000000)
            return STATUS_INVALID_PARAMETER;

        // Calculate the new timing value
        new_timing = (66 * 1000000) / new_freq / 4;
        if (new_timing < 1 || new_timing > 255)
            return STATUS_INVALID_PARAMETER;
    }

    // From datasheet: 'Frequency = 66Mhz/(SmBusTiming * 4)'
    out[0] = (66 * 1000000) / (io_in_byte(SMBTIMING) * 4);

    if (new_timing != -1) {
        // Set the new timing value
        io_out_byte(SMBTIMING, new_timing);
    }

    return STATUS_SUCCESS;
}

/// SMBus transfer.
///
/// Performs a transfer of data over the SMBus using the specified command.
/// I2C_SMBUS_QUICK (protocol 0) only requires the address and read/write parameters, command must be left as 0.
/// I2C_SMBUS_BYTE (1), I2C_SMBUS_BYTE_DATA (2), and I2C_SMBUS_WORD_DATA (3) require the address, read/write, command, and data (write only) parameters.
/// I2C_SMBUS_BLOCK_DATA (5) requires the address, read/write, command, data parameters.
///
/// @param in [0] = Address, [1] = Read(1)/Write(0), [2] = Command, [3] = Protocol, [4..9] Data
/// @param in_size Must be between 4 and 9
/// @param out [0..5] = Length then Data (byte packed)
/// @param out_size Must be between 0 and 5
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
DEFINE_IOCTL(ioctl_smbus_xfer) {
    if (in_size < 4)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new read_write = in[1];
    new command = in[2];
    new hstcmd = in[3];

    new pci_cmd_original;
    new pci_cmd_modified;

    pci_config_read_word(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, PCICMD, pci_cmd_original);

    //PCI CMD IO not enabled
    if (0 == (pci_cmd_original & PCICMD_IOBIT))
    {
        //Enable PCI CMD IO
        pci_cmd_modified = pci_cmd_original | PCICMD_IOBIT;
        pci_config_write_word(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, PCICMD, pci_cmd_modified);
    }

    new NTSTATUS:status;

    switch (hstcmd) {
    case I2C_SMBUS_QUICK:
        {
            new unused;
            status = piix4_access_simple(address, read_write, command, hstcmd, 0, unused);
        }
    case I2C_SMBUS_BYTE, I2C_SMBUS_BYTE_DATA, I2C_SMBUS_WORD_DATA:
        {
            new unused;
            if (read_write == I2C_SMBUS_WRITE) {
                if (in_size < 5) {
                    status = STATUS_BUFFER_TOO_SMALL;
                    goto getout;
                }

                new data = in[4];

                status = piix4_access_simple(address, read_write, command, hstcmd, data, unused);
            } else {
                // read_write == I2C_SMBUS_READ
                if (out_size < 1) {
                    status = STATUS_BUFFER_TOO_SMALL;
                    goto getout;
                }

                status = piix4_access_simple(address, read_write, command, hstcmd, unused, out[0]);
            }
        }
    case I2C_SMBUS_BLOCK_DATA:
        {
            if (read_write == I2C_SMBUS_WRITE) {
                // 4 parameters, 5 cells of data
                if (in_size < (4 + 5)) {
                    status = STATUS_BUFFER_TOO_SMALL;
                    goto getout;
                }

                new in_data[I2C_SMBUS_BLOCK_MAX + 1];
                unpack_bytes_le(in, in_data, I2C_SMBUS_BLOCK_MAX + 1, 4 * 8, 0);

                new unused[I2C_SMBUS_BLOCK_MAX + 1];

                status = piix4_access_block(address, read_write, command, hstcmd, in_data, unused);
            } else {
                // read_write == I2C_SMBUS_READ
                if (out_size < 5) {
                    status = STATUS_BUFFER_TOO_SMALL;
                    goto getout;
                }

                new unused[I2C_SMBUS_BLOCK_MAX + 1];
                new out_data[I2C_SMBUS_BLOCK_MAX + 1];

                status = piix4_access_block(address, read_write, command, hstcmd, unused, out_data);

                if (!NT_SUCCESS(status))
                    return status;

                pack_bytes_le(out_data, out, I2C_SMBUS_BLOCK_MAX + 1);

                return status;
            }
        }
        default:
        {
            debug_print(''Unsupported transaction %d\n'', hstcmd);
            status = STATUS_NOT_SUPPORTED;
        }
    }

getout:
    //Restore original PCI CMD, if it was modified
    if (pci_cmd_original != pci_cmd_modified)
        pci_config_write_word(PIIX4_PCI_BUS, PIIX4_PCI_DEVICE, PIIX4_PCI_FUNCTION, PCICMD, pci_cmd_original);

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return piix4_init();
}
