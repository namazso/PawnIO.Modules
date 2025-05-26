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

// A 32 byte block read at 10MHz is 32.5ms, 64ms should be plenty
#define MAX_TIMEOUT 64

new addresses[] = [0x0B00, 0x0B20];
new piix4_smba = 0x0B00;

NTSTATUS:piix4_init()
{
    new NTSTATUS:status;

    // Check that a PCI device at 00:14.0 exists and is an AMD SMBus controller
    new dev_vid;
    status = pci_config_read_word(0x0, 0x14, 0x0, 0x0, dev_vid);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;

    if (dev_vid != PCI_VENDOR_ID_AMD)
        return STATUS_NOT_SUPPORTED;

    new dev_did;
    status = pci_config_read_word(0x0, 0x14, 0x0, 0x2, dev_did);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;

    if (dev_did != PCI_DEVICE_ID_AMD_KERNCZ_SMBUS)
        return STATUS_NOT_SUPPORTED;

    // Check that the device supports MMIO
    new dev_rev;
    status = pci_config_read_byte(0x0, 0x14, 0x0, 0x8, dev_rev);
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
                if (len <= 0 || len > I2C_SMBUS_BLOCK_MAX)
                    return STATUS_INVALID_PARAMETER;
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
/// @param in [0] = Port or -1 for none
/// @param in_size Must be 1
/// @param out [0] = Old port
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
/// @warning Changing to port 2, 3, or 4 may break other software expecting to be using port 0
/// @note Port 1 uses a different base address, and should not break other software expecting another port
/// @note Ports 3 and 4 are marked as reserved in the datasheet, use at your own risk
forward NTSTATUS:ioctl_piix4_port_sel(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_port_sel(in[], in_size, out[], out_size) {
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new new_port = in[0];
    new old_port = -1;

    new NTSTATUS:status = piix4_port_sel(new_port, old_port);
    
    out[0] = old_port;

    return status;
}

/// SMBus quick write.
///
/// The SMBus Read/Write Quick protocol (SMBQuick) is typically used to control
/// simple devices using a device-specific binary command (for example, ON and OFF).
/// Command values are not used by this protocol and thus only a single element
/// (at offset 0) can be specified in the field definition.
///
/// @param in [0] = Address, [1] = Command
/// @param in_size Must be 2
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_write_quick(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_write_quick(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    new unused;

    return piix4_access_simple(address, command, 0, I2C_SMBUS_QUICK, 0, unused);
}

/// SMBus byte receive.
///
/// The SMBus Send/Receive Byte protocol (SMBSendReceive) transfers a single
/// byte of data. Like Read/Write Quick, command values are not used by this
/// protocol and thus only a single element (at offset 0) can be specified in
/// the field definition.
///
/// @param in [0] = Address
/// @param in_size Must be 1
/// @param out [0] = Data
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_read_byte(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_read_byte(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];

    return piix4_access_simple(address, I2C_SMBUS_READ, 0, I2C_SMBUS_BYTE, 0, out[0]);
}

/// SMBus byte send.
///
/// The SMBus Send/Receive Byte protocol (SMBSendReceive) transfers a single
/// byte of data. Like Read/Write Quick, command values are not used by this
/// protocol and thus only a single element (at offset 0) can be specified in
/// the field definition.
///
/// @param in [0] = Address, [1] = Data
/// @param in_size Must be 2
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_write_byte(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_write_byte(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new data = in[1];

    new unused;

    return piix4_access_simple(address, I2C_SMBUS_WRITE, data, I2C_SMBUS_BYTE, 0, unused);
}

/// SMBus byte read.
///
/// The SMBus Read/Write Byte protocol (SMBByte) also transfers a single byte of
/// data. But unlike Send/Receive Byte, this protocol uses a command value to
/// reference up to 256 byte-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command
/// @param in_size Must be 2
/// @param out [0] = Data
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_read_byte_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_read_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    return piix4_access_simple(address, I2C_SMBUS_READ, command, I2C_SMBUS_BYTE_DATA, 0, out[0]);
}

/// SMBus byte write.
///
/// The SMBus Read/Write Byte protocol (SMBByte) also transfers a single byte of
/// data. But unlike Send/Receive Byte, this protocol uses a command value to
/// reference up to 256 byte-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command, [2] = Data
/// @param in_size Must be 3
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_write_byte_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_write_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new data = in[2];

    new unused;

    return piix4_access_simple(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_BYTE_DATA, data, unused);
}

/// SMBus word read.
///
/// The SMBus Read/Write Word protocol (SMBWord) transfers 2 bytes of data.
/// This protocol also uses a command value to reference up to 256 word-sized
/// virtual device registers.
///
/// @param in [0] = Address, [1] = Command
/// @param in_size Must be 2
/// @param out [0] = Data
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_read_word_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_read_word_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    return piix4_access_simple(address, I2C_SMBUS_READ, command, I2C_SMBUS_WORD_DATA, 0, out[0]);
}

/// SMBus word write.
///
/// The SMBus Read/Write Word protocol (SMBWord) transfers 2 bytes of data.
/// This protocol also uses a command value to reference up to 256 word-sized
/// virtual device registers.
///
/// @param in [0] = Address, [1] = Command, [2] = Data
/// @param in_size Must be 3
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_write_word_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_write_word_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new data = in[2];

    new unused;

    return piix4_access_simple(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_WORD_DATA, data, unused);
}

/// SMBus block read.
///
/// The SMBus Read/Write Block protocol (SMBBlock) transfers variable-sized
/// (0-32 bytes) data. This protocol uses a command value to reference up to 256
/// block-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command
/// @param in_size Must be 2
/// @param out [0] = Length in bytes, [1..5] = Data (byte packed)
/// @param out_size Must be 5
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_read_block_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_read_block_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 5)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    new unused[I2C_SMBUS_BLOCK_MAX + 1];
    new out_data[I2C_SMBUS_BLOCK_MAX + 1];

    new NTSTATUS:status = piix4_access_block(address, I2C_SMBUS_READ, command, I2C_SMBUS_BLOCK_DATA, unused, out_data);

    if (!NT_SUCCESS(status))
        return status;

    out[0] = out_data[0];
    pack_bytes_le(out_data, out, I2C_SMBUS_BLOCK_MAX, 1, 8);

    return status;
}

/// SMBus block write.
///
/// The SMBus Read/Write Block protocol (SMBBlock) transfers variable-sized
/// (0-32 bytes) data. This protocol uses a command value to reference up to 256
/// block-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command, [2] = Length in bytes, [3..7] = Data (byte packed)
/// @param in_size Must be 7
/// @param out Unused
/// @param out_size Unused
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_piix4_write_block_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_piix4_write_block_data(in[], in_size, out[], out_size) {
    if (in_size < 7)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new length = in[2];

    if (length > I2C_SMBUS_BLOCK_MAX || length < 1)
        return STATUS_INVALID_PARAMETER;

    new in_data[I2C_SMBUS_BLOCK_MAX + 1];
    in_data[0] = length;
    unpack_bytes_le(in, in_data, I2C_SMBUS_BLOCK_MAX, 3 * 8, 1);
    new unused[I2C_SMBUS_BLOCK_MAX + 1];

    return piix4_access_block(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_BLOCK_DATA, in_data, unused);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return piix4_init();
}
