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

#include <pawnio.inc>

// PawnIO PIIX4 Driver
// Many parts of this was copied from the Linux kernel codebase.
// See https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/i2c/busses/i2c-piix4.c


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
#define MAX_TIMEOUT		500
#define  ENABLE_INT9	0

/* PIIX4 constants */
#define PIIX4_QUICK			0x00
#define PIIX4_BYTE			0x04
#define PIIX4_BYTE_DATA		0x08
#define PIIX4_WORD_DATA		0x0C

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

/* PIIX4 constants */
#define PIIX4_BLOCK_DATA	0x14

new allowed_ports[] = [0x0B00, 0x0B20];
new piix4_smba = 0x0B00;

piix4_transaction()
{
    new temp;
    new result = STATUS_SUCCESS;
    new timeout = 0;

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

    /* start the transaction by setting bit 6 */
    io_out_byte(SMBHSTCNT, io_in_byte(SMBHSTCNT) | 0x040);

    /* We will always wait for a fraction of a second! (See PIIX4 docs errata) */
    do
        microsleep(250);
    while ((++timeout < MAX_TIMEOUT) &&
           ((temp = io_in_byte(SMBHSTSTS)) & 0x01));

    /* If the SMBus is still busy, we give up */
    if (timeout == MAX_TIMEOUT) {
        debug_print(''SMBus Timeout!\n'');
        result = STATUS_IO_TIMEOUT;
    }

    if (temp & 0x10) {
        result = STATUS_IO_DEVICE_ERROR;
        debug_print(''Error: Failed bus transaction\n'');
    }

    if (temp & 0x08) {
        result = STATUS_IO_DEVICE_ERROR;
        debug_print(''Bus collision! SMBus may be locked until next hard reset. (sorry!)\n'');
        /* Clock stops and target is stuck in mid-transmission */
    }

    if (temp & 0x04) {
        result = STATUS_NO_SUCH_DEVICE;
        debug_print(''Error: no response!\n'');
    }

    if (io_in_byte(SMBHSTSTS) != 0x00)
        io_out_byte(SMBHSTSTS, io_in_byte(SMBHSTSTS));

    if ((temp = io_in_byte(SMBHSTSTS)) != 0x00) {
        debug_print(''Failed reset at end of transaction (%x)\n'', temp);
    }
    // debug_print(''Transaction (post): CNT=%x, CMD=%x, ADD=%x, DAT0=%x, DAT1=%x\n'',
    //             io_in_byte(SMBHSTCNT), io_in_byte(SMBHSTCMD), io_in_byte(SMBHSTADD),
    //             io_in_byte(SMBHSTDAT0), io_in_byte(SMBHSTDAT1));
    return result;
}

piix4_access(addr, read_write, command, size, in[], out[])
{
    new i;
    new len;
    new status;

    switch (size) {
    case I2C_SMBUS_QUICK:
        {
            io_out_byte(SMBHSTADD, 
                        (addr << 1) | read_write);
            size = PIIX4_QUICK;
        }
    case I2C_SMBUS_BYTE:
        {
            io_out_byte(SMBHSTADD, 
                        (addr << 1) | read_write);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTCMD, command);
            size = PIIX4_BYTE;
        }
    case I2C_SMBUS_BYTE_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTDAT0, in[0]);
            size = PIIX4_BYTE_DATA;
        }
    case I2C_SMBUS_WORD_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE) {
                io_out_byte(SMBHSTDAT0, in[0] & 0xff);
                io_out_byte(SMBHSTDAT1, (in[0] & 0xff00) >> 8);
            }
            size = PIIX4_WORD_DATA;
        }
    case I2C_SMBUS_BLOCK_DATA:
        {
            io_out_byte(SMBHSTADD,
                        (addr << 1) | read_write);
            io_out_byte(SMBHSTCMD, command);
            if (read_write == I2C_SMBUS_WRITE) {
                len = in[0];
                if (len == 0 || len > I2C_SMBUS_BLOCK_MAX)
                    return STATUS_INVALID_PARAMETER;
                io_out_byte(SMBHSTDAT0, len);
                io_in_byte(SMBHSTCNT);    /* Reset SMBBLKDAT */
                for (i = 1; i <= len; i++)
                    io_out_byte(SMBBLKDAT, in[i]);
            }
            size = PIIX4_BLOCK_DATA;
        }
    default:
        {
            debug_print(''Unsupported transaction %d\n'', size);
            return STATUS_NOT_SUPPORTED;
        }
    }

    io_out_byte(SMBHSTCNT, (size & 0x1C) + (ENABLE_INT9 & 1));

    status = piix4_transaction();
    if (status)
        return status;

    if ((read_write == I2C_SMBUS_WRITE) || (size == PIIX4_QUICK))
        return STATUS_SUCCESS;

    switch (size) {
    case PIIX4_BYTE, PIIX4_BYTE_DATA:
        out[0] = io_in_byte(SMBHSTDAT0);
    case PIIX4_WORD_DATA:
        out[0] = io_in_byte(SMBHSTDAT0) + (io_in_byte(SMBHSTDAT1) << 8);
    case PIIX4_BLOCK_DATA:
        {
            out[0] = io_in_byte(SMBHSTDAT0);
            if (out[0] == 0 || out[0] > I2C_SMBUS_BLOCK_MAX)
                return STATUS_DEVICE_PROTOCOL_ERROR;
            io_in_byte(SMBHSTCNT);    /* Reset SMBBLKDAT */
            for (i = 1; i <= out[0]; i++)
                out[i] = io_in_byte(SMBBLKDAT);
        }
    }
    return STATUS_SUCCESS;
}

/* Allows changing the SMBus base address. */
// IN: [0] = address
forward ioctl_piix4_init(in[], in_size, out[], out_size);
public ioctl_piix4_init(in[], in_size, out[], out_size) {
    if (in_size < 1) {
        piix4_smba = 0x0B00;
        return STATUS_SUCCESS;
    }

    for (new i = 0; i < sizeof allowed_ports; i++) {
        if (in[0] == allowed_ports[i]) {
            piix4_smba = in[0];
            return STATUS_SUCCESS;
        }
    }
    return STATUS_ACCESS_DENIED;
}

/*
 * The SMBus Read/Write Quick protocol (SMBQuick) is typically used to control
 * simple devices using a device-specific binary command (for example, ON and OFF).
 * Command values are not used by this protocol and thus only a single element
 * (at offset 0) can be specified in the field definition.
 */
// IN: [0] = address, [1] = command
forward ioctl_piix4_write_quick(in[], in_size, out[], out_size);
public ioctl_piix4_write_quick(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];

    return piix4_access(in[0], in[1], 0, I2C_SMBUS_QUICK, data, out);
}

/*
 * The SMBus Send/Receive Byte protocol (SMBSendReceive) transfers a single
 * byte of data. Like Read/Write Quick, command values are not used by this
 * protocol and thus only a single element (at offset 0) can be specified in
 * the field definition.
 */
// IN: [0] = address
// OUT: [0] = data
forward ioctl_piix4_read_byte(in[], in_size, out[], out_size);
public ioctl_piix4_read_byte(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];

    return piix4_access(in[0], I2C_SMBUS_READ, 0, I2C_SMBUS_BYTE, data, out);
}

// IN: [0] = address, [1] = data
forward ioctl_piix4_write_byte(in[], in_size, out[], out_size);
public ioctl_piix4_write_byte(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_BYTE, data, out);
}

/*
 * The SMBus Read/Write Byte protocol (SMBByte) also transfers a single byte of
 * data. But unlike Send/Receive Byte, this protocol uses a command value to
 * reference up to 256 byte-sized virtual registers.
 */
// IN: [0] = address, [1] = command
// OUT: [0] = data
forward ioctl_piix4_read_byte_data(in[], in_size, out[], out_size);
public ioctl_piix4_read_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];

    return piix4_access(in[0], I2C_SMBUS_READ, in[1], I2C_SMBUS_BYTE_DATA, data, out);
}

// IN: [0] = address, [1] = command, [2] = data
forward ioctl_piix4_write_byte_data(in[], in_size, out[], out_size);
public ioctl_piix4_write_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];
    data[0] = in[2];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_BYTE_DATA, data, out);
}

/*
 * The SMBus Read/Write Word protocol (SMBWord) transfers 2 bytes of data.
 * This protocol also uses a command value to reference up to 256 word-sized
 * virtual device registers.
 */
// IN: [0] = address, [1] = command
// OUT: [0] = data
forward ioctl_piix4_read_word_data(in[], in_size, out[], out_size);
public ioctl_piix4_read_word_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];

    return piix4_access(in[0], I2C_SMBUS_READ, in[1], I2C_SMBUS_WORD_DATA, data, out);
}

// IN: [0] = address, [1] = command, [2] = data
forward ioctl_piix4_write_word_data(in[], in_size, out[], out_size);
public ioctl_piix4_write_word_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];
    data[0] = in[2];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_WORD_DATA, data, out);
}

/*
 * The SMBus Read/Write Block protocol (SMBBlock) transfers variable-sized
 * (0-32 bytes) data. This protocol uses a command value to reference up to 256
 * block-sized virtual registers.
 */
// IN: [0] = address, [1] = command
// OUT: [0] = length, [1...] = data
forward ioctl_piix4_read_block_data(in[], in_size, out[], out_size);
public ioctl_piix4_read_block_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];
    return piix4_access(in[0], I2C_SMBUS_READ, in[1], I2C_SMBUS_BLOCK_DATA, data, out);
}

// IN: [0] = address, [1] = command, [2...] = data
forward ioctl_piix4_write_block_data(in[], in_size, out[], out_size);
public ioctl_piix4_write_block_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new data[I2C_SMBUS_BLOCK_MAX + 1];
    data[0] = in_size - 2;

    if (data[0] > I2C_SMBUS_BLOCK_MAX)
        return STATUS_INVALID_PARAMETER;

    for (new i = 0; i < data[0]; i++)
        data[i+1] = in[i+2];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_BLOCK_DATA, data, out);
}

/*
 * The SMBus Process Call protocol (SMBProcessCall) transfers 2 bytes of data
 * bi-directionally (performs a Write Word followed by a Read Word as an atomic
 * transaction). This protocol uses a command value to reference up to 256
 * word-sized virtual registers.
 */
// IN: [0] = address, [1] = command, [2] = data
// OUT: [0] = data
forward ioctl_piix4_process_call(in[], in_size, out[], out_size);
public ioctl_piix4_process_call(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[1];
    data[0] = in[2];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_PROC_CALL, data, out);
}

/*
 * The SMBus Block Write-Read Block Process Call protocol (SMBBlockProcessCall)
 * transfers a block of data bi-directionally (performs a Write Block followed
 * by a Read Block as an atomic transaction). The maximum aggregate amount of
 * data that may be transferred is limited to 32 bytes. This protocol uses a
 * command value to reference up to 256 block-sized virtual registers.
 */
// IN: [0] = address, [1] = command, [2...] = data
// OUT: [0] = length, [1...] = data
forward ioctl_piix4_block_process_call(in[], in_size, out[], out_size);
public ioctl_piix4_block_process_call(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new data[I2C_SMBUS_BLOCK_MAX + 1]
    data[0] = in_size - 2;

    if (data[0] > I2C_SMBUS_BLOCK_MAX)
        return STATUS_INVALID_PARAMETER;
        
    for (new i = 0; i < data[0]; i++)
        data[i+1] = in[i+2];

    return piix4_access(in[0], I2C_SMBUS_WRITE, in[1], I2C_SMBUS_BLOCK_PROC_CALL, data, out);
}

main() {
    return STATUS_SUCCESS;
}
