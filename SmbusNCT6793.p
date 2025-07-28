//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  Adam Honse <calcprogrammer1@gmail.com>
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

// Nuvoton NCT6793D (and compatible) Super IO SMBus Driver
// Adapted to Windows/PawnIO from my out-of-tree i2c-nct6793 Linux driver
// See https://gitlab.com/CalcProgrammer1/i2c-nct6793-dkms

/* Nuvoton SMBus address offsets */
#define SMBHSTDAT                   (nuvoton_nct6793_smba + 0x0)
#define SMBBLKSZ                    (nuvoton_nct6793_smba + 0x1)
#define SMBHSTCMD                   (nuvoton_nct6793_smba + 0x2)
#define SMBHSTIDX                   (nuvoton_nct6793_smba + 0x3)
#define SMBHSTCTL                   (nuvoton_nct6793_smba + 0x4)
#define SMBHSTADD                   (nuvoton_nct6793_smba + 0x5)
#define SMBHSTCLK                   (nuvoton_nct6793_smba + 0x6)
#define SMBHSTERR                   (nuvoton_nct6793_smba + 0x9)
#define SMBHSTSTS                   (nuvoton_nct6793_smba + 0xE)

/* Command register */
#define NCT6793_READ_BYTE           0
#define NCT6793_READ_WORD           1
#define NCT6793_READ_BLOCK          2
#define NCT6793D_BLOCK_PROC_CALL    3
#define NCT6793_PROC_CALL           4
#define NCT6793_WRITE_BYTE          8
#define NCT6793_WRITE_WORD          9
#define NCT6793_WRITE_BLOCK         10

/* Control register */
#define NCT6793_MANUAL_START        BIT(7)
#define NCT6793_SOFT_RESET          BIT(6)

/* Error register */
#define NCT6793_NO_ACK              BIT(5)

/* Status register */
#define NCT6793_FIFO_EMPTY          BIT(0)
#define NCT6793_FIFO_FULL           BIT(1)
#define NCT6793_MANUAL_ACTIVE       BIT(2)

#define NCT6793_LD_SMBUS            0x0B

#define SIO_REG_LDSEL               0x07    /* Logical device select            */
#define SIO_REG_DEVID               0x20    /* Device ID (2 bytes)              */
#define SIO_REG_SMBA                0x62    /* SMBus base address register      */
#define SIO_REG_LOGDEV              0x07    /* Logical Device Register          */

#define SIO_NCT6791_ID              0xc800
#define SIO_NCT6792_ID              0xc910
#define SIO_NCT6793_ID              0xd120
#define SIO_NCT6795_ID              0xd350
#define SIO_NCT6796_ID              0xd420
#define SIO_NCT6798_ID              0xd428
#define SIO_ID_MASK                 0xFFF0

#define I2C_SMBUS_BLOCK_MAX         32      /* As specified in SMBus standard   */

/* i2c_smbus_xfer read or write markers */
#define I2C_SMBUS_READ              1
#define I2C_SMBUS_WRITE             0

/* SMBus transaction types (size parameter in the above functions) */
#define I2C_SMBUS_QUICK             0
#define I2C_SMBUS_BYTE              1
#define I2C_SMBUS_BYTE_DATA         2
#define I2C_SMBUS_WORD_DATA         3
#define I2C_SMBUS_PROC_CALL         4
#define I2C_SMBUS_BLOCK_DATA        5
#define I2C_SMBUS_I2C_BLOCK_BROKEN  6
#define I2C_SMBUS_BLOCK_PROC_CALL   7       /* SMBus 2.0                        */
#define I2C_SMBUS_I2C_BLOCK_DATA    8

/* Each retry happens after 250us, and a FIFO of 4 bytes (A+RW, IDX, LEN, 32 DATA) takes at most 0.7ms at 100KHz */
/* A full 32-byte block requires waiting 4 bytes at a time before refilling the 4-byte FIFO */
/* Allow up to 2ms wait, or 8 retries */
#define MAX_RETRIES                 8

new nuvoton_nct6793_smba    = 0;
new driver_name             = 0;
new smbus_clock             = 0;

new const k_clock_vals[16] = [365000, 261000, 200000, 162000, 136000, 117000, 103000, 99000, 83000, 76000, 71000, 65000, 61000, 57000, 53000, 47000];

superio_enter(addr)
{
    io_out_byte(addr, 0x87);
    io_out_byte(addr, 0x87);
}

superio_inb(addr, reg)
{
    io_out_byte(addr, reg);
    return(io_in_byte(addr + 1));
}

superio_outb(addr, reg, val)
{
    io_out_byte(addr, reg);
    io_out_byte((addr + 1), val);
}

NTSTATUS:nct6793_init()
{
    new sioaddr = 0x2e;
    new val;

    superio_enter(sioaddr);

    /* Read SuperIO ID and test if it is a supported device */
    val = (superio_inb(sioaddr, SIO_REG_DEVID) << 8) | (superio_inb(sioaddr, SIO_REG_DEVID + 1));

    switch(val & SIO_ID_MASK)
    {
        case SIO_NCT6791_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '1');
            }

        case SIO_NCT6792_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '2');
            }

        case SIO_NCT6793_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '3');
            }

        case SIO_NCT6795_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '5');
            }

        case SIO_NCT6796_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '6');
            }

        case SIO_NCT6798_ID:
            {
                driver_name = CHAR7_CONST('N', 'C', 'T', '6', '7', '9', '8');
            }
        
        default:
            {
                return STATUS_NOT_SUPPORTED;
            }
    }

    /* Enable SMBus logical device */
    superio_outb(sioaddr, SIO_REG_LOGDEV, NCT6793_LD_SMBUS);

    /* Determine base address */
    nuvoton_nct6793_smba = (superio_inb(sioaddr, SIO_REG_SMBA) << 8) | superio_inb(sioaddr, SIO_REG_SMBA + 1);

    /* Read initial clock setting */
    smbus_clock = (io_in_byte(SMBHSTCLK) & 0xF);

    return STATUS_SUCCESS;
}

NTSTATUS:nct6793_access(addr, read_write, command, size, in[5], out[5])
{
    new cnt;
    new i;
    new len;
    new timeout;

    /* Perform soft reset of SMBus controller */
    io_out_byte(SMBHSTCTL, NCT6793_SOFT_RESET);

    /* Write the clock value as long as it is valid */
    if(smbus_clock < 16)
    {
        io_out_byte(SMBHSTCLK, smbus_clock);
    }

    switch(size)
    {
        case I2C_SMBUS_BYTE_DATA:
            {
                /* Write the address + RW into SMBHSTADD, the command into SMBHSTIDX */
                io_out_byte(SMBHSTADD, (addr << 1) | read_write);
                io_out_byte(SMBHSTIDX, command);

                /* If write, write one byte of data into SMBHSTDAT */
                if(read_write == I2C_SMBUS_WRITE)
                {
                    io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, 0));
                    io_out_byte(SMBHSTCMD, NCT6793_WRITE_BYTE);
                }
                else
                {
                    io_out_byte(SMBHSTCMD, NCT6793_READ_BYTE);
                }
            }

        case I2C_SMBUS_WORD_DATA:
            {
                /* Write the address + RW into SMBHSTADD, the command into SMBHSTIDX */
                io_out_byte(SMBHSTADD, (addr << 1) | read_write);
                io_out_byte(SMBHSTIDX, command);

                /* If write, write two bytes of data into SMBHSTDAT */
                if(read_write == I2C_SMBUS_WRITE)
                {
                    io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, 0));
                    io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, 1));
                    io_out_byte(SMBHSTCMD, NCT6793_WRITE_WORD);
                }
                else
                {
                    io_out_byte(SMBHSTCMD, NCT6793_READ_WORD);
                }
            }

        case I2C_SMBUS_BLOCK_DATA:
            {
                /* Write the address + RW into SMBHSTADD, the command into SMBHSTIDX */
                io_out_byte(SMBHSTADD, (addr << 1) | read_write);
                io_out_byte(SMBHSTIDX, command);

                /* If write, write up to 4 bytes into SMBHSTDAT */
                if(read_write == I2C_SMBUS_WRITE)
                {
                    len = GET_BYTE_LE(in, 0);
                    if(len == 0 || len > I2C_SMBUS_BLOCK_MAX)
                    {
                        return STATUS_INVALID_BLOCK_LENGTH;
                    }

                    io_out_byte(SMBBLKSZ, len);

                    cnt = 1;
                    if(len >= 4)
                    {
                        for(i = cnt; i <= 4; i++)
                        {
                            io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, i));
                        }

                        len -= 4;
                        cnt += 4;
                    }
                    else
                    {
                        for(i = cnt; i <= len; i++)
                        {
                            io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, i));
                        }

                        len = 0;
                    }

                    io_out_byte(SMBHSTCMD, NCT6793_WRITE_BLOCK);
                }

                /* Block read not supported by this driver */
                else
                {
                    return STATUS_NOT_SUPPORTED;
                }
            }
        
        default:
            {
                return STATUS_NOT_SUPPORTED;
            }
    }

    /* Begin manual transmission */
    io_out_byte(SMBHSTCTL, NCT6793_MANUAL_START);

    /* Handle filling FIFO in block transmit mode */
    while(size == I2C_SMBUS_BLOCK_DATA && len > 0)
    {
        if(read_write == I2C_SMBUS_WRITE)
        {
            timeout = 0;
            while((io_in_byte(SMBHSTSTS) & NCT6793_FIFO_EMPTY) == 0)
            {
                if(timeout > MAX_RETRIES)
                {
                    return STATUS_TIMEOUT;
                }

                microsleep2(250);
                timeout++;
            }

            /* Load more bytes into FIFO */
            if(len >= 4)
            {
                for(i = cnt; i <= (cnt + 4); i++)
                {
                    io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, i));
                }

                len -= 4;
                cnt += 4;
            }
            else
            {
                for(i = cnt; i <= (cnt + len); i++)
                {
                    io_out_byte(SMBHSTDAT, GET_BYTE_LE(in, i));
                }

                len = 0;
            }
        }
    }

    /* Wait for manual mode to complete */
    timeout = 0;
    while((io_in_byte(SMBHSTSTS) & NCT6793_MANUAL_ACTIVE) != 0)
    {
        if(timeout > MAX_RETRIES)
        {
            return STATUS_TIMEOUT;
        }

        microsleep2(250);
        timeout++;
    }

    /* Check if transmission not acked */
    if((io_in_byte(SMBHSTERR) & NCT6793_NO_ACK) != 0)
    {
        return STATUS_NO_SUCH_DEVICE;
    }

    if(read_write == I2C_SMBUS_WRITE)
    {
        return STATUS_SUCCESS;
    }

    /* Handle reading out received data */
    switch(size)
    {
        case I2C_SMBUS_BYTE_DATA:
            {
                SET_BYTE_LE(out, 0, io_in_byte(SMBHSTDAT));
            }

        case I2C_SMBUS_WORD_DATA:
            {
                SET_BYTE_LE(out, 0, io_in_byte(SMBHSTDAT));
                SET_BYTE_LE(out, 1, io_in_byte(SMBHSTDAT));
            }
    }

    return STATUS_SUCCESS;
}

/// Identify the SMBus controller.
///
/// @param out [0] = Type of the SMBus controller, [1] = I/O Base address, [2] = PCI vendor/device ID, subsystem vendor/device ID
/// @param out_size Must be 3
/// @return An NTSTATUS
forward NTSTATUS:ioctl_identity(in[1], in_size, out[3], out_size);
public NTSTATUS:ioctl_identity(in[1], in_size, out[3], out_size)
{
    if (out_size < 3)
    {
        return STATUS_BUFFER_TOO_SMALL;
    }

    /* Fill in driver name */
    out[0] = driver_name;

    /* Fill in base address */
    out[1] = nuvoton_nct6793_smba;

    /* Not a PCI device, set PCI information to invalid (FFFF:FFFF, FFFF:FFFF) */
    out[2] = 0xffffffffffffffff;

    return STATUS_SUCCESS;
}

/// Set the SMBus clock frequency.
///
/// @param in [0] = Frequency in Hz or -1 for no change
/// @param in_size Must be 1
/// @param out [0] = Previous frequency in Hz
/// @param out_size Must be 1
/// @note The NCT6793 controller use a fixed clock frequency.
/// @return An NTSTATUS
forward NTSTATUS:ioctl_clock_freq(in[1], in_size, out[1], out_size);
public NTSTATUS:ioctl_clock_freq(in[1], in_size, out[1], out_size)
{
    if(in_size < 1 || out_size < 1)
    {
        return STATUS_BUFFER_TOO_SMALL;
    }

    /* Read previous frequency */
    out[0] = k_clock_vals[(io_in_byte(SMBHSTCLK) & 0xF)];

    /* If not -1, try to write new frequency */
    if(in[0] != -1)
    {
        /* Check if new frequency matches any supported frequency */
        for(new i = 0; i < 16; i++)
        {
            if(in[0] == k_clock_vals[i])
            {
                smbus_clock = i;
                io_out_byte(SMBHSTCLK, smbus_clock);
                return STATUS_SUCCESS;
            }
        }
        return STATUS_NOT_SUPPORTED;
    }
    else
    {
        return STATUS_SUCCESS;
    }
}

/// SMBus transfer.
///
/// Performs a transfer of data over the SMBus using the specified command.
/// I2C_SMBUS_QUICK (protocol 0) only requires the address and read/write parameters, command must be left as 0.
/// I2C_SMBUS_BYTE (1), I2C_SMBUS_BYTE_DATA (2), and I2C_SMBUS_WORD_DATA (3) require the address, read/write, command, and data[0] (write only) parameters.
/// I2C_SMBUS_BLOCK_DATA (5) requires the address, read/write, command, data as length, and array data parameters.
///
/// @param in [0] = Address, [1] = Read(1)/Write(0), [2] = Command, [3] = Protocol, [4] Data, [5..9] = Array Data (byte packed)
/// @param in_size Must be between 4 and 9
/// @param out [0] = Length in bytes, [1..5] = Data (byte packed)
/// @param out_size Must be between 0 and 5
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_smbus_xfer(in[9], in_size, out[5], out_size);
public NTSTATUS:ioctl_smbus_xfer(in[9], in_size, out[5], out_size)
{
    if(in_size < 9 || out_size < 5)
    {
        return STATUS_BUFFER_TOO_SMALL;
    }

    /* Extract data fields from input */
    new address     = in[0];
    new read_write  = in[1];
    new command     = in[2];
    new hstcmd      = in[3];
    new in_data[5];

    in_data[0]      = in[4];
    in_data[1]      = in[5];
    in_data[2]      = in[6];
    in_data[3]      = in[7];
    in_data[4]      = in[8];

    return nct6793_access(address, read_write, command, hstcmd, in_data, out);
}

NTSTATUS:main()
{
    if(get_arch() != ARCH_X64)
    {
        return STATUS_NOT_SUPPORTED;
    }

    return nct6793_init();
}