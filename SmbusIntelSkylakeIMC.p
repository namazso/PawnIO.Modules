//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026 Blacktempel, Adam Honse <calcprogrammer1@gmail.com>
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

// PawnIO Intel Skylake IMC Driver
// Most of this has been reverse engineered from other software.

// The global SMBus mutex has to be used to stay compliant with other software.
// For example we have received confirmation of HWiNFO and SIV to use the SMBus mutex for this access.

// For reference: https://github.com/Blacktempel/RAMSPDToolkit/

/* i2c_smbus_xfer read or write markers */
#define I2C_SMBUS_READ  1
#define I2C_SMBUS_WRITE 0

#define I2C_SMBUS_QUICK          0
#define I2C_SMBUS_BYTE           1
#define I2C_SMBUS_BYTE_DATA      2
#define I2C_SMBUS_WORD_DATA      3
#define I2C_SMBUS_BLOCK_DATA     5
#define I2C_SMBUS_I2C_BLOCK_DATA 8
#define I2C_SMBUS_BLOCK_MAX      32

//IMC SMBus Register Layout (offsets from PCI config space of IMC / SMBus device)
#define REG_STEP  0x04
#define CMD_BASE  0x9C //Command = CMD_BASE + idx * 4
#define STS_BASE  0xA8 //Status  = STS_BASE + idx * 4
#define DAT_BASE  0xB4 //Data    = DAT_BASE + idx * 4
//#define TSOD_BASE 0xC0 //Temperature Sensor On DIMM = TSOD_BASE + idx * 4

//Bits in CMD
#define WORD_BIT            0x00020000
#define SEL_PTR_BIT         0x00040000
#define WRITE_OPERATION     0x00008000
#define GO_BIT              0x00080000
#define TSOD_ACTIVE_BIT     0x00100000
#define COMMAND_TOGGLE_BIT  0x20000000
#define COMMAND_KEEP_MASK   0xFFEFFFFF
#define COMMAND_PREFIX      0x20080000

#define ADDR_SHIFT      8

//Status bits
#define STS_BUSY        0x1
#define STS_ERROR       0x2
#define STS_READ_DONE   0x4
#define STS_WRITE_DONE  0x8
#define STS_ANY_DONE    (STS_ERROR | STS_READ_DONE | STS_WRITE_DONE)
#define STS_STATE_MASK  0x7

#define PAGE_STATUS_RETRIES         9999
#define PAGE_COMMAND_RETRIES        999
#define TRANSFER_STATUS_RETRIES     99999
#define TRANSFER_TOGGLE_RETRIES     9999

#define MAX_SMBUS_CONTROLLERS 2
#define ADDRESS_SPACE_8BIT_SIZE 0x100

#define CMD_REG CMD_BASE + smbus_index * REG_STEP
#define STS_REG STS_BASE + smbus_index * REG_STEP
#define DAT_REG DAT_BASE + smbus_index * REG_STEP

// PCI slot addresses
// In order of most to least common
new pci_addresses[1][3] = [
    [0x16, 0x1e, 0x5],
];

new pci_address[3];
new smbus_index = 0;

NTSTATUS:intel_imc_init()
{
    new NTSTATUS:status = STATUS_SUCCESS;

    pci_address = pci_addresses[0];

    new cmd = 0;
    new sts = 0;
    new dat = 0;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, cmd);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, sts);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], DAT_REG, dat);
    if (!NT_SUCCESS(status))
        return status;

    return status;
}

bool:ClearTsodStateIfNeeded(oldCommand)
{
    if ((oldCommand & TSOD_ACTIVE_BIT) == 0)
    {
        return true;
    }

    new statusReg = 0;
    for (new i = 0; i < PAGE_STATUS_RETRIES; i++)
    {
        new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, statusReg);
        if (!NT_SUCCESS(status))
        {
            return false;
        }

        if ((statusReg & STS_BUSY) == 0 && (statusReg & STS_ANY_DONE) != 0)
        {
            break;
        }
    }

    new NTSTATUS:status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand & COMMAND_KEEP_MASK);
    if (!NT_SUCCESS(status))
    {
        return false;
    }

    for (new i = 0; i < PAGE_COMMAND_RETRIES; i++)
    {
        new command = 0;
        status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, command);
        if (!NT_SUCCESS(status))
        {
            return false;
        }

        if ((command & GO_BIT) == 0)
        {
            return true;
        }
    }

    return false;
}

bool:WaitTransferComplete(expectedDoneBit, &lastStatus)
{
    lastStatus = 0;

    new doneMask = STS_ERROR | expectedDoneBit;

    for (new i = 0; i < TRANSFER_STATUS_RETRIES; i++)
    {
        new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, lastStatus);
        if (!NT_SUCCESS(status))
        {
            return false;
        }

        if ((lastStatus & STS_BUSY) == 0 && (lastStatus & doneMask) != 0)
        {
            break;
        }
    }

    if ((lastStatus & STS_STATE_MASK) == STS_BUSY)
    {
        for (new i = 0; i < TRANSFER_TOGGLE_RETRIES; i++)
        {
            new command = 0;
            new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, command);
            if (!NT_SUCCESS(status))
            {
                return false;
            }

            status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, command ^ COMMAND_TOGGLE_BIT);
            if (!NT_SUCCESS(status))
            {
                return false;
            }

            status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, lastStatus);
            if (!NT_SUCCESS(status))
            {
                return false;
            }

            if ((lastStatus & STS_STATE_MASK) == STS_BUSY)
            {
                break;
            }
        }

        for (new i = 0; i < TRANSFER_TOGGLE_RETRIES; i++)
        {
            new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, lastStatus);
            if (!NT_SUCCESS(status))
            {
                return false;
            }

            if ((lastStatus & STS_BUSY) == 0 && (lastStatus & doneMask) != 0)
            {
                break;
            }
        }
    }

    return (lastStatus & doneMask) == expectedDoneBit;
}

NTSTATUS:ImcAccess(command, read_write, addr, hstcmd, &value)
{
    if (hstcmd != I2C_SMBUS_BYTE
     && hstcmd != I2C_SMBUS_BYTE_DATA
     && hstcmd != I2C_SMBUS_WORD_DATA)
    {
        debug_print(''Unsupported transaction %d\n'', hstcmd);
        return STATUS_NOT_SUPPORTED;
    }

    if (read_write != I2C_SMBUS_READ && read_write != I2C_SMBUS_WRITE)
    {
        return STATUS_INVALID_PARAMETER;
    }

    new oldCommand = 0;
    new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);
    if (!NT_SUCCESS(status))
    {
        return status;
    }

    if (!ClearTsodStateIfNeeded(oldCommand))
    {
        return STATUS_DEVICE_BUSY;
    }

    new cmd = COMMAND_PREFIX
            | (addr & 0xFF) << ADDR_SHIFT
            | (command & 0xFF) ;

    if (hstcmd == I2C_SMBUS_BYTE)
    {
        cmd |= SEL_PTR_BIT;
    }

    if (hstcmd == I2C_SMBUS_WORD_DATA)
    {
        cmd |= WORD_BIT;
    }

    if (read_write == I2C_SMBUS_WRITE)
    {
        new writeData = value & 0xFF;

        if (hstcmd == I2C_SMBUS_WORD_DATA)
        {
            writeData = ((value & 0xFF00) >> 8) | ((value & 0x00FF) << 8);
        }

        status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], DAT_REG, writeData << 16);
        if (!NT_SUCCESS(status))
        {
            return status;
        }

        cmd |= WRITE_OPERATION;
    }

    status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, cmd);
    if (!NT_SUCCESS(status))
    {
        return status;
    }

    new lastStatus = 0;
    new expectedDoneBit = read_write == I2C_SMBUS_WRITE ? STS_WRITE_DONE : STS_READ_DONE;
    if (WaitTransferComplete(expectedDoneBit, lastStatus))
    {
        if (read_write == I2C_SMBUS_WRITE)
        {
            pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);
            return STATUS_SUCCESS;
        }

        new dataReg = 0;
        status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], DAT_REG, dataReg);
        pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);

        if (!NT_SUCCESS(status))
        {
            return status;
        }

        if( hstcmd == I2C_SMBUS_BYTE
         || hstcmd == I2C_SMBUS_BYTE_DATA)
        {
            value = dataReg & 0xFF;
            return STATUS_SUCCESS;
        }

        if(hstcmd == I2C_SMBUS_WORD_DATA)
        {
            //Swap high / low
            value = ((dataReg & 0xFF00) >> 8) | ((dataReg & 0x00FF) << 8);
            return STATUS_SUCCESS;
        }
    }

    pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);

    return STATUS_DEVICE_BUSY;
}

/// SMBus transfer.
///
/// Performs a transfer of data over the SMBus using the specified command.
/// I2C_SMBUS_BYTE_DATA (2)
/// I2C_SMBUS_WORD_DATA (3)
///
/// @param in [0] = Address, [1] = Read(1)/Write(0), [2] = Command, [3] = Protocol, [4] Data
/// @param out [0] = Read data
/// @param out_size Must be 5
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
DEFINE_IOCTL_SIZED(ioctl_smbus_xfer, 9, 5)
{
    new addr = in[0];
    new read_write = in[1];
    new command = in[2];
    new hstcmd = in[3];

    // This controller does not appear to support QUICK or BLOCK operations
    if (hstcmd == I2C_SMBUS_QUICK
     || hstcmd == I2C_SMBUS_BLOCK_DATA
     || hstcmd == I2C_SMBUS_I2C_BLOCK_DATA)
    {
        return STATUS_NOT_SUPPORTED;
    }

    new value = in[4];
    new NTSTATUS:status = ImcAccess(command, read_write, addr, hstcmd, value);
    if (!NT_SUCCESS(status))
    {
        return status;
    }

    out[0] = value;
    return STATUS_SUCCESS;
}

/// Identify the SMBus controller.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Type of the SMBus controller, [1] = I/O Base address, [2] = PCI Identifiers
/// @param out_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_identity, 0, 3)
{
    new NTSTATUS:status;

    out[0] = CHAR8_CONST('I', 'n', 't', 'e', 'l', 'I', 'M', 'C');

    out[1] = 0;

    //Read the PCI vendor/device ID
    new pci_ids;
    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], 0x00, pci_ids);
    if (!NT_SUCCESS(status))
        return status;

    //Read the PCI subsystem vendor/device ID
    new pci_subsys_ids;
    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], 0x2C, pci_subsys_ids);
    if (!NT_SUCCESS(status))
        return status;

    out[2] = pci_ids | (pci_subsys_ids << 32);

    return STATUS_SUCCESS;
}

/// Set the SMBus index.
///
/// @param in [0] SMBus-index (0 or 1)
/// @param in_size Must be 1
/// @param out [0] Unused
/// @param out_size Unused
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_smbus_index, 1, 0)
{
    smbus_index = in[0] & 0x1;

    debug_print(''Set SMBus-index to %d\n'', smbus_index);

    return STATUS_SUCCESS;
}

NTSTATUS:main()
{
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return intel_imc_init();
}
