//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026 Blacktempel
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

// PawnIO Intel PCU Driver
// Most of this has been reverse engineered from other software.

// The global SMBus mutex has to be used to stay compliant with other software.
// For example we have received confirmation of HWiNFO and SIV to use the SMBus mutex for this access.

// For reference: https://github.com/Blacktempel/RAMSPDToolkit/

/* i2c_smbus_xfer read or write markers */
#define I2C_SMBUS_READ	1
#define I2C_SMBUS_WRITE	0

#define I2C_SMBUS_BYTE_DATA		    2
#define I2C_SMBUS_WORD_DATA		    3

//PCU SMBus Register Layout (offsets from PCI config space of PCU / SMBus device)
#define REG_STEP 0x04
#define CMD_BASE 0x9C //Command = CMD_BASE + idx * 4
#define STS_BASE 0xA8 //Status  = STS_BASE + idx * 4
#define DAT_BASE 0xB4 //Data    = DAT_BASE + idx * 4
//#define TSOD_BASE 0xC0 //Temperature Sensor On DIMM = TSOD_BASE + idx * 4

//Bits in CMD
#define GO_BIT          0x80000
#define WORD_BIT        0x20000
#define PEC_BIT         0x100000 //Unused here
#define CMD_MASK_KEEP   0xFFE00000 //Keep upper bits from old CMD
#define SLOT_SHIFT      8
#define OP_SHIFT        11

#define BANK_MASK       0xB000
#define BANK_0_MASK     0x600
#define BANK_1_MASK     0x700

//Status bits
#define STS_BUSY    0x1
#define STS_ERROR   0x2

#define START_RETRIES       5
#define CMD_DELAY_MS        3
#define POLL_SLEEP_MS       1
#define POLL_TIMEOUT_MS     10

#define MAX_SMBUS_CONTROLLERS 2

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

NTSTATUS:intel_pcu_init()
{
    new NTSTATUS:status;

    pci_address = pci_addresses[0];

    new cmd = 0;
    new sts = 0;
    new dat = 0;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, cmd);
    if (status != STATUS_SUCCESS)
        return status;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, sts);
    if (status != STATUS_SUCCESS)
        return status;

    status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], DAT_REG, dat);
    if (status != STATUS_SUCCESS)
        return status;

    return status;
}

int:CheckElapsed(int:start)
{
    return int:(get_tick_count()) - start;
}

bool:WaitReady()
{
    new int:start = int:(get_tick_count());

    while (true)
    {
        new status = 0;
        pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, status);

        if ((status & STS_BUSY) == 0)
        {
            return true;
        }

        if (CheckElapsed(start) > int:(POLL_TIMEOUT_MS))
        {
            return false;
        }

        microsleep(POLL_SLEEP_MS * 1000);
    }
}

bool:WaitDone()
{
    new int:start = int:(get_tick_count());

    while (true)
    {
        new lastStatus = 0;
        pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], STS_REG, lastStatus);

        if ((lastStatus & STS_BUSY) == 0)
        {
            if ((lastStatus & STS_ERROR) != 0)
            {
                return false;
            }

            return true;
        }

        if (CheckElapsed(start) > int:(POLL_TIMEOUT_MS))
        {
            return false;
        }

        microsleep(POLL_SLEEP_MS * 1000);
    }
}

/// SMBus transfer.
///
/// Performs a transfer of data over the SMBus using the specified command.
/// I2C_SMBUS_BYTE_DATA (2)
/// I2C_SMBUS_WORD_DATA (3)
///
/// @param in [0] = Address/Offset, [1] = Read(1)/Write(0), [2] = Command (Opcode & Slot), [3] = Protocol
/// @param in_size Must be 4
/// @param out [0]
/// @param out_size Must be at least 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
DEFINE_IOCTL(ioctl_smbus_xfer)
{
    if (in_size < 4)
        return STATUS_BUFFER_TOO_SMALL;

    new offset = in[0];
    new read_write = in[1];
    new command = in[2];
    new hstcmd = in[3];

    new NTSTATUS:status;

    if (hstcmd != I2C_SMBUS_BYTE_DATA
     && hstcmd != I2C_SMBUS_WORD_DATA)
    {
        debug_print(''Unsupported transaction %d\n'', hstcmd);
        status = STATUS_NOT_SUPPORTED;
    }

    if (read_write == I2C_SMBUS_WRITE)
    {
        debug_print(''Write is unsupported %d\n'', read_write);
        status = STATUS_NOT_SUPPORTED;
    }

    new opcode = command >> 4;
    new slot = command & 0x0F;

    for (new i; i < START_RETRIES; i++)
    {
        if (!WaitReady())
        {
            continue;
        }

        new oldCommand = 0;
        status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);

        new cmd = (oldCommand & CMD_MASK_KEEP)
                | ((opcode & 0xF) << OP_SHIFT)
                | ((slot & 0x7) << SLOT_SHIFT)
                | offset;

        if (hstcmd == I2C_SMBUS_WORD_DATA)
        {
            cmd |= WORD_BIT;
        }

        cmd |= GO_BIT;

        status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, cmd);
        microsleep(CMD_DELAY_MS * 1000);

        if (WaitDone())
        {
            new dataReg = 0;
            status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], DAT_REG, dataReg);

            switch (hstcmd)
            {
                case I2C_SMBUS_BYTE_DATA:
                {
                    out[0] = dataReg & 0xFF;
                    return STATUS_SUCCESS;
                }
                case I2C_SMBUS_WORD_DATA:
                {
                    //Swap high / low
                    out[0] = ((dataReg & 0xFF00) >> 8) | ((dataReg & 0x00FF) << 8);
                    return STATUS_SUCCESS;
                }
                default:
                {
                    debug_print(''Unsupported transaction %d\n'', hstcmd);
                    status = STATUS_NOT_SUPPORTED;
                }
            }
        }

        microsleep(CMD_DELAY_MS * 1000);
    }

    status = STATUS_NOT_SUPPORTED;

    return status;
}

/// Set the bank index.
///
/// @param in [0] Bank index (0 or 1)
/// @param in_size Must be 1
/// @param out [0] Unused
/// @param out_size Unused
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_set_bank, 1, 0)
{
    new bankIndex = in[0];

    if (bankIndex < 0 || bankIndex > 1)
    {
        return STATUS_NO_SUCH_DEVICE;
    }

    for (new i; i < START_RETRIES; i++)
    {
        if (!WaitReady())
        {
            continue;
        }

        new oldCommand = 0;
        new NTSTATUS:status = pci_config_read_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, oldCommand);

        new bankIndexMask = bankIndex == 0 ? BANK_0_MASK : BANK_1_MASK;
        new cmd = (oldCommand & CMD_MASK_KEEP)
                | BANK_MASK
                | bankIndexMask
                | GO_BIT;

        status = pci_config_write_dword(pci_address[0], pci_address[1], pci_address[2], CMD_REG, cmd);
        microsleep(CMD_DELAY_MS * 1000);

        if (WaitDone())
        {
            debug_print(''Set Bank index to %d\n'', bankIndex);

            return status;
        }

        microsleep(CMD_DELAY_MS * 1000);
    }

    return STATUS_DEVICE_BUSY;
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

    out[0] = CHAR8_CONST('I', 'n', 't', 'e', 'l', 'P', 'C', 'U');

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
    smbus_index = in[0];

    debug_print(''Set SMBus-index to %d\n'', smbus_index);

    return STATUS_SUCCESS;
}

NTSTATUS:main()
{
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return intel_pcu_init();
}

