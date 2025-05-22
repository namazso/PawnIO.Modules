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

// PawnIO i801 Driver
// Many parts of this was ported from the Linux kernel codebase.
// See https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/i2c/busses/i2c-i801.c

#define PCI_VENDOR_ID_INTEL 0x8086

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

/* I801 SMBus address offsets */
#define SMBHSTSTS	(0 + i801_smba)
#define SMBHSTCNT	(2 + i801_smba)
#define SMBHSTCMD	(3 + i801_smba)
#define SMBHSTADD	(4 + i801_smba)
#define SMBHSTDAT0	(5 + i801_smba)
#define SMBHSTDAT1	(6 + i801_smba)
#define SMBBLKDAT	(7 + i801_smba)
#define SMBPEC		(8 + i801_smba)		/* ICH3 and later */
#define SMBAUXSTS	(12 + i801_smba)	/* ICH4 and later */
#define SMBAUXCTL	(13 + i801_smba)	/* ICH4 and later */
#define SMBSLVSTS	(16 + i801_smba)	/* ICH3 and later */
#define SMBSLVCMD	(17 + i801_smba)	/* ICH3 and later */
#define SMBNTFDADD	(20 + i801_smba)	/* ICH3 and later */

/* PCI Address Constants */
#define SMB_BASE	0x020
#define SMBHSTCFG	0x040
#define TCOBASE		0x050
#define TCOCTL		0x054

/* Host configuration bits for SMBHSTCFG */
#define SMBHSTCFG_HST_EN		BIT(0)
#define SMBHSTCFG_SMB_SMI_EN	BIT(1)
#define SMBHSTCFG_I2C_EN		BIT(2)
#define SMBHSTCFG_SPD_WD		BIT(4)

/* Auxiliary status register bits, ICH4+ only */
#define SMBAUXSTS_CRCE			BIT(0)
#define SMBAUXSTS_STCO			BIT(1)

/* Auxiliary control register bits, ICH4+ only */
#define SMBAUXCTL_CRC			BIT(0)
#define SMBAUXCTL_E32B			BIT(1)

/* I801 command constants */
#define I801_QUICK				0x00
#define I801_BYTE				0x04
#define I801_BYTE_DATA			0x08
#define I801_WORD_DATA			0x0C
#define I801_PROC_CALL			0x10
#define I801_BLOCK_DATA			0x14
#define I801_I2C_BLOCK_DATA		0x18	/* ICH5 and later */
#define I801_BLOCK_PROC_CALL	0x1C

/* I801 Host Control register bits */
#define SMBHSTCNT_INTREN		BIT(0)
#define SMBHSTCNT_KILL			BIT(1)
#define SMBHSTCNT_LAST_BYTE		BIT(5)
#define SMBHSTCNT_START			BIT(6)
#define SMBHSTCNT_PEC_EN		BIT(7)	/* ICH3 and later */

/* I801 Hosts Status register bits */
#define SMBHSTSTS_BYTE_DONE		BIT(7)
#define SMBHSTSTS_INUSE_STS		BIT(6)
#define SMBHSTSTS_SMBALERT_STS	BIT(5)
#define SMBHSTSTS_FAILED		BIT(4)
#define SMBHSTSTS_BUS_ERR		BIT(3)
#define SMBHSTSTS_DEV_ERR		BIT(2)
#define SMBHSTSTS_INTR			BIT(1)
#define SMBHSTSTS_HOST_BUSY		BIT(0)

#define STATUS_ERROR_FLAGS	(SMBHSTSTS_FAILED | SMBHSTSTS_BUS_ERR | \
                 SMBHSTSTS_DEV_ERR)

#define STATUS_FLAGS		(SMBHSTSTS_BYTE_DONE | SMBHSTSTS_INTR | \
                 STATUS_ERROR_FLAGS)

#define SMBUS_LEN_SENTINEL (I2C_SMBUS_BLOCK_MAX + 1)

// A 32 byte block process at 10MHz is 62.3ms, 80ms should be plenty
#define MAX_TIMEOUT 80

// PCI slot addresses
// In order of most to least common
new pci_addresses[4][3] = [
    [0x00, 0x1f, 0x4],
    [0x00, 0x1f, 0x3],
    [0x00, 0x1f, 0x1],
    [0x80, 0x1f, 0x4],
    // These are only used by a few server boards
    // which PawnIO is very unlikely to be running on
    /* [0x00, 0x0f, 0x0],
    // These are also secondary addresses, which aren't handled by this driver
    [0x00, 0x11, 0x1],
    [0x00, 0x11, 0x3],
    [0x03, 0x00, 0x3],
    [0x03, 0x00, 0x4], */
];

// Supported PCI device IDs
// ICH4 introduces CRC and ICH5 introduces block reads
// So we don't support older chips
new pci_devices[] = [
    // 0x2413,     // 82801AA (ICH) - 00:1f.3
    // 0x2423,     // 82801AA (ICH)
    // 0x2443,     // 82801BA (ICH2) - 00:1f.3
    // 0x2483,     // 82801CA (ICH3) - 00:1f.3
    // 0x24c3,     // 82801DB (ICH4) - 00:1f.3
    0x24d3,     // 82801E (ICH5) - 00:1f.3
    0x25a4,     // 6300ESB - 00:1f.3
    0x266a,     // 82801F (ICH6) - 00:1f.3
    0x269b,     // 6310ESB/6320ESB - 00:1f.3
    0x27da,     // 82801G (ICH7) - 00:1f.3
    0x283e,     // 82801H (ICH8) - 00:1f.3
    0x2930,     // 82801I (ICH9) - 00:1f.3
    0x5032,     // EP80579 (Tolapai)
    0x3a30,     // ICH10 - 00:1f.3
    0x3a60,     // ICH10 - 00:1f.3
    0x3b30,     // 5/3400 Series (PCH) - 00:1f.3
    0x1c22,     // 6 Series (PCH) - 00:1f.3
    0x1d22,     // Patsburg (PCH) - 00:1f.3
    // 0x1d70,     // Patsburg (PCH) IDF - 03:00.3
    // 0x1d71,     // Patsburg (PCH) IDF - 03:00.4
    // 0x1d72,     // Patsburg (PCH) IDF
    0x2330,     // DH89xxCC (PCH) - 00:1f.3
    0x1e22,     // Panther Point (PCH) - 00:1f.3
    0x8c22,     // Lynx Point (PCH) - 00:1f.3
    0x9c22,     // Lynx Point-LP (PCH) - 00:1f.3
    0x1f3c,     // Avoton (SOC) - 00:1f.3
    0x8d22,     // Wellsburg (PCH) - 00:1f.3
    // 0x8d7d,     // Wellsburg (PCH) MS - 00:11.1
    // 0x8d7e,     // Wellsburg (PCH) MS
    // 0x8d7f,     // Wellsburg (PCH) MS - 00:11.3
    0x23b0,     // Coleto Creek (PCH) - 00:1f.3
    0x8ca2,     // Wildcat Point (PCH) - 00:1f.3
    0x9ca2,     // Wildcat Point-LP (PCH) - 00:1f.3
    0x0f12,     // BayTrail (SOC) - 00:1f.3
    0x2292,     // Braswell (SOC) - 00:1f.3
    0xa123,     // Sunrise Point-H (PCH) - 00:1f.4
    0x9d23,     // Sunrise Point-LP (PCH) - 00:1f.4
    0x19df,     // DNV (SOC) - 00:1f.4
    0x1bc9,     // Emmitsburg (PCH) - 00:1f.4
    0x5ad4,     // Broxton (SOC) - 00:1f.1
    0xa1a3,     // Lewisburg (PCH) - 00:1f.4
    0xa223,     // Lewisburg Supersku (PCH)
    0xa2a3,     // Kaby Lake PCH-H (PCH) - 00:1f.4
    0x31d4,     // Gemini Lake (SOC) - 00:1f.1
    0xa323,     // Cannon Lake-H (PCH) - 00:1f.4
    0x9da3,     // Cannon Lake-LP (PCH) - 00:1f.4
    // 0x18df,     // Cedar Fork (PCH) - 00:0f.0
    0x34a3,     // Ice Lake-LP (PCH) - 00:1f.4
    0x38a3,     // Ice Lake-N (PCH) - 00:1f.4
    0x02a3,     // Comet Lake (PCH) - 00:1f.4
    0x06a3,     // Comet Lake-H (PCH) - 00:1f.4
    0x4b23,     // Elkhart Lake (PCH) - 00:1f.4
    0xa0a3,     // Tiger Lake-LP (PCH) - 00:1f.4
    0x43a3,     // Tiger Lake-H (PCH) - 00:1f.4
    0x4da3,     // Jasper Lake (SOC) - 00:1f.4
    0xa3a3,     // Comet Lake-V (PCH) - 00:1f.4
    0x7aa3,     // Alder Lake-S (PCH) - 00:1f.4
    0x51a3,     // Alder Lake-P (PCH) - 00:1f.4
    0x54a3,     // Alder Lake-M (PCH) - 00:1f.4
    0x7a23,     // Raptor Lake-S (PCH) - 00:1f.4
    0x7e22,     // Meteor Lake-P (SOC) - 00:1f.4
    0xae22,     // Meteor Lake SoC-S (SOC)
    0x7f23,     // Meteor Lake PCH-S (PCH) - 80:1f.4
    0x5796,     // Birch Stream (SOC) - 00:1f.4
    0x7722,     // Arrow Lake-H (SOC) - 00:1f.4
    0xe322,     // Panther Lake-H (SOC)
    0xe422,     // Panther Lake-P (SOC)
];

new pci_addr[3];
new i801_smba;

NTSTATUS:i801_init()
{
    new NTSTATUS:status;
    new pci_config;
    new bool:found = false;

    for (new i; i < sizeof pci_addresses; i++) {
        // Check the vendor ID
        status = pci_config_read_word(pci_addresses[i][0], pci_addresses[i][1], pci_addresses[i][2], 0x00, pci_config);
        if (!NT_SUCCESS(status) || pci_config != PCI_VENDOR_ID_INTEL)
            continue;
        // Check the device class is SMBUS (Base 0Ch, Sub 05h)
        status = pci_config_read_word(pci_addresses[i][0], pci_addresses[i][1], pci_addresses[i][2], 0x0A, pci_config);
        if (!NT_SUCCESS(status) || pci_config != 0x0c05)
            continue;
        // Check the device ID
        status = pci_config_read_word(pci_addresses[i][0], pci_addresses[i][1], pci_addresses[i][2], 0x02, pci_config);
        if (!NT_SUCCESS(status))
            continue;
        for (new j; j < sizeof pci_devices; j++) {
            if (pci_config == pci_devices[j]) {
                pci_addr = pci_addresses[i];
                found = true;
                break;
            }
        }

    }

    if (!found)
        return STATUS_NOT_SUPPORTED;

    // Check SMBus is enabled
    status = pci_config_read_byte(pci_addr[0], pci_addr[1], pci_addr[2], SMBHSTCFG, pci_config);
    if (!NT_SUCCESS(status))
        return STATUS_NOT_SUPPORTED;
    if (!(pci_config & SMBHSTCFG_HST_EN))
        return STATUS_NOT_SUPPORTED;

    // Get SMBus IO base address, the last bit indicates IO mapping
    status = pci_config_read_dword(pci_addr[0], pci_addr[1], pci_addr[2], SMB_BASE, pci_config);
    if (!NT_SUCCESS(status) && !(pci_config & 0x1))
        return STATUS_NOT_SUPPORTED;

    i801_smba = pci_config & 0xffe0;
    return STATUS_SUCCESS;
}

NTSTATUS:i801_get_block_len(&len)
{
    len = io_in_byte(SMBHSTDAT0);

    if (len < 1 || len > I2C_SMBUS_BLOCK_MAX) {
        len = 0;
        debug_print(''Illegal SMBus block read size %u\n'', len);
        return STATUS_DEVICE_PROTOCOL_ERROR;
    }

    return STATUS_SUCCESS;
}

// Make sure the SMBus host is ready to start transmitting.
NTSTATUS:i801_check_pre()
{
    new hststs;

    hststs = io_in_byte(SMBHSTSTS);
    if (hststs & SMBHSTSTS_HOST_BUSY) {
        debug_print(''SMBus is busy, can't use it!\n'');
        return STATUS_DEVICE_BUSY;
    }

    hststs &= STATUS_FLAGS;
    if (hststs) {
        debug_print(''Clearing status flags (%x)\n'', hststs);
        io_out_byte(SMBHSTSTS, hststs);
    }

    return STATUS_SUCCESS;
}

i801_kill() {
    // try to stop the current command
    io_out_byte(SMBHSTCNT, SMBHSTCNT_KILL);
    microsleep(1000);
    io_out_byte(SMBHSTCNT, 0);

    // Check if it worked
    new hststs = io_in_byte(SMBHSTSTS);
    if ((hststs & SMBHSTSTS_HOST_BUSY) ||
        !(hststs & SMBHSTSTS_FAILED))
        debug_print(''Failed terminating the transaction\n'');
}

NTSTATUS:i801_hststs_to_ntstatus(hststs)
{
    new NTSTATUS:status = STATUS_SUCCESS;

    if (hststs & SMBHSTSTS_FAILED) {
        status = STATUS_IO_DEVICE_ERROR;
        debug_print(''Transaction failed\n'');
    }
    if (hststs & SMBHSTSTS_DEV_ERR) {
        status = STATUS_NO_SUCH_DEVICE;
        debug_print(''No response\n'');
    }
    if (hststs & SMBHSTSTS_BUS_ERR) {
        status = STATUS_RETRY;
        debug_print(''Lost arbitration\n'');
    }

    return status;
}

NTSTATUS:i801_wait_intr(&hststs, size)
{
    // 100 khz period in microseconds
    const clock_us = 10;

    // Don't wait more than MAX_TIMEOUT ms for the transaction to complete
    new deadline = get_tick_count() + MAX_TIMEOUT;

    // 10 start bits (start + slave address + rd/wr + ack)
    // 9 bits per byte (byte + ack)
    microsleep2((10 + (9 * size)) * clock_us);
    do {
        // Only check for result once per clock cycle
        // Also allows for 1 stop bit
        microsleep2(clock_us);
        hststs = io_in_byte(SMBHSTSTS);
        hststs &= STATUS_ERROR_FLAGS | SMBHSTSTS_INTR;
        if (!(hststs & SMBHSTSTS_HOST_BUSY) && hststs) {
            hststs &= STATUS_ERROR_FLAGS;
            return STATUS_SUCCESS;
        }
    } while (((hststs & SMBHSTSTS_HOST_BUSY) || !(hststs & (STATUS_ERROR_FLAGS | SMBHSTSTS_INTR))) && (get_tick_count() < deadline));

    if ((hststs & SMBHSTSTS_HOST_BUSY) || !(hststs & (STATUS_ERROR_FLAGS | SMBHSTSTS_INTR)))
        return STATUS_IO_TIMEOUT;

    hststs &= (STATUS_ERROR_FLAGS | SMBHSTSTS_INTR);
    
    return STATUS_SUCCESS;
}

NTSTATUS:i801_transaction(xact, &hststs, size)
{
    new old_hstcnt = io_in_byte(SMBHSTCNT);
    io_out_byte(SMBHSTCNT, old_hstcnt & ~SMBHSTCNT_INTREN);

    io_out_byte(SMBHSTCNT, xact | SMBHSTCNT_START);

    new NTSTATUS:status = i801_wait_intr(hststs, size);
    // restore previous HSTCNT, enabling interrupts if previously enabled
    io_out_byte(SMBHSTCNT, old_hstcnt);
    return status;
}

NTSTATUS:i801_block_transaction_by_block(read_write, command, in[33], out[33], &hststs)
{
    hststs = 0;
    new NTSTATUS:status, len, xact;
    // We don't know the return size, so lets just wait the minimum amount of time
    // write lacks repeated address
    new size = 2 + read_write;

    switch (command) {
    case I2C_SMBUS_BLOCK_PROC_CALL:
        xact = I801_BLOCK_PROC_CALL;
    case I2C_SMBUS_BLOCK_DATA:
        xact = I801_BLOCK_DATA;
    default:
        return STATUS_NOT_SUPPORTED;
    }

    /* Set block buffer mode */
    io_out_byte(SMBAUXCTL, io_in_byte(SMBAUXCTL) | SMBAUXCTL_E32B);

    if (read_write == I2C_SMBUS_WRITE) {
        len = in[0];
        size += len;
        io_out_byte(SMBHSTDAT0, len);
        io_in_byte(SMBHSTCNT);	/* reset the data buffer index */
        for (new i = 0; i < len; i++)
            io_out_byte(SMBBLKDAT, in[i+1]);
    }

    // size = command + count + address (read only) + data...
    status = i801_transaction(xact, hststs, size);
    if (!NT_SUCCESS(status)) {
        goto cleanup;
    }

    if (hststs) {
        status = STATUS_SUCCESS; // the transaction succeeded, but we got an error back
        goto cleanup;
    }

    if (read_write == I2C_SMBUS_READ ||
        command == I2C_SMBUS_BLOCK_PROC_CALL) {
        status = i801_get_block_len(len);
        if (!NT_SUCCESS(status)) {
            goto cleanup;
        }

        out[0] = len;
        io_in_byte(SMBHSTCNT);	/* reset the data buffer index */
        for (new i = 0; i < len; i++)
            out[i + 1] = io_in_byte(SMBBLKDAT);
    }
cleanup:
    io_out_byte(SMBAUXCTL, io_in_byte(SMBAUXCTL) & ~SMBAUXCTL_E32B);
    return status;
}

Void:i801_set_hstadd(addr, read_write)
{
    io_out_byte(SMBHSTADD, ((addr & 0x7f) << 1) | (read_write & 0x01));
}

NTSTATUS:i801_simple_transaction(addr, hstcmd, read_write, command, in, &out, &hststs)
{
    new xact, size = hstcmd;

    switch (command) {
    case I2C_SMBUS_QUICK:
        {
            i801_set_hstadd(addr, read_write);
            xact = I801_QUICK;
        }
    case I2C_SMBUS_BYTE:
        {
            i801_set_hstadd(addr, read_write);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTCMD, hstcmd);
            xact = I801_BYTE;
        }
    case I2C_SMBUS_BYTE_DATA:
        {
            i801_set_hstadd(addr, read_write);
            if (read_write == I2C_SMBUS_WRITE)
                io_out_byte(SMBHSTDAT0, in);
            io_out_byte(SMBHSTCMD, hstcmd);
            xact = I801_BYTE_DATA;
            size += read_write;
        }
    case I2C_SMBUS_WORD_DATA:
        {
            i801_set_hstadd(addr, read_write);
            if (read_write == I2C_SMBUS_WRITE) {
                io_out_byte(SMBHSTDAT0, in & 0xff);
                io_out_byte(SMBHSTDAT1, (in & 0xff00) >> 8);
            }
            io_out_byte(SMBHSTCMD, hstcmd);
            xact = I801_WORD_DATA;
            size += read_write;
        }
    case I2C_SMBUS_PROC_CALL:
        {
            i801_set_hstadd(addr, I2C_SMBUS_WRITE);
            io_out_byte(SMBHSTDAT0, in & 0xff);
            io_out_byte(SMBHSTDAT1, (in & 0xff00) >> 8);
            io_out_byte(SMBHSTCMD, hstcmd);
            read_write = I2C_SMBUS_READ;
            xact = I801_PROC_CALL;
            // This doesn't follow the trend of the other commands matching their size
            size = 6
        }
    default:
        {
            debug_print(''Unsupported transaction %d\n'', command);
            return STATUS_NOT_SUPPORTED;
        }
    }

    new NTSTATUS:status = i801_transaction(xact, hststs, size);
    if (!NT_SUCCESS(status))
        return status;
    

    if (!hststs && read_write != I2C_SMBUS_WRITE) {
        switch (command) {
        case I2C_SMBUS_BYTE, I2C_SMBUS_BYTE_DATA:
            {
                out = io_in_byte(SMBHSTDAT0);
            }
        case I2C_SMBUS_WORD_DATA, I2C_SMBUS_PROC_CALL:
            {
                out = io_in_byte(SMBHSTDAT0) | (io_in_byte(SMBHSTDAT1) << 8);
            }
        }
    }

    return STATUS_SUCCESS;
}

NTSTATUS:i801_smbus_block_transaction(addr, hstcmd, read_write, command, in[33], out[33], &hststs)
{
    if (read_write == I2C_SMBUS_READ && command == I2C_SMBUS_BLOCK_DATA)
        /* Mark block length as invalid */
        out[0] = SMBUS_LEN_SENTINEL;
    else if (in[0] < 1 || in[0] > I2C_SMBUS_BLOCK_MAX)
        return STATUS_INVALID_PARAMETER;

    if (command == I2C_SMBUS_BLOCK_PROC_CALL)
        /* Needs to be flagged as write transaction */
        i801_set_hstadd(addr, I2C_SMBUS_WRITE);
    else
        i801_set_hstadd(addr, read_write);
    io_out_byte(SMBHSTCMD, hstcmd);

    // if (priv->features & FEATURE_BLOCK_BUFFER)
    // 	return i801_block_transaction_by_block(data, read_write, command);
    // else
    // 	return i801_block_transaction_byte_by_byte(data, read_write, command);
    return i801_block_transaction_by_block(read_write, command, in, out, hststs);
}

NTSTATUS:i801_inuse(bool:inuse)
{
    if (inuse) {
        // Wait for device to be unlocked by BIOS/ACPI
        // Linux doesn't do this, since some BIOSes might not unlock it
        new deadline = get_tick_count() + MAX_TIMEOUT;
        new is_inuse = io_in_byte(SMBHSTSTS) & SMBHSTSTS_INUSE_STS;
        while (is_inuse && (get_tick_count() < deadline)) {
            microsleep(250);
            is_inuse = io_in_byte(SMBHSTSTS) & SMBHSTSTS_INUSE_STS;
        }
        
        if (is_inuse) {
            debug_print(''SMBus device is in use by BIOS/ACPI\n'');
            return STATUS_IO_TIMEOUT;
        }
        return STATUS_SUCCESS;
    } else {
        // Unlock the SMBus device for use by BIOS/ACPI, and clear status flags
        // if not done already.
        io_out_byte(SMBHSTSTS, SMBHSTSTS_INUSE_STS | STATUS_FLAGS);
        return STATUS_SUCCESS;
    }
}

NTSTATUS:i801_access_simple(addr, read_write, command, size, in, &out)
{
    new NTSTATUS:status, hststs;

    status = i801_inuse(true);
    if (!NT_SUCCESS(status))
        goto unlock;

    status = i801_check_pre();
    if (!NT_SUCCESS(status))
        goto unlock;

    io_out_byte(SMBAUXCTL, io_in_byte(SMBAUXCTL) & (~SMBAUXCTL_CRC));

    switch (size) {
        case I2C_SMBUS_QUICK, I2C_SMBUS_BYTE, I2C_SMBUS_BYTE_DATA, I2C_SMBUS_WORD_DATA, I2C_SMBUS_PROC_CALL:
            status = i801_simple_transaction(addr, command, read_write, size, in, out, hststs);
        default:
            {
                debug_print(''Unsupported simple transaction %d\n'', size);
                status = STATUS_NOT_SUPPORTED;
                goto unlock;
            }
    }


    if (!NT_SUCCESS(status)) {
        i801_kill();
        goto unlock;
    }

    status = i801_hststs_to_ntstatus(hststs);

unlock:
    i801_inuse(false);

    return status;
}

NTSTATUS:i801_access_block(addr, read_write, command, size, in[33], out[33])
{
    new NTSTATUS:status, hststs;

    status = i801_inuse(true);
    if (!NT_SUCCESS(status))
        goto unlock;

    status = i801_check_pre();
    if (!NT_SUCCESS(status))
        goto unlock;

    io_out_byte(SMBAUXCTL, io_in_byte(SMBAUXCTL) & (~SMBAUXCTL_CRC));

    switch (size) {
        case I2C_SMBUS_BLOCK_DATA, I2C_SMBUS_BLOCK_PROC_CALL:
            status = i801_smbus_block_transaction(addr, command, read_write, size, in, out, hststs);
        default:
            {
                debug_print(''Unsupported block transaction %d\n'', size);
                status = STATUS_NOT_SUPPORTED;
                goto unlock;
            }
    }

    if (!NT_SUCCESS(status)) {
        i801_kill();
        goto unlock;
    }
    
    status = i801_hststs_to_ntstatus(hststs);

unlock:
    i801_inuse(false);

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
forward NTSTATUS:ioctl_i801_write_quick(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_write_quick(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    new unused;

    return i801_access_simple(address, command, 0, I2C_SMBUS_QUICK, 0, unused);
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
forward NTSTATUS:ioctl_i801_read_byte(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_read_byte(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];

    return i801_access_simple(address, I2C_SMBUS_READ, 0, I2C_SMBUS_BYTE, 0, out[0]);
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
forward NTSTATUS:ioctl_i801_write_byte(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_write_byte(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new data = in[1];

    new unused;

    return i801_access_simple(address, I2C_SMBUS_WRITE, data, I2C_SMBUS_BYTE, 0, unused);
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
forward NTSTATUS:ioctl_i801_read_byte_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_read_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    return i801_access_simple(address, I2C_SMBUS_READ, command, I2C_SMBUS_BYTE_DATA, 0, out[0]);
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
forward NTSTATUS:ioctl_i801_write_byte_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_write_byte_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new data = in[2];

    new unused;

    return i801_access_simple(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_BYTE_DATA, data, unused);
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
forward NTSTATUS:ioctl_i801_read_word_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_read_word_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    return i801_access_simple(address, I2C_SMBUS_READ, command, I2C_SMBUS_WORD_DATA, 0, out[0]);
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
forward NTSTATUS:ioctl_i801_write_word_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_write_word_data(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new data = in[2];

    new unused;

    return i801_access_simple(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_WORD_DATA, data, unused);
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
forward NTSTATUS:ioctl_i801_read_block_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_read_block_data(in[], in_size, out[], out_size) {
    if (in_size < 2)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 5)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];

    new unused[I2C_SMBUS_BLOCK_MAX + 1];
    new out_data[I2C_SMBUS_BLOCK_MAX + 1];

    new NTSTATUS:status = i801_access_block(address, I2C_SMBUS_READ, command, I2C_SMBUS_BLOCK_DATA, unused, out_data);

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
forward NTSTATUS:ioctl_i801_write_block_data(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_write_block_data(in[], in_size, out[], out_size) {
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

    return i801_access_block(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_BLOCK_DATA, in_data, unused);
}

/// SMBus process call.
///
/// The SMBus Process Call protocol (SMBProcessCall) transfers 2 bytes of data
/// bi-directionally (performs a Write Word followed by a Read Word as an atomic
/// transaction). This protocol uses a command value to reference up to 256
/// word-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command, [2] = Data
/// @param in_size Must be 3
/// @param out [0] = Data
/// @param out_size Must be 1
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_i801_process_call(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_process_call(in[], in_size, out[], out_size) {
    if (in_size < 3)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new data = in[2];

    new unused;

    return i801_access_simple(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_PROC_CALL, data, unused);
}

/// SMBus block process call.
///
/// The SMBus Block Write-Read Block Process Call protocol (SMBBlockProcessCall)
/// transfers a block of data bi-directionally (performs a Write Block followed
/// by a Read Block as an atomic transaction). The maximum aggregate amount of
/// data that may be transferred is limited to 32 bytes. This protocol uses a
/// command value to reference up to 256 block-sized virtual registers.
///
/// @param in [0] = Address, [1] = Command, [2] = Length in bytes, [3..7] = Data (byte packed)
/// @param in_size Must be 7
/// @param out [0] = Length in bytes, [1..5] = Data (byte packed)
/// @param out_size Must be 5
/// @return An NTSTATUS
/// @warning You should acquire the "\BaseNamedObjects\Access_SMBUS.HTP.Method" mutant before calling this
forward NTSTATUS:ioctl_i801_block_process_call(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_i801_block_process_call(in[], in_size, out[], out_size) {
    if (in_size < 7)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 5)
        return STATUS_BUFFER_TOO_SMALL;

    new address = in[0];
    new command = in[1];
    new length = in[2];

    if (length > I2C_SMBUS_BLOCK_MAX || length < 0)
        return STATUS_INVALID_PARAMETER;

    new in_data[I2C_SMBUS_BLOCK_MAX + 1];
    in_data[0] = length;
    unpack_bytes_le(in, in_data, I2C_SMBUS_BLOCK_MAX, 3 * 8, 1);

    new out_data[I2C_SMBUS_BLOCK_MAX + 1];

    new NTSTATUS:status = i801_access_block(address, I2C_SMBUS_WRITE, command, I2C_SMBUS_BLOCK_PROC_CALL, in_data, out_data);

    if (!NT_SUCCESS(status))
        return status;

    out[0] = out_data[0];
    pack_bytes_le(out_data, out, I2C_SMBUS_BLOCK_MAX, 1, 8);

    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    return i801_init();
}
