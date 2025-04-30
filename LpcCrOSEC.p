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

// Many parts of this was ported from the Chromium OS EC codebase.
// See https://chromium.googlesource.com/chromiumos/platform/ec/

/* I/O addresses for host command */
#define EC_LPC_ADDR_HOST_DATA 0x200
#define EC_LPC_ADDR_HOST_CMD 0x204

/* I/O addresses for host command args and params */
/* Protocol version 3 */
#define EC_LPC_ADDR_HOST_PACKET 0x800 /* Offset of version 3 packet */
#define EC_LPC_HOST_PACKET_SIZE 0x100 /* Max size of version 3 packet */


// 0x900 is the default, 0xE00 is used by AMD Framework Laptops
#define EC_LPC_ADDR_MEMMAP 0x900
#define EC_LPC_ADDR_MEMMAP_FWAMD 0xE00
#define EC_MEMMAP_SIZE 255 /* ACPI IO buffer max is 255 bytes */
#define EC_MEMMAP_TEXT_MAX 8 /* Size of a string in the memory map */

/* LPC command status byte masks */
/* EC has written a byte in the data register and host hasn't read it yet */
#define EC_LPC_STATUS_TO_HOST 0x01
/* Host has written a command/data byte and the EC hasn't read it yet */
#define EC_LPC_STATUS_FROM_HOST 0x02
/* EC is processing a command */
#define EC_LPC_STATUS_PROCESSING 0x04
/* Last write to EC was a command, not data */
#define EC_LPC_STATUS_LAST_CMD 0x08
/* EC is in burst mode */
#define EC_LPC_STATUS_BURST_MODE 0x10
/* SCI event is pending (requesting SCI query) */
#define EC_LPC_STATUS_SCI_PENDING 0x20
/* SMI event is pending (requesting SMI query) */
#define EC_LPC_STATUS_SMI_PENDING 0x40
/* (reserved) */
#define EC_LPC_STATUS_RESERVED 0x80

/*
 * EC is busy.  This covers both the EC processing a command, and the host has
 * written a new command but the EC hasn't picked it up yet.
 */
#define EC_LPC_STATUS_BUSY_MASK \
    (EC_LPC_STATUS_FROM_HOST | EC_LPC_STATUS_PROCESSING)

/*
 * Value written to legacy command port / prefix byte to indicate protocol
 * 3+ structs are being used.  Usage is bus-dependent.
 */
#define EC_COMMAND_PROTOCOL_3 0xda

#define EC_HOST_REQUEST_VERSION 3

#define EC_HOST_RESPONSE_VERSION 3


#define EC_MEMMAP_ID 0x20 /* 0x20 == 'E', 0x21 == 'C' */

#define EC_MEMMAP_HOST_CMD_FLAGS 0x27 /* Host cmd interface flags (8 bits) */

/* Host command interface supports version 3 protocol */
#define EC_HOST_CMD_FLAG_VERSION_3 0x02

#define INITIAL_UDELAY 5 /* 5 us */
#define MAXIMUM_UDELAY 10000 /* 10 ms */


new memmap_addr;

new ec_command_proto;


NTSTATUS:wait_for_ec(const status_addr, const timeout_usec) {
	new delay = INITIAL_UDELAY;

	for (new i = 0; i < timeout_usec; i += delay) {
		if (!(io_in_byte(status_addr) & EC_LPC_STATUS_BUSY_MASK))
			return STATUS_SUCCESS;

		microsleep(_min(delay, timeout_usec - i));

		/* Increase the delay interval after a few rapid checks */
		if (i > 20)
			delay = _min(delay * 2, MAXIMUM_UDELAY);
	}
	return STATUS_TIMEOUT; /* Timeout */
}

NTSTATUS:ec_command_lpc_3(command, version, outdata[], outsize, outoffset, indata[], insize) {
    new csum = 0;
    const rq_size = 1 + 1 + 2 + 1 + 1 + 2;
    const rs_size = 1 + 1 + 2 + 2 + 2;

    /* Fail if we're going to overrun the EC */
    if (outsize + rq_size > EC_LPC_HOST_PACKET_SIZE ||
        insize + rs_size > EC_LPC_HOST_PACKET_SIZE)
        return STATUS_BUFFER_TOO_SMALL;

    // struct_version, checksum, command (16bit), command_version, reserved, data_len (16bit)
    new request[8] = [];
    request[0] = EC_HOST_REQUEST_VERSION;
    request[1] = csum;
    request[2] = command & 0xff;
    request[3] = command >> 8;
    request[4] = version;
    request[5] = 0;
    request[6] = outsize & 0xff;
    request[7] = outsize >> 8;

    /* Copy data and start checksum */
    for (new i = 0; i < outsize; i++) {
        io_out_byte(EC_LPC_ADDR_HOST_PACKET + rq_size + i, outdata[i + outoffset]);
        csum += outdata[i + outoffset] & 0xff;
    }

    /* Finish checksum */
    for (new i = 0; i < rq_size; i++)
        csum += request[i] & 0xff;

    /* Write checksum field so the entire packet sums to 0 */
    request[1] = (-csum) & 0xff;

    /* Copy header */
    for (new i = 0; i < rq_size; i++)
        io_out_byte(EC_LPC_ADDR_HOST_PACKET + i, request[i]);

    /* Start the command */
    io_out_byte(EC_LPC_ADDR_HOST_CMD, EC_COMMAND_PROTOCOL_3);

    if (wait_for_ec(EC_LPC_ADDR_HOST_CMD, 1000000)) {
        debug_print(''Timeout waiting for EC response\n'');
        return STATUS_TIMEOUT;
    }

    /* Check result */
    new res = io_in_byte(EC_LPC_ADDR_HOST_DATA);
    /* First cell is negative for error */
    indata[0] = -res;
    if (res) {
        debug_print(''EC returned error result code %d\n'', res);
        /*  I would like to return STATUS_IO_DEVICE_ERROR here,
            but I still want to transfer the EC's error code. */
        return STATUS_SUCCESS;
    }

    /* Read back response header and start checksum */
    csum = 0;
    // struct_version, checksum, result, data_len, reserved
    new data_out[rs_size];
    for (new i = 0; i < rs_size; i++) {
        data_out[i] = io_in_byte(EC_LPC_ADDR_HOST_PACKET + i);
        csum += data_out[i];
    }

    if (data_out[0] != EC_HOST_RESPONSE_VERSION) {
        debug_print(''EC response version mismatch\n'');
        return STATUS_DEVICE_PROTOCOL_ERROR;
    }

    if (data_out[6] || data_out[7]) {
        debug_print(''EC response reserved != 0\n'');
        return STATUS_DEVICE_PROTOCOL_ERROR;
    }

    new data_len = data_out[4] | data_out[5] << 8;
    /* First cell is positive for EC length */
    indata[0] = data_len;
    if (data_len > insize) {
        debug_print(''EC returned too much data\n'');
        return STATUS_BUFFER_TOO_SMALL;
    }

    /* Read back data and update checksum */
    for (new i = 0; i < data_len; i++) {
        indata[i + 1] = io_in_byte(EC_LPC_ADDR_HOST_PACKET + rs_size + i);
        csum += indata[i + 1];
    }

    /* Verify checksum */
    if (csum & 0xff) {
        debug_print(''EC response has invalid checksum\n'');
        return STATUS_CRC_ERROR;
    }

    return STATUS_SUCCESS;
}

NTSTATUS:ec_readmem_lpc(offset, bytes, dest[]) {
    if ((offset + bytes) >= EC_MEMMAP_SIZE)
        return STATUS_INVALID_PARAMETER;

    if (bytes <= EC_MEMMAP_SIZE) { /* fixed length */
        for (new i = 0; i < bytes; i++)
            dest[i] = io_in_byte(memmap_addr + offset + i);
    } else { /* string */
        for (new i = 0; i < EC_MEMMAP_SIZE; i++) {
            dest[i] = io_in_byte(memmap_addr + offset + i);
            if (!dest[i])
                break;
        }
    }

    return STATUS_SUCCESS;
}

NTSTATUS:comm_init_lpc() {
    new byte = 0xff;

    /*
     * Test if the I/O port has been configured for Chromium EC LPC
     * interface.  Chromium EC guarantees that at least one status bit will
     * be 0, so if the command and data bytes are both 0xff, very likely
     * that Chromium EC is not present.  See crosbug.com/p/10963.
     */
    byte &= io_in_byte(EC_LPC_ADDR_HOST_CMD);
    byte &= io_in_byte(EC_LPC_ADDR_HOST_DATA);
    if (byte == 0xff) {
        return STATUS_NOT_SUPPORTED;
    }

    /*
     * Test if LPC command args are supported.
     *
     * The cheapest way to do this is by looking for the memory-mapped
     * flag.  This is faster than sending a new-style 'hello' command and
     * seeing whether the EC sets the EC_HOST_ARGS_FLAG_FROM_HOST flag
     * in args when it responds.
     */
    if (io_in_byte(EC_LPC_ADDR_MEMMAP + EC_MEMMAP_ID) == 'E' &&
        io_in_byte(EC_LPC_ADDR_MEMMAP + EC_MEMMAP_ID + 1) == 'C') {
        memmap_addr = EC_LPC_ADDR_MEMMAP;
    }
    if (io_in_byte(EC_LPC_ADDR_MEMMAP_FWAMD + EC_MEMMAP_ID) == 'E' &&
        io_in_byte(EC_LPC_ADDR_MEMMAP_FWAMD + EC_MEMMAP_ID + 1) == 'C') {
        memmap_addr = EC_LPC_ADDR_MEMMAP_FWAMD;
    }
    if (!memmap_addr) {
        return STATUS_NOT_SUPPORTED;
    }

    /* Check which command version we'll use */
    new version = io_in_byte(memmap_addr + EC_MEMMAP_HOST_CMD_FLAGS);

    if (version & EC_HOST_CMD_FLAG_VERSION_3) {
        /* Protocol version 3 */
        ec_command_proto = 3;
    } else {
        // TODO: Implement protocol version 2
        // EC doesn't support protocols we need.
        return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

forward NTSTATUS:ioctl_ec_command(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_ec_command(in[], in_size, out[], out_size) {
    if (in_size < 3 || out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    if (ec_command_proto == 3) {
        new command = (in[0] & 0xff) | (in[1] << 8);
        new version = in[2];
        new in_size_offset = 3;
        new ec_in_size = in_size - in_size_offset;
        new out_size_offset = 1;
        new ec_out_size = out_size - out_size_offset;

        return ec_command_lpc_3(command, version, in, ec_in_size, in_size_offset, out, ec_out_size);
    }

    return STATUS_NOT_SUPPORTED;
}

forward NTSTATUS:ioctl_ec_readmem(in[], in_size, out[], out_size);
public NTSTATUS:ioctl_ec_readmem(in[], in_size, out[], out_size) {
    if (in_size < 1)
        return STATUS_BUFFER_TOO_SMALL;
    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    new offset = in[0] & 0xFFFF;

    return ec_readmem_lpc(offset, out_size, out);
}

NTSTATUS:main() {
    return comm_init_lpc();
}
