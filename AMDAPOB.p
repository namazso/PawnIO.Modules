//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  Ivan Rusanov (contact@zentimings.com)
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

// Read-only access to the AGESA PSP Output Block (APOB).
//
// The PSP leaves the APOB in reserved DRAM at one of a few fixed physical addresses. It starts with a
// header:
//   0x00  Signature   "APOB"
//   0x04  Version
//   0x08  Table size  Size of the whole table in bytes, header included
//   0x0C  Header size Also the offset of the first entry
//
// The entries follow the header, each one starting with:
//   0x00  Group
//   0x04  Type
//   0x08  Instance
//   0x0C  Entry size  Size of the entry in bytes, entry header included
//
// The module takes no address from the caller: it looks for the table at the known addresses only, and
// reads are limited to the table found there.
//
// The actual layout implementation and data interpretation is left to the consumer app

#define APOB_SIGNATURE          (0x424F5041) // "APOB"

// Offsets
#define APOB_HDR_SIGNATURE      (0x00)
#define APOB_HDR_VERSION        (0x04)
#define APOB_HDR_TABLE_SIZE     (0x08)
#define APOB_HDR_HEADER_SIZE    (0x0C)
#define APOB_HDR_MIN_SIZE       (0x10)

#define APOB_ENTRY_SIZE         (0x0C)
#define APOB_ENTRY_MIN_SIZE     (0x10)

// Sanity limit for the table size read from the header, just in case.
#define APOB_MAX_TABLE_SIZE     (0x100000) // 1MiB

// Physical addresses the APOB is found at, in the order they are probed.
//
// The address cannot be queried at runtime. It is the destination of the APOB entry (type 0x61) in the BIOS
// directory table of the firmware image, a constant of the firmware build, and these are the known values.
new const k_apob_addresses[] = [
    // AM4 and AM5 desktop firmware
    0x0A200000,
    // Mobile firmware (Rembrandt, Krackan Point, Strix Point)
    0x09F00000,
    // Found in some older AGESA, probably earlier default
    0x04000000
];

new g_apob_addr = 0;
new g_apob_version = 0;
new g_apob_table_size = 0;
new g_apob_header_size = 0;

/// Read and validate the APOB header, and the header of the first entry.
///
/// @param va Mapping of the first page of the table
/// @param version Version
/// @param table_size Table size in bytes
/// @param header_size Header size in bytes
/// @return Whether the page starts with a valid header
bool:read_header(VA:va, &version, &table_size, &header_size) {
    new signature = 0;
    if (!NT_SUCCESS(virtual_read_dword(va + APOB_HDR_SIGNATURE, signature))
        || (signature & 0xFFFFFFFF) != APOB_SIGNATURE)
        return false;

    if (!NT_SUCCESS(virtual_read_dword(va + APOB_HDR_VERSION, version))
        || !NT_SUCCESS(virtual_read_dword(va + APOB_HDR_TABLE_SIZE, table_size))
        || !NT_SUCCESS(virtual_read_dword(va + APOB_HDR_HEADER_SIZE, header_size)))
        return false;

    version &= 0xFFFFFFFF;
    table_size &= 0xFFFFFFFF;
    header_size &= 0xFFFFFFFF;

    // The header and the header of the first entry are within the mapped page
    if (header_size < APOB_HDR_MIN_SIZE || header_size > PAGE_SIZE - APOB_ENTRY_MIN_SIZE || (header_size & 0x3))
        return false;

    if (table_size < header_size + APOB_ENTRY_MIN_SIZE || table_size > APOB_MAX_TABLE_SIZE)
        return false;

    // The first entry starts right after the header and has to fit in the table
    new entry_size = 0;
    if (!NT_SUCCESS(virtual_read_dword(va + header_size + APOB_ENTRY_SIZE, entry_size)))
        return false;

    entry_size &= 0xFFFFFFFF;

    return entry_size >= APOB_ENTRY_MIN_SIZE && entry_size <= table_size - header_size;
}

/// Check for an APOB header at a physical address.
///
/// @param addr Page aligned physical address
/// @return Whether a valid header was found; the header fields are stored in the globals
bool:probe(addr) {
    new VA:va = io_space_map(addr, PAGE_SIZE);
    if (va == NULL)
        return false;

    new version = 0;
    new table_size = 0;
    new header_size = 0;
    new bool:ok = read_header(va, version, table_size, header_size);

    io_space_unmap(va, PAGE_SIZE);

    if (!ok)
        return false;

    g_apob_addr = addr;
    g_apob_version = version;
    g_apob_table_size = table_size;
    g_apob_header_size = header_size;
    return true;
}

/// Get the APOB location and header.
/// Header size is also the offset of the first entry in the table.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Physical address, [1] = Version, [2] = Table size in bytes, [3] = Header size in bytes
/// @param out_size Must be 4
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_apob_info, 0, 4) {
    if (!g_apob_addr || !g_apob_table_size)
        return STATUS_DEVICE_NOT_READY;

    out[0] = g_apob_addr;
    out[1] = g_apob_version;
    out[2] = g_apob_table_size;
    out[3] = g_apob_header_size;

    return STATUS_SUCCESS;
}

/// Read the APOB table. The pages holding the requested range are mapped for the call only: the table does
/// not change after boot and is read once, so nothing stays mapped between calls.
///
/// @param in [0] = Byte offset into the table, a multiple of 8 (optional, defaults to 0)
/// @param in_size 0 or 1
/// @param out Table contents, 8 bytes per cell (little endian); bytes past the end of the table read as 0
/// @param out_size Number of cells to read; reading stops at the end of the table
/// @return An NTSTATUS
DEFINE_IOCTL(ioctl_read_apob) {
    if (in_size > 1)
        return STATUS_INVALID_PARAMETER;

    if (out_size < 1)
        return STATUS_BUFFER_TOO_SMALL;

    if (!g_apob_addr || !g_apob_table_size)
        return STATUS_DEVICE_NOT_READY;

    new offset = in_size == 1 ? in[0] : 0;
    if (offset < 0 || offset >= g_apob_table_size || (offset & 0x7))
        return STATUS_INVALID_PARAMETER;

    new remaining = g_apob_table_size - offset;
    new count = min(out_size, div_ceil(remaining, 8));

    // Whole pages covering [offset, offset + count * 8), clipped to the table
    new map_start = offset & ~(PAGE_SIZE - 1);
    new map_end = (min(offset + count * 8, g_apob_table_size) + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
    new map_size = map_end - map_start;

    new VA:va = io_space_map(g_apob_addr + map_start, map_size);
    if (va == NULL)
        return STATUS_COMMITMENT_LIMIT;

    new NTSTATUS:status = STATUS_SUCCESS;

    // The table is static after boot; when the header is in range, make sure it is still what main() found.
    if (map_start == 0) {
        new version = 0;
        new table_size = 0;
        new header_size = 0;
        if (!read_header(va, version, table_size, header_size)
            || table_size != g_apob_table_size || header_size != g_apob_header_size)
            status = STATUS_DEVICE_NOT_READY;
    }

    new base = offset - map_start;
    new value;
    for (new i = 0; i < count && NT_SUCCESS(status); ++i) {
        status = virtual_read_qword(va + base + i * 8, value);
        if (!NT_SUCCESS(status))
            break;

        // Last, partial cell: clear the bytes past the end of the table.
        new left = remaining - i * 8;
        if (left < 8)
            value &= (1 << (left * 8)) - 1;

        out[i] = value;
    }

    io_space_unmap(va, map_size);
    return status;
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    if (get_cpu_vendor() != CpuVendor_AMD)
        return STATUS_NOT_SUPPORTED;

    new fms = get_cpu_fms();

    new family = cpu_fms_family(fms);
    new model = cpu_fms_model(fms);

    debug_print(''AMDAPOB: family: %x model: %x\n'', family, model);

    if (family < 0x17 || family > 0x1A)
        return STATUS_NOT_SUPPORTED;

    for (new i = 0; i < sizeof k_apob_addresses; ++i) {
        if (probe(k_apob_addresses[i])) {
            debug_print(''AMDAPOB: found at %x, version %x, size %x, header size %x\n'',
                g_apob_addr, g_apob_version, g_apob_table_size, g_apob_header_size);
            return STATUS_SUCCESS;
        }
    }

    debug_print(''AMDAPOB: not found\n'');
    return STATUS_NOT_FOUND;
}
