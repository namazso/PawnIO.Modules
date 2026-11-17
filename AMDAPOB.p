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
// The PSP leaves the APOB in reserved DRAM, at an address chosen per platform and firmware build. It
// starts with a header:
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
// The module takes no address from the caller: it takes the address from the firmware image, or failing
// that looks at the known addresses, and reads are limited to the table found there.
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

// TOP_MEM: below TOM[47:23] is DRAM, from there to 4GiB is MMIO
#define MSR_TOP_MEM             (0xC001001A)
#define TOP_MEM_ADDR_MASK       (0x0000FFFFFF800000)
// Limit used when TOP_MEM cannot be read
#define TOP_MEM_FALLBACK        (0x10000000) // 256MiB

// The address of the APOB is a constant of the firmware build, recorded in the firmware image: it is the
// destination of the APOB entry (type 0x61) of the BIOS directory table. The directory tables are reached from
// the Embedded Firmware Structure (EFS), which is at one of a few fixed offsets of the SPI ROM:
//
//   EFS -> PSP directory ($PSP) -> [image slot header ->] level 2 ($PL2) -> BIOS directory ($BL2)
//   EFS -> BIOS directory ($BHD) -> level 2 ($BL2)
//   EFS -> combo directory (2PSP, 2BHD) -> a directory per supported processor
//
// The structures are the ones of coreboot's amdfwtool (util/amdfwtool, src/vendorcode/amd/psp). Every table
// is checked for its cookie.
//
// The SPI ROM is memory mapped by the FCH in two windows (PPR, "Address Space Mapping", LPC/SPI ROM_2 and
// ROM_3):
//   ROM2  FF00_0000h, 16MiB  Below 4GiB. Shows the whole ROM when it is 16MiB (AM4, most laptops), but not
//                            the part with the EFS on boards with a larger ROM (AM5).
//   ROM3  FD_0000_0000h, 64MiB  Above 4GiB. Shows the ROM from its start.
// ROM2 is walked first, ROM3 only when ROM2 gives no address. A ROM holds one or more 16MiB images, and the
// pointers of an image are relative to its start, or x86 addresses of the image mapped at the top of 4GiB.
#define ROM2_BASE               (0xFF000000)
#define ROM2_SIZE               (0x01000000) // 16MiB
#define ROM3_BASE               (0xFD00000000)
#define ROM3_SIZE               (0x04000000) // 64MiB
#define ROM_IMAGE_SIZE          (0x01000000) // 16MiB
#define ROM_X86_BASE            (0xFF000000)

#define EFS_SIGNATURE           (0x55AA55AA)

// Offsets of an image the EFS is found at
new const k_efs_offsets[] = [0xFA0000, 0xF20000, 0xE20000, 0xC20000, 0x820000, 0x120000, 0x020000];
// Offsets of the directory pointers in the EFS
new const k_efs_dir_fields[] = [0x10, 0x14, 0x18, 0x1C, 0x20, 0x28, 0x2C];

#define DIR_COOKIE_PSP          (0x50535024) // "$PSP"
#define DIR_COOKIE_PSP_L2       (0x324C5024) // "$PL2"
#define DIR_COOKIE_BIOS         (0x44484224) // "$BHD"
#define DIR_COOKIE_BIOS_L2      (0x324C4224) // "$BL2"
#define DIR_COOKIE_PSP_COMBO    (0x50535032) // "2PSP"
#define DIR_COOKIE_BIOS_COMBO   (0x44484232) // "2BHD"

// Directory header: cookie, checksum, number of entries, additional info
#define DIR_HDR_NUM_ENTRIES     (0x08)
#define DIR_HDR_INFO            (0x0C)
#define DIR_HDR_SIZE            (0x10)
#define COMBO_HDR_SIZE          (0x20)

// Directory entries. The address is 62 bits wide, with the address mode in the 2 bits above it.
#define DIR_ENTRY_TYPE          (0x00)
#define DIR_ENTRY_ADDR          (0x08)
#define DIR_ENTRY_ADDR_HI       (0x0C)
#define PSP_ENTRY_SIZE          (0x10)
#define BIOS_ENTRY_DEST         (0x10)
#define BIOS_ENTRY_DEST_HI      (0x14)
#define BIOS_ENTRY_SIZE         (0x18)
#define COMBO_ENTRY_ADDR        (0x08)
#define COMBO_ENTRY_SIZE        (0x10)

#define PSP_TYPE_L2_DIR         (0x40)
#define PSP_TYPE_RECOVERY_A     (0x48)
#define PSP_TYPE_BIOS_DIR       (0x49)
#define PSP_TYPE_RECOVERY_B     (0x4A)
#define BIOS_TYPE_APOB          (0x61)
#define BIOS_TYPE_L2_DIR        (0x70)

// Address modes: 0 = physical address, 1 = offset in the ROM, 2 = offset from the directory,
// 3 = offset from the image slot
#define ADDR_MODE_REL_ROM       (1)
#define ADDR_MODE_REL_DIR       (2)

// Location of the level 2 PSP directory in an image slot header, and in its older layout
#define ISH_PL2_LOCATION        (0x10)
#define ISH_PL2_LOCATION_OLD    (0x0C)

// Limits for the walk
#define MAX_DIRS                (32)
#define MAX_DIR_ENTRIES         (256)
#define MAX_COMBO_ENTRIES       (32)
#define MAX_FW_ADDRESSES        (4)

// Physical addresses the APOB is found at, in the order they are probed. These are the known values of the
// address, used when it cannot be taken from the firmware image. The address is a platform setting of the
// firmware build (openSIL: PLAT_APOB_ADDRESS in Kconfig.Plat, overridden by the reference board configs).
new const k_apob_addresses[] = [
    // AM4 and AM5 desktop firmware
    0x0A200000,
    // Mobile and Phoenix firmware (Rembrandt, Phoenix, Krackan Point, Strix Point); openSIL Phoenix Mayan CRB
    0x09F00000,
    // openSIL Genoa Onyx CRB
    0x04000000,
    // openSIL Turin Purico CRB
    0x76B00000,
    // openSIL default (Kconfig.Plat)
    0x75BA0000
];

new g_apob_addr = 0;
new g_apob_version = 0;
new g_apob_table_size = 0;
new g_apob_header_size = 0;

// How the table was located, for ioctl_get_apob_debug
#define APOB_SOURCE_FIRMWARE    (1)
#define APOB_SOURCE_KNOWN       (2)

new g_apob_source = 0;
new g_fw_addr_count = 0;
new g_fw_efs_addr = 0;

// Size of the ROM window that is being walked
new g_rom_size = 0;

/// Get the end of DRAM below 4GiB.
///
/// @return TOP_MEM, or a fallback limit when it cannot be read
get_top_mem() {
    new top_mem = 0;
    if (!NT_SUCCESS(msr_read(MSR_TOP_MEM, top_mem)))
        return TOP_MEM_FALLBACK;

    top_mem &= TOP_MEM_ADDR_MASK;
    return top_mem ? top_mem : TOP_MEM_FALLBACK;
}

/// Read a dword of the memory mapped SPI ROM.
///
/// @param rom Mapping of the ROM
/// @param offset Offset in the ROM
/// @param value Value read
/// @return Whether the read is in range, aligned and succeeded
bool:rom_read(VA:rom, offset, &value) {
    if (offset < 0 || offset > g_rom_size - 4 || (offset & 0x3))
        return false;

    if (!NT_SUCCESS(virtual_read_dword(rom + offset, value)))
        return false;

    value &= 0xFFFFFFFF;
    return true;
}

/// Convert a pointer of a firmware image to an offset in the mapped ROM. Depending on the image it is
/// an x86 address of the image or an offset from its start.
///
/// @param image Offset of the image in the ROM
/// @param addr Pointer, 32 bits
/// @return Offset in the ROM, -1 when it is empty or not in the mapped part
rom_offset(image, addr) {
    if (addr == 0 || addr == 0xFFFFFFFF)
        return -1;

    new offset = image + (addr >= ROM_X86_BASE ? addr - ROM_X86_BASE : addr);
    return offset < g_rom_size ? offset : -1;
}

/// Convert the address of a directory entry to an offset in the mapped ROM.
///
/// @param image Offset of the image in the ROM
/// @param dir_offset Offset of the directory the entry is in
/// @param addr Address of the entry, lower 32 bits
/// @param mode Address mode
/// @return Offset in the ROM, -1 when it is empty or not in the mapped part
entry_offset(image, dir_offset, addr, mode) {
    if (mode < ADDR_MODE_REL_DIR)
        return rom_offset(image, addr);

    new offset = dir_offset + addr;
    return offset < g_rom_size ? offset : -1;
}

/// Get the address mode of a directory from the additional info of its header.
///
/// @param info Additional info
/// @return Address mode
dir_addr_mode(info) {
    // Bit 31 is the version of the field
    return (info & BIT(31)) ? (info >>> 24) & 0x3 : (info >>> 29) & 0x3;
}

/// Add a directory to the list of directories to walk.
///
/// @param dirs Offsets of the directories
/// @param count Number of directories
/// @param offset Offset of the directory, ignored when negative, already listed or the list is full
Void:add_dir(dirs[MAX_DIRS], &count, offset) {
    if (offset < 0 || count >= MAX_DIRS)
        return;

    for (new i = 0; i < count; ++i) {
        if (dirs[i] == offset)
            return;
    }

    dirs[count++] = offset;
}

/// Add the directories a combo directory points to.
///
/// @param rom Mapping of the ROM
/// @param image Offset of the image in the ROM
/// @param offset Offset of the combo directory
/// @param dirs Offsets of the directories
/// @param count Number of directories
Void:walk_combo_dir(VA:rom, image, offset, dirs[MAX_DIRS], &count) {
    new num_entries = 0;
    if (!rom_read(rom, offset + DIR_HDR_NUM_ENTRIES, num_entries))
        return;

    num_entries = min(num_entries, MAX_COMBO_ENTRIES);

    for (new i = 0; i < num_entries; ++i) {
        new addr = 0;
        if (!rom_read(rom, offset + COMBO_HDR_SIZE + i * COMBO_ENTRY_SIZE + COMBO_ENTRY_ADDR, addr))
            return;

        add_dir(dirs, count, rom_offset(image, addr));
    }
}

/// Add the directories a PSP directory points to: its level 2, the level 2 of the recovery A/B image slots
/// and the BIOS directory.
///
/// @param rom Mapping of the ROM
/// @param image Offset of the image in the ROM
/// @param offset Offset of the PSP directory
/// @param dirs Offsets of the directories
/// @param count Number of directories
Void:walk_psp_dir(VA:rom, image, offset, dirs[MAX_DIRS], &count) {
    new num_entries = 0;
    new info = 0;
    if (!rom_read(rom, offset + DIR_HDR_NUM_ENTRIES, num_entries) || !rom_read(rom, offset + DIR_HDR_INFO, info))
        return;

    num_entries = min(num_entries, MAX_DIR_ENTRIES);
    new dir_mode = dir_addr_mode(info);

    for (new i = 0; i < num_entries; ++i) {
        new entry = offset + DIR_HDR_SIZE + i * PSP_ENTRY_SIZE;
        new type = 0;
        if (!rom_read(rom, entry + DIR_ENTRY_TYPE, type))
            return;

        type &= 0xFF;
        if (type != PSP_TYPE_L2_DIR && type != PSP_TYPE_BIOS_DIR
            && type != PSP_TYPE_RECOVERY_A && type != PSP_TYPE_RECOVERY_B)
            continue;

        new addr = 0;
        new addr_hi = 0;
        if (!rom_read(rom, entry + DIR_ENTRY_ADDR, addr) || !rom_read(rom, entry + DIR_ENTRY_ADDR_HI, addr_hi))
            return;

        // The mode of the entry only applies when the directory is not in an absolute mode
        new mode = dir_mode < ADDR_MODE_REL_DIR ? dir_mode : addr_hi >>> 30;

        if (type == PSP_TYPE_L2_DIR || type == PSP_TYPE_BIOS_DIR) {
            add_dir(dirs, count, entry_offset(image, offset, addr, mode));
            continue;
        }

        // Recovery A/B: the entry is never relative to the directory. It points to an image slot header
        // holding the location of the level 2 directory or, without slot headers, to the directory itself.
        new target = entry_offset(image, offset, addr, mode < ADDR_MODE_REL_DIR ? mode : ADDR_MODE_REL_ROM);
        new cookie = 0;
        if (!rom_read(rom, target, cookie))
            continue;

        if (cookie == DIR_COOKIE_PSP_L2) {
            add_dir(dirs, count, target);
            continue;
        }

        new const k_locations[] = [ISH_PL2_LOCATION, ISH_PL2_LOCATION_OLD];
        for (new j = 0; j < sizeof k_locations; ++j) {
            new location = 0;
            if (!rom_read(rom, target + k_locations[j], location))
                break;

            new l2_offset = rom_offset(image, location);
            if (rom_read(rom, l2_offset, cookie) && cookie == DIR_COOKIE_PSP_L2) {
                add_dir(dirs, count, l2_offset);
                break;
            }
        }
    }
}

/// Collect the destination of the APOB entries of a BIOS directory, and add its level 2 directory.
///
/// @param rom Mapping of the ROM
/// @param image Offset of the image in the ROM
/// @param offset Offset of the BIOS directory
/// @param dirs Offsets of the directories
/// @param count Number of directories
/// @param addrs APOB addresses
/// @param addr_count Number of APOB addresses
Void:walk_bios_dir(VA:rom, image, offset, dirs[MAX_DIRS], &count, addrs[MAX_FW_ADDRESSES], &addr_count) {
    new num_entries = 0;
    new info = 0;
    if (!rom_read(rom, offset + DIR_HDR_NUM_ENTRIES, num_entries) || !rom_read(rom, offset + DIR_HDR_INFO, info))
        return;

    num_entries = min(num_entries, MAX_DIR_ENTRIES);
    new dir_mode = dir_addr_mode(info);

    for (new i = 0; i < num_entries; ++i) {
        new entry = offset + DIR_HDR_SIZE + i * BIOS_ENTRY_SIZE;
        new type = 0;
        if (!rom_read(rom, entry + DIR_ENTRY_TYPE, type))
            return;

        type &= 0xFF;

        if (type == BIOS_TYPE_L2_DIR) {
            new addr = 0;
            new addr_hi = 0;
            if (!rom_read(rom, entry + DIR_ENTRY_ADDR, addr) || !rom_read(rom, entry + DIR_ENTRY_ADDR_HI, addr_hi))
                return;

            new mode = dir_mode < ADDR_MODE_REL_DIR ? dir_mode : addr_hi >>> 30;
            add_dir(dirs, count, entry_offset(image, offset, addr, mode));
        } else if (type == BIOS_TYPE_APOB) {
            new dest = 0;
            new dest_hi = 0;
            if (!rom_read(rom, entry + BIOS_ENTRY_DEST, dest) || !rom_read(rom, entry + BIOS_ENTRY_DEST_HI, dest_hi))
                return;

            // A page aligned address below 4GiB
            if (dest_hi != 0 || dest == 0 || (dest & (PAGE_SIZE - 1)) || addr_count >= MAX_FW_ADDRESSES)
                continue;

            new bool:listed = false;
            for (new j = 0; j < addr_count; ++j) {
                if (addrs[j] == dest)
                    listed = true;
            }

            if (!listed)
                addrs[addr_count++] = dest;
        }
    }
}

/// Collect the APOB addresses recorded in the images of a ROM window.
///
/// @param base Physical address of the window
/// @param size Size of the window, a multiple of the image size
/// @param addrs APOB addresses
/// @param addr_count Number of APOB addresses
Void:walk_rom_window(base, size, addrs[MAX_FW_ADDRESSES], &addr_count) {
    new VA:rom = io_space_map(base, size);
    if (rom == NULL)
        return;

    g_rom_size = size;

    for (new image = 0; image + ROM_IMAGE_SIZE <= size; image += ROM_IMAGE_SIZE) {
        new dirs[MAX_DIRS];
        new count = 0;

        for (new i = 0; i < sizeof k_efs_offsets; ++i) {
            new efs = image + k_efs_offsets[i];
            new signature = 0;
            if (!rom_read(rom, efs, signature) || signature != EFS_SIGNATURE)
                continue;

            debug_print(''AMDAPOB: EFS at %x\n'', base + efs);
            g_fw_efs_addr = base + efs;

            for (new j = 0; j < sizeof k_efs_dir_fields; ++j) {
                new addr = 0;
                if (rom_read(rom, efs + k_efs_dir_fields[j], addr))
                    add_dir(dirs, count, rom_offset(image, addr));
            }
        }

        // The list grows while it is walked. Each directory is listed once, so the walk ends.
        for (new i = 0; i < count; ++i) {
            new cookie = 0;
            if (!rom_read(rom, dirs[i], cookie))
                continue;

            switch (cookie) {
                case DIR_COOKIE_PSP_COMBO, DIR_COOKIE_BIOS_COMBO:
                    walk_combo_dir(rom, image, dirs[i], dirs, count);
                case DIR_COOKIE_PSP, DIR_COOKIE_PSP_L2:
                    walk_psp_dir(rom, image, dirs[i], dirs, count);
                case DIR_COOKIE_BIOS, DIR_COOKIE_BIOS_L2:
                    walk_bios_dir(rom, image, dirs[i], dirs, count, addrs, addr_count);
            }
        }
    }

    io_space_unmap(rom, size);
}

/// Get the APOB addresses recorded in the firmware image. There is one per BIOS directory, so more than one
/// when the image holds directories for several processors that do not agree on it.
///
/// @param addrs APOB addresses
/// @return Number of APOB addresses
find_firmware_addresses(addrs[MAX_FW_ADDRESSES]) {
    new addr_count = 0;

    walk_rom_window(ROM2_BASE, ROM2_SIZE, addrs, addr_count);

    if (addr_count == 0)
        walk_rom_window(ROM3_BASE, ROM3_SIZE, addrs, addr_count);

    g_fw_addr_count = addr_count;
    return addr_count;
}

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
/// @param top_mem End of DRAM below 4GiB
/// @return Whether a valid header was found; the header fields are stored in the globals
bool:probe(addr, top_mem) {
    // Only DRAM is probed
    if (addr + PAGE_SIZE > top_mem)
        return false;

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

    // The whole table has to be in DRAM
    if (addr + table_size > top_mem)
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

/// Get how the APOB was located. For diagnostics only.
///
/// @param in Unused
/// @param in_size Unused
/// @param out [0] = Source of the address: 1 = the firmware image, 2 = one of the known addresses
///            [1] = Number of APOB addresses found in the firmware image
///            [2] = Physical address of the last EFS found, 0 when none
/// @param out_size Must be 3
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_get_apob_debug, 0, 3) {
    out[0] = g_apob_source;
    out[1] = g_fw_addr_count;
    out[2] = g_fw_efs_addr;

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

    new top_mem = get_top_mem();

    // The address recorded in the firmware image first, then the known ones
    new fw_addrs[MAX_FW_ADDRESSES];
    new fw_count = find_firmware_addresses(fw_addrs);
    new bool:found = false;

    for (new i = 0; i < fw_count && !found; ++i) {
        debug_print(''AMDAPOB: address in the firmware image: %x\n'', fw_addrs[i]);
        found = probe(fw_addrs[i], top_mem);
    }

    if (found)
        g_apob_source = APOB_SOURCE_FIRMWARE;

    for (new i = 0; i < sizeof k_apob_addresses && !found; ++i)
        found = probe(k_apob_addresses[i], top_mem);

    if (!found) {
        debug_print(''AMDAPOB: not found\n'');
        return STATUS_NOT_FOUND;
    }

    if (!g_apob_source)
        g_apob_source = APOB_SOURCE_KNOWN;

    debug_print(''AMDAPOB: found at %x, version %x, size %x, header size %x\n'',
        g_apob_addr, g_apob_version, g_apob_table_size, g_apob_header_size);

    return STATUS_SUCCESS;
}
