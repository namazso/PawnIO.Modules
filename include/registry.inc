//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  namazso <admin@namazso.eu>
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

/// Registry value types.
const REG_NONE = 0;
const REG_SZ = 1;
const REG_EXPAND_SZ = 2;
const REG_BINARY = 3;
const REG_DWORD = 4;
const REG_DWORD_BIG_ENDIAN = 5;
const REG_LINK = 6;
const REG_MULTI_SZ = 7;
const REG_QWORD = 11;

/// Highest valid Unicode codepoint.
const UNICODE_MAX = 0x10FFFF;

// OBJECT_ATTRIBUTES::Attributes flags.
const OBJ_CASE_INSENSITIVE = 0x00000040;
const OBJ_KERNEL_HANDLE = 0x00000200;

// Registry key access rights.
const KEY_QUERY_VALUE = 0x0001;

// KEY_VALUE_INFORMATION_CLASS::KeyValuePartialInformation.
const KeyValuePartialInformation = 2;

// A UNICODE_STRING::Length is a USHORT counting bytes, so this many bytes is
// the hard ceiling for a path or value name.
const UNICODE_STRING_MAX_BYTES = 0xFFFE;

/// Ceiling on the scratch buffer handed to ZwQueryValueKey.
const REG_MAX_DATA_BYTES = 0x10000;

// Fixed part of the single scratch allocation each query makes. Everything the
// kernel touches lives in that allocation: the OBJECT_ATTRIBUTES, both
// UNICODE_STRINGs and their UTF-16 buffers, the output handle, the returned
// length, and the KEY_VALUE_PARTIAL_INFORMATION buffer.
//
//   +0x00  OBJECT_ATTRIBUTES        (48 bytes)
//   +0x30  UNICODE_STRING           key path
//   +0x40  UNICODE_STRING           value name
//   +0x50  HANDLE                   key handle out
//   +0x58  ULONG                    ResultLength out (8 bytes, kept aligned)
//   +0x60  WCHAR[]                  key path, then value name, then the KVPI
const REG_OFF_OA = 0x00;
const REG_OFF_PATH_US = 0x30;
const REG_OFF_NAME_US = 0x40;
const REG_OFF_HANDLE = 0x50;
const REG_OFF_RESULTLEN = 0x58;
const REG_OFF_SCRATCH = 0x60;

// sizeof(OBJECT_ATTRIBUTES) on x64.
const REG_SIZEOF_OA = 48;

// Field offsets within KEY_VALUE_PARTIAL_INFORMATION.
const REG_KVPI_TYPE = 0x04;
const REG_KVPI_DATALENGTH = 0x08;
const REG_KVPI_DATA = 0x0C;

// Indices into the layout array describing the variable part of the allocation.
const REG_LAYOUT_PATH_OFF = 0;
const REG_LAYOUT_PATH_AREA = 1;
const REG_LAYOUT_NAME_OFF = 2;
const REG_LAYOUT_NAME_AREA = 3;
const REG_LAYOUT_DATA_OFF = 4;
const REG_LAYOUT_DATA_AREA = 5;
const REG_LAYOUT_TOTAL = 6;
const REG_LAYOUT_CELLS = 7;

#define REG_ALIGN8(%1) (((%1) + 7) & ~7)

static VAProc:g_ZwOpenKey = VAProc:0;
static VAProc:g_ZwQueryValueKey = VAProc:0;
static VAProc:g_ZwClose = VAProc:0;

/// Resolve the ntoskrnl routines the registry helpers need.
///
/// Safe to call repeatedly; the addresses are cached after the first success.
/// Calling it once from main() keeps the first query from paying for the
/// lookups.
///
/// @return Whether all three routines were found
stock bool:reg_resolve_imports() {
    if (g_ZwOpenKey == VAProc:0) {
        g_ZwOpenKey = get_proc_address(''ZwOpenKey'');
    }
    if (g_ZwQueryValueKey == VAProc:0) {
        g_ZwQueryValueKey = get_proc_address(''ZwQueryValueKey'');
    }
    if (g_ZwClose == VAProc:0) {
        g_ZwClose = get_proc_address(''ZwClose'');
    }
    return g_ZwOpenKey != VAProc:0 && g_ZwQueryValueKey != VAProc:0 && g_ZwClose != VAProc:0;
}

/// Length of a NUL terminated Pawn string, in cells.
///
/// @param s String to measure
/// @return Cell count, excluding the terminator
stock reg_strlen(const s[]) {
    new n = 0;
    while (s[n] != 0) {
        n++;
    }
    return n;
}

/// Encode a codepoint string as UTF-16 into kernel memory.
///
/// A cell in the surrogate range is not a Unicode scalar value, but it is passed
/// straight through as a single code unit rather than rejected. See the note on
/// reg_read_utf16().
///
/// @param src Codepoint string
/// @param src_len Codepoints in src
/// @param dst Destination virtual address
/// @param dst_bytes Bytes available at dst
/// @param units Receives the UTF-16 code units written
/// @return An NTSTATUS
stock NTSTATUS:reg_write_utf16(const src[], src_len, VA:dst, dst_bytes, &units) {
    new n = 0;
    for (new i = 0; i < src_len; i++) {
        new cp = src[i];
        if (cp < 0 || cp > UNICODE_MAX) {
            return STATUS_INVALID_PARAMETER;
        }
        if (cp < 0x10000) {
            if ((n + 1) * 2 > dst_bytes) {
                return STATUS_BUFFER_TOO_SMALL;
            }
            virtual_write_word(dst + (n * 2), cp);
            n++;
        } else {
            if ((n + 2) * 2 > dst_bytes) {
                return STATUS_BUFFER_TOO_SMALL;
            }
            new v = cp - 0x10000;
            virtual_write_word(dst + (n * 2), 0xD800 | (v >>> 10));
            virtual_write_word(dst + (n * 2) + 2, 0xDC00 | (v & 0x3FF));
            n += 2;
        }
    }
    units = n;
    return STATUS_SUCCESS;
}

/// Decode UTF-16 in kernel memory into a codepoint string.
///
/// Stops at the first NUL code unit, so a value carrying the usual trailing NUL
/// and one without both decode to the same string.
///
/// Registry data is not trustworthy and may hold ill-formed UTF-16. An unpaired
/// surrogate is kept as its own codepoint instead of being rejected.
///
/// @param src Source virtual address
/// @param units UTF-16 code units available at src
/// @param dst Destination codepoint string, NUL terminated on success
/// @param dst_size Cells available in dst, including room for the terminator
/// @param len Receives the codepoints written, excluding the terminator
/// @return An NTSTATUS
stock NTSTATUS:reg_read_utf16(VA:src, units, dst[], dst_size, &len) {
    new n = 0;
    new i = 0;
    while (i < units) {
        new hi = 0;
        virtual_read_word(src + (i * 2), hi);
        i++;
        if (hi == 0) {
            break;
        }

        // A high surrogate followed by a low one is an astral codepoint.
        // Anything else, surrogate or not, stands for itself.
        new cp = hi;
        if (hi >= 0xD800 && hi <= 0xDBFF && i < units) {
            new lo = 0;
            virtual_read_word(src + (i * 2), lo);
            if (lo >= 0xDC00 && lo <= 0xDFFF) {
                cp = 0x10000 + (((hi - 0xD800) << 10) | (lo - 0xDC00));
                i++;
            }
        }

        // Leave room for the terminator.
        if (n + 1 >= dst_size) {
            return STATUS_BUFFER_TOO_SMALL;
        }
        dst[n] = cp;
        n++;
    }

    if (n >= dst_size) {
        return STATUS_BUFFER_TOO_SMALL;
    }
    dst[n] = 0;
    len = n;
    return STATUS_SUCCESS;
}

/// Work out how to carve up the scratch allocation for one query.
///
/// @param path_len Codepoints in the key path
/// @param name_len Codepoints in the value name
/// @param want_type Required value type, REG_SZ or REG_DWORD
/// @param out_size Cells available in the caller's output buffer
/// @param layout Receives the offsets and sizes, indexed by REG_LAYOUT_*
stock Void:reg_plan_layout(path_len, name_len, want_type, out_size, layout[REG_LAYOUT_CELLS]) {
    // Worst case every codepoint is astral: two code units, four bytes. Plus a
    // NUL code unit, which ZwOpenKey does not need but costs nothing.
    new path_area = REG_ALIGN8(path_len * 4 + 2);
    new name_area = REG_ALIGN8(name_len * 4 + 2);

    new data_area;
    if (want_type == REG_DWORD) {
        data_area = REG_ALIGN8(REG_KVPI_DATA + 8);
    } else {
        // Four bytes of UTF-16 per output cell covers any string that could
        // still fit in the caller's buffer, so overflowing this buffer means
        // overflowing theirs. The extra four bytes cover the value's own NUL.
        data_area = REG_ALIGN8(REG_KVPI_DATA + out_size * 4 + 4);
        if (data_area > REG_MAX_DATA_BYTES) {
            data_area = REG_MAX_DATA_BYTES;
        }
    }

    layout[REG_LAYOUT_PATH_OFF] = REG_OFF_SCRATCH;
    layout[REG_LAYOUT_PATH_AREA] = path_area;
    layout[REG_LAYOUT_NAME_OFF] = REG_OFF_SCRATCH + path_area;
    layout[REG_LAYOUT_NAME_AREA] = name_area;
    layout[REG_LAYOUT_DATA_OFF] = REG_OFF_SCRATCH + path_area + name_area;
    layout[REG_LAYOUT_DATA_AREA] = data_area;
    layout[REG_LAYOUT_TOTAL] = REG_OFF_SCRATCH + path_area + name_area + data_area;
}

/// Fill in the structures ZwOpenKey will read out of the scratch allocation.
///
/// @param base Scratch allocation
/// @param layout Layout from reg_plan_layout()
/// @param path Key path, one codepoint per cell
/// @param path_len Codepoints in path
/// @param name Value name, one codepoint per cell
/// @param name_len Codepoints in name
/// @return An NTSTATUS
stock NTSTATUS:reg_fill_request(VA:base, const layout[REG_LAYOUT_CELLS], const path[], path_len, const name[], name_len) {
    // virtual_alloc() makes no promise about the contents, and the kernel reads
    // every field of the header.
    for (new i = 0; i < REG_OFF_SCRATCH; i += 8) {
        virtual_write_qword(base + i, 0);
    }

    new path_units = 0;
    new NTSTATUS:status = reg_write_utf16(
        path,
        path_len,
        base + layout[REG_LAYOUT_PATH_OFF],
        layout[REG_LAYOUT_PATH_AREA],
        path_units
    );
    if (status != STATUS_SUCCESS) {
        return status;
    }

    new name_units = 0;
    status = reg_write_utf16(
        name,
        name_len,
        base + layout[REG_LAYOUT_NAME_OFF],
        layout[REG_LAYOUT_NAME_AREA],
        name_units
    );
    if (status != STATUS_SUCCESS) {
        return status;
    }

    if (path_units * 2 > UNICODE_STRING_MAX_BYTES || name_units * 2 > UNICODE_STRING_MAX_BYTES) {
        return STATUS_INVALID_PARAMETER;
    }

    // UNICODE_STRING { USHORT Length; USHORT MaximumLength; PWSTR Buffer; }
    virtual_write_word(base + REG_OFF_PATH_US, path_units * 2);
    virtual_write_word(base + REG_OFF_PATH_US + 2, path_units * 2);
    virtual_write_qword(base + REG_OFF_PATH_US + 8, _:(base + layout[REG_LAYOUT_PATH_OFF]));

    virtual_write_word(base + REG_OFF_NAME_US, name_units * 2);
    virtual_write_word(base + REG_OFF_NAME_US + 2, name_units * 2);
    virtual_write_qword(base + REG_OFF_NAME_US + 8, _:(base + layout[REG_LAYOUT_NAME_OFF]));

    // OBJECT_ATTRIBUTES; RootDirectory, SecurityDescriptor and
    // SecurityQualityOfService stay NULL from the zero fill above.
    virtual_write_dword(base + REG_OFF_OA, REG_SIZEOF_OA);
    virtual_write_qword(base + REG_OFF_OA + 0x10, _:(base + REG_OFF_PATH_US));
    virtual_write_dword(base + REG_OFF_OA + 0x18, OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE);

    return STATUS_SUCCESS;
}

/// Open the key, read the value into the scratch allocation, close the key.
///
/// @param base Scratch allocation, already filled by reg_fill_request()
/// @param layout Layout from reg_plan_layout()
/// @return An NTSTATUS
stock NTSTATUS:reg_open_and_query(VA:base, const layout[REG_LAYOUT_CELLS]) {
    new ret = 0;

    // ZwOpenKey(&KeyHandle, DesiredAccess, &ObjectAttributes)
    new NTSTATUS:ex = invoke(
        g_ZwOpenKey,
        ret,
        _:(base + REG_OFF_HANDLE),
        KEY_QUERY_VALUE,
        _:(base + REG_OFF_OA)
    );
    if (ex != STATUS_SUCCESS) {
        return ex;
    }
    new NTSTATUS:status = NTSTATUS:SIGN_EXTEND32(ret);
    if (status != STATUS_SUCCESS) {
        return status;
    }

    new hkey = 0;
    virtual_read_qword(base + REG_OFF_HANDLE, hkey);

    // ZwQueryValueKey(KeyHandle, &ValueName, InfoClass, Buffer, Length, &ResultLength)
    ex = invoke(
        g_ZwQueryValueKey,
        ret,
        hkey,
        _:(base + REG_OFF_NAME_US),
        KeyValuePartialInformation,
        _:(base + layout[REG_LAYOUT_DATA_OFF]),
        layout[REG_LAYOUT_DATA_AREA],
        _:(base + REG_OFF_RESULTLEN)
    );
    if (ex == STATUS_SUCCESS) {
        status = NTSTATUS:SIGN_EXTEND32(ret);
    } else {
        status = ex;
    }

    new close_ret = 0;
    invoke(g_ZwClose, close_ret, hkey);

    // STATUS_BUFFER_OVERFLOW is passed back untouched: the fixed header of the
    // KEY_VALUE_PARTIAL_INFORMATION is still valid, so the caller can tell a
    // wrong type apart from a value that simply did not fit.
    return status;
}

/// Convert the queried value out of the scratch allocation for the caller.
///
/// @param base Scratch allocation
/// @param layout Layout from reg_plan_layout()
/// @param want_type Required value type, REG_SZ or REG_DWORD
/// @param truncated Whether the data was too big for the scratch allocation
/// @param out For REG_DWORD, out[0] receives the value; for REG_SZ, the string
/// @param out_size Cells available in out
/// @param out_len Receives cells written, excluding any terminator
/// @return An NTSTATUS
stock NTSTATUS:reg_read_result(VA:base, const layout[REG_LAYOUT_CELLS], want_type, bool:truncated, out[], out_size, &out_len) {
    new data_off = layout[REG_LAYOUT_DATA_OFF];

    new type = 0;
    new data_len = 0;
    virtual_read_dword(base + data_off + REG_KVPI_TYPE, type);
    virtual_read_dword(base + data_off + REG_KVPI_DATALENGTH, data_len);

    // Checked before the length, because a value of the wrong type is worth
    // reporting as such even when it was also too big to read.
    if (type != want_type) {
        return STATUS_OBJECT_TYPE_MISMATCH;
    }
    if (truncated) {
        // The scratch buffer was sized so that anything fitting in the caller's
        // output fits there too, so this is really the caller's buffer.
        return STATUS_BUFFER_TOO_SMALL;
    }
    if (data_len < 0 || REG_KVPI_DATA + data_len > layout[REG_LAYOUT_DATA_AREA]) {
        return STATUS_INFO_LENGTH_MISMATCH;
    }

    if (want_type == REG_DWORD) {
        if (data_len != 4) {
            return STATUS_INFO_LENGTH_MISMATCH;
        }
        new value = 0;
        virtual_read_dword(base + data_off + REG_KVPI_DATA, value);
        out[0] = value;
        out_len = 1;
        return STATUS_SUCCESS;
    }

    // An odd byte count cannot be UTF-16; the stray byte is dropped.
    return reg_read_utf16(base + data_off + REG_KVPI_DATA, data_len / 2, out, out_size, out_len);
}

/// Query a registry value, converting it for the caller.
///
/// Makes exactly one allocation, holding every structure the kernel sees.
///
/// @param path NUL terminated native key path, one codepoint per cell
/// @param name NUL terminated value name, one codepoint per cell
/// @param want_type Required value type, REG_SZ or REG_DWORD
/// @param out For REG_DWORD, out[0] receives the value; for REG_SZ, the string
/// @param out_size Cells available in out
/// @param out_len Receives cells written, excluding any terminator
/// @return An NTSTATUS
stock NTSTATUS:reg_query_typed(const path[], const name[], want_type, out[], out_size, &out_len) {
    out_len = 0;

    if (!reg_resolve_imports()) {
        return STATUS_PROCEDURE_NOT_FOUND;
    }
    if (out_size < 1) {
        return STATUS_BUFFER_TOO_SMALL;
    }

    new path_len = reg_strlen(path);
    if (path_len == 0) {
        return STATUS_INVALID_PARAMETER;
    }
    // An empty name is legal and selects the key's default value.
    new name_len = reg_strlen(name);

    new layout[REG_LAYOUT_CELLS];
    reg_plan_layout(path_len, name_len, want_type, out_size, layout);

    new VA:base = virtual_alloc(layout[REG_LAYOUT_TOTAL]);
    if (base == NULL) {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    new NTSTATUS:status = reg_fill_request(base, layout, path, path_len, name, name_len);
    if (status == STATUS_SUCCESS) {
        status = reg_open_and_query(base, layout);
    }
    if (status == STATUS_SUCCESS || status == STATUS_BUFFER_OVERFLOW) {
        // On overflow the value's type and size came back even though its data
        // did not, which is enough to report the more useful error.
        status = reg_read_result(
            base,
            layout,
            want_type,
            status == STATUS_BUFFER_OVERFLOW,
            out,
            out_size,
            out_len
        );
    }

    virtual_free(base);
    return status;
}

/// Query a REG_DWORD value.
///
/// @param path NUL terminated native key path, one codepoint per cell
/// @param name NUL terminated value name, empty for the key's default value
/// @param value Receives the value, zero extended from 32 bits
/// @return An NTSTATUS
/// @retval STATUS_OBJECT_TYPE_MISMATCH The value exists but is not a REG_DWORD
stock NTSTATUS:reg_query_dword(const path[], const name[], &value) {
    new buf[1];
    new len = 0;
    new NTSTATUS:status = reg_query_typed(path, name, REG_DWORD, buf, sizeof buf, len);
    if (status == STATUS_SUCCESS) {
        value = buf[0];
    }
    return status;
}

/// Query a REG_SZ value.
///
/// @param path NUL terminated native key path, one codepoint per cell
/// @param name NUL terminated value name, empty for the key's default value
/// @param out Receives the string, one codepoint per cell, NUL terminated
/// @param out_size Cells available in out, including room for the terminator
/// @param len Receives the codepoint count, excluding the terminator
/// @return An NTSTATUS
/// @retval STATUS_BUFFER_TOO_SMALL The value does not fit in out_size cells, or
///                                 exceeds REG_MAX_DATA_BYTES
/// @retval STATUS_OBJECT_TYPE_MISMATCH The value exists but is not a REG_SZ
stock NTSTATUS:reg_query_sz(const path[], const name[], out[], out_size, &len) {
    return reg_query_typed(path, name, REG_SZ, out, out_size, len);
}
