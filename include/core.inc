//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  namazso <admin@namazso.eu>
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

#define cellbytes (cellbits / 8)

#define SIGN_EXTEND(%1,%2) (((%1) << (cellbits - (%2))) >> (cellbits - (%2)))
#define SIGN_EXTEND8(%1) (SIGN_EXTEND(%1, 8))
#define SIGN_EXTEND16(%1) (SIGN_EXTEND(%1, 16))
#define SIGN_EXTEND32(%1) (SIGN_EXTEND(%1, 32))

#define CHAR1_CONST(%1) (%1)
#define CHAR2_CONST(%1,%2) (CHAR1_CONST(%1) | ((%2) << 8))
#define CHAR3_CONST(%1,%2,%3) (CHAR2_CONST(%1, %2) | ((%3) << 16))
#define CHAR4_CONST(%1,%2,%3,%4) (CHAR3_CONST(%1, %2, %3) | ((%4) << 24))
#define CHAR5_CONST(%1,%2,%3,%4,%5) (CHAR4_CONST(%1, %2, %3, %4) | ((%5) << 32))
#define CHAR6_CONST(%1,%2,%3,%4,%5,%6) (CHAR5_CONST(%1, %2, %3, %4, %5) | ((%6) << 40))
#define CHAR7_CONST(%1,%2,%3,%4,%5,%6,%7) (CHAR6_CONST(%1, %2, %3, %4, %5, %6) | ((%7) << 48))
#define CHAR8_CONST(%1,%2,%3,%4,%5,%6,%7,%8) (CHAR7_CONST(%1, %2, %3, %4, %5, %6, %7) | ((%8) << 56))

/// Minimum of two numbers.
///
/// @param a First number
/// @param b Second number
/// @return The smaller of the two arguments
stock _min(a, b) {
    return a < b ? a : b;
}

/// Maximum of two numbers.
///
/// @param a First number
/// @param b Second number
/// @return The greater of the two arguments
stock _max(a, b) {
    return a > b ? a : b;
}

#define PAGE_SIZE (0x1000)

#define BIT(%1) (1 << (%1))

/// Unpack bytes packed into cells.
///
/// @param src Source array
/// @param dst Destination array
/// @param size Count of bytes to unpack
/// @param src_offs Source offset, in bytes
/// @param dst_offs Destination offset, in cells
stock Void:unpack_bytes_le(src[], dst[], size, src_offs = 0, dst_offs = 0) {
    for (new i = 0; i < size; i++) {
        new src_idx = src_offs + i;
        new src_cell = src[src_idx / cellbytes];
        new byte = (src_cell >> ((src_idx % cellbytes) * 8)) & 0xff;
        dst[dst_offs + i] = byte;
    }
}

/// Pack bytes into cells
///
/// @param src Source array
/// @param dst Destination array
/// @param size Count of bytes to pack
/// @param src_offs Source offset, in cells
/// @param dst_offs Destination offset, in bytes
stock Void:pack_bytes_le(src[], dst[], size, src_offs = 0, dst_offs = 0) {
    for (new i = 0; i < size; i++) {
        new byte = src[src_offs + i] & 0xff;
        new dst_idx = dst_offs + i;
        dst[dst_idx / cellbytes] &= ~(0xff << ((dst_idx % cellbytes) * 8));
        dst[dst_idx / cellbytes] |= byte << ((dst_idx % cellbytes) * 8);
    }
}

/// Get individual bytes from a cell.
///
/// @param src Source cell
/// @param idx Byte index in cell
stock get_byte_le(src, idx) {
    return((src >> ((idx % cellbytes) * 8)) & 0xff);
}

#define GET_BYTE_LE(%1,%2) (get_byte_le((%1[(%2) / cellbytes]), ((%2) % cellbytes)))


/// Set an individual byte in a cell.
///
/// @param dst Destination cell
/// @param idx Byte index in cell
/// @param val Byte value to set
stock set_byte_le(&dst, idx, val) {
    new shift = ((idx % cellbytes) * 8);
    dst &= ~(0xff << shift);
    dst |= ((val & 0xff) << shift);
}

#define SET_BYTE_LE(%1,%2,%3) (set_byte_le((%1[(%2) / cellbytes]), ((%2) % cellbytes), (%3)))

/// Define an IOCTL function.
/// @param %1 Name of the IOCTL function, must start with ioctl_
#define DEFINE_IOCTL(%1) \
    forward NTSTATUS:%1(in[], in_size, out[], out_size); \
    public NTSTATUS:%1(in[], in_size, out[], out_size)

/// Define an IOCTL function with static buffer sizes.
/// @param %1 Name of the IOCTL function, must start with ioctl_
/// @param %2 Size of the input buffer
/// @param %3 Size of the output buffer
#define DEFINE_IOCTL_SIZED(%1,%2,%3) \
    forward NTSTATUS:%1(in[%2], in_size, out[%3], out_size); \
    public NTSTATUS:%1(in[%2], in_size, out[%3], out_size) \
        if (in_size != (%2)) \
            return STATUS_INVALID_PARAMETER; \
        else if (out_size != (%3)) \
            return STATUS_INVALID_PARAMETER; \
        else

NTSTATUS:main();

/// Executed on module unload.
forward NTSTATUS:unload();
