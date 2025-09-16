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

// Implementation of some of Pawn's core functions. Normally these would be
// natives, but they're easily implemented in Pawn itself.

stock tolower(c) {
    return (c >= 'A' && c <= 'Z') ? (c + ('a' - 'A')) : c;
}

stock toupper(c) {
    return (c >= 'a' && c <= 'z') ? (c - ('a' - 'A')) : c;
}

stock swapchars(c) {
    const cellbytes = cellbits / 8;
    for (new i = 0; i < cellbytes / 2; i++) {
        new low = (c >>> (i * 8)) & 0xff;
        new high = (c >>> ((cellbytes - 1 - i) * 8)) & 0xff;
        c &= ~((0xff << (i * 8)) | (0xff << ((cellbytes - 1 - i) * 8)));
        c |= (low << ((cellbytes - 1 - i) * 8)) | (high << (i * 8));
    }
    return c;
}

stock min(a, b) {
    return a < b ? a : b;
}

stock max(a, b) {
    return a > b ? a : b;
}

stock clamp(value, min=cellmin, max=cellmax) {
    return value < min ? min : (value > max ? max : value);
}
