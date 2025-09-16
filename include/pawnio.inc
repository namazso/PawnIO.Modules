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

#include "core.inc"
#include "util.inc"
#include "ntstatus.inc"
#include "native.inc"
#include "extra.inc"

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
        if (((%2) != 0 && in_size != (%2)) || ((%3) != 0 && out_size != (%3))) \
            return STATUS_INVALID_PARAMETER; \
        else

NTSTATUS:main();

/// Executed on module unload.
forward NTSTATUS:unload();
