//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2023  namazso <admin@namazso.eu>
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

#include "core.inc"
#include "ntstatus.inc"
#include "native.inc"

stock is_amd(vendor[4]) {
    return vendor[1] == CHAR4_CONST('A', 'u', 't', 'h') && vendor[3] == CHAR4_CONST('e', 'n', 't', 'i') && vendor[2] == CHAR4_CONST('c', 'A', 'M', 'D');
}

stock is_intel(vendor[4]) {
    return vendor[1] == CHAR4_CONST('G', 'e', 'n', 'u') && vendor[3] == CHAR4_CONST('i', 'n', 'e', 'I') && vendor[2] == CHAR4_CONST('n', 't', 'e', 'l');
}

stock io_space_map(pa, size) {
    static MmMapIoSpace = 0;
    if (!MmMapIoSpace) {
        MmMapIoSpace = get_proc_address(''MmMapIoSpace'');
        if (!MmMapIoSpace)
            return 0;
    }
    new retval;
    new excpt = invoke(MmMapIoSpace, retval, pa, size, 0 /* MmNonCached */);
    if (!NT_SUCCESS(excpt))
        return 0;
    return retval;
}

stock io_space_unmap(va, size) {
    static MmUnmapIoSpace = 0;
    if (!MmUnmapIoSpace) {
        MmUnmapIoSpace = get_proc_address(''MmUnmapIoSpace'');
        if (!MmUnmapIoSpace)
            return 0;
    }
    new retval;
    invoke(MmUnmapIoSpace, retval, va, size);
    return 0;
}

