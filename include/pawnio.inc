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
    new retval = 0;
    new excpt = fix_status(invoke(MmMapIoSpace, retval, pa, size, 0 /* MmNonCached */));
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
    new retval = 0;
    invoke(MmUnmapIoSpace, retval, va, size);
    return 0;
}

stock microsleep(us) {
    new p = virtual_alloc(8);
    if (!p)
        return STATUS_NO_MEMORY;
    virtual_write_qword(p, -10000 * us);
    static KeDelayExecutionThread = 0;
    if (!KeDelayExecutionThread) {
        KeDelayExecutionThread = get_proc_address(''KeDelayExecutionThread'');
        if (!KeDelayExecutionThread)
            return STATUS_UNSUCCESSFUL;
    }
    new retval = 0;
    invoke(KeDelayExecutionThread, retval, 0 /* KernelMode */, 0 /* FALSE */, p);
    virtual_free(p);
    return fix_status(retval);
}

stock fix_status(status) {
    return SIGN_EXTEND32(status);
}


