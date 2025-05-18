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
#include "ntstatus.inc"
#include "native.inc"

/// Test if a vendor string is AMD
///
/// @param vendor Vendor string from CPUID
/// @return Whether the vendor is AMD
stock bool:is_amd(vendor[4]) {
    return vendor[1] == CHAR4_CONST('A', 'u', 't', 'h') && vendor[3] == CHAR4_CONST('e', 'n', 't', 'i') && vendor[2] == CHAR4_CONST('c', 'A', 'M', 'D');
}

/// Test if a vendor string is Intel
///
/// @param vendor Vendor string from CPUID
/// @return Whether the vendor is Intel
stock bool:is_intel(vendor[4]) {
    return vendor[1] == CHAR4_CONST('G', 'e', 'n', 'u') && vendor[3] == CHAR4_CONST('i', 'n', 'e', 'I') && vendor[2] == CHAR4_CONST('n', 't', 'e', 'l');
}

/// Busy sleep, not recommended above 50us.
///
/// @param us Microseconds to sleep
stock Void:microsleep2(us) {
    static VAProc:KeStallExecutionProcessor = VAProc:0;
    if (KeStallExecutionProcessor == VAProc:0) {
        KeStallExecutionProcessor = get_proc_address(''KeStallExecutionProcessor'');
    }
    // Allow precaching the ptr with us == 0
    if (us > 0) {
        new ret;
        invoke(KeStallExecutionProcessor, ret, us);
    }
}

#define KUSER_SHARED_DATA VA:0xFFFFF78000000000

/// Get tick count.
///
/// @return Tick count
stock get_tick_count() {
    new value;
    new multiplier;
    // KUSER_SHARED_DATA->TickCountQuad
    virtual_read_qword(KUSER_SHARED_DATA + 0x0320, value);
    virtual_read_dword(KUSER_SHARED_DATA + 0x0004, multiplier);
    return (value * multiplier) >> 24;
}
