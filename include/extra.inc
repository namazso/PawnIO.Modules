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

// Windows or PawnIO specific extra functions.

#define PAGE_SIZE (0x1000)

const CpuVendor: {
    CpuVendor_AMD   = 0xc0e946f161868306, // AuthenticAMD
    CpuVendor_Intel = 0x0d4770f6cacd93a6, // GenuineIntel
    CpuVendor_VIA   = 0xc3feac365a7e42f0, // CentaurHauls
    CpuVendor_Hygon = 0x4fc576e4f1b66033, // HygonGenuine
};

/// Get CPU vendor.
///
/// @return CPU vendor
stock CpuVendor:get_cpu_vendor() {
    new vendor[4];
    cpuid(0, 0, vendor);
    return CpuVendor:(fnv1ad(vendor[2], fnv1ad(vendor[3], fnv1ad(vendor[1]))));
}

/// Get CPU family, model, and stepping.
///
/// @return CPU family (bits 16-23), model (bits 8-15), and stepping (bits 0-7)
stock get_cpu_fms() {
    new procinfo[4];
    cpuid(1, 0, procinfo);
    new family = ((procinfo[0] & 0x0FF00000) >>> 20) + ((procinfo[0] & 0x0F00) >> 8);
    new model = ((procinfo[0] & 0x0F0000) >>> 12) | ((procinfo[0] & 0xF0) >> 4);
    new stepping = procinfo[0] & 0x0F;
    return (family << 16) | (model << 8) | stepping;
}

/// Get CPU family from FMS.
///
/// @param fms CPU family, model, and stepping as returned by get_cpu_fms()
/// @return CPU family (bits 16-23)
stock cpu_fms_family(fms) {
    return (fms >>> 16) & 0xFF;
}

/// Get CPU model from FMS.
///
/// @param fms CPU family, model, and stepping as returned by get_cpu_fms()
/// @return CPU model (bits 8-15)
stock cpu_fms_model(fms) {
    return (fms >>> 8) & 0xFF;
}

/// Get CPU stepping from FMS.
///
/// @param fms CPU family, model, and stepping as returned by get_cpu_fms()
/// @return CPU stepping (bits 0-7)
stock cpu_fms_stepping(fms) {
    return fms & 0xFF;
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

const VA:KUSER_SHARED_DATA = 0xFFFFF78000000000;

/// Get tick count.
///
/// @return Tick count
stock get_tick_count() {
    new value;
    new multiplier;
    // KUSER_SHARED_DATA->TickCountQuad
    virtual_read_qword(KUSER_SHARED_DATA + 0x0320, value);
    virtual_read_dword(KUSER_SHARED_DATA + 0x0004, multiplier);
    return (value * multiplier) >>> 24;
}
