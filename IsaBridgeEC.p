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

#include <pawnio.inc>

stock mask64on32(in, mask) {
    return (in & (mask >> 32)) | (mask & 0xFFFFFFFF);
}

#define	REG_2E	0x2e
#define	REG_4E	0x4e

#define IT8688E_DEVID   0x8688
#define IT8689E_DEVID   0x8689
#define IT8696E_DEVID   0x8696
#define IT8698E_DEVID   0x8698
#define IT8790E_DEVID   0x8790
#define IT8792E_DEVID   0x8733
#define IT87952E_DEVID  0x8695

#define SIO_CHIP_ID     0x20
#define SIO_LDN         0x07

new g_superio_base = 0;
new g_superio_size = 0;
new g_superio_chipid = 0xFFFF;

new g_superio_2_base = 0;
new g_superio_2_size = 0;
new g_superio_2_chipid = 0xFFFF;

new bool:g_is_intel;

const MMIOState: {
    MMIO_Original = -1,
    MMIO_Disabled = 0,
    MMIO_Enabled2E = 1,
    MMIO_Enabled4E = 2,
    MMIO_EnabledBoth = 3
};

superio_enter(ioreg) {
    io_out_byte(ioreg, 0x87);
    io_out_byte(ioreg, 0x01);
    io_out_byte(ioreg, 0x55);
    io_out_byte(ioreg, ioreg == REG_4E ? 0xaa : 0x55);
}

superio_exit(ioreg) {
    io_out_byte(ioreg, 0x02);
    io_out_byte(ioreg + 1, 0x02);
}

superio_inb(ioreg, reg) {
    io_out_byte(ioreg, reg);
    return io_in_byte(ioreg + 1);
}

superio_outb(ioreg, reg, val) {
    io_out_byte(ioreg, reg);
    io_out_byte(ioreg + 1, val);
}

superio_inw(ioreg, reg) {
    return (superio_inb(ioreg, reg) << 8) | superio_inb(ioreg, reg + 1);
}

find_superio_mmio_2e(&base, &size, &chipid) {
    superio_enter(REG_2E);
    new id = superio_inw(REG_2E, SIO_CHIP_ID);
    chipid = id;

    if (id == IT8688E_DEVID || id == IT8689E_DEVID || id == IT8696E_DEVID || id == IT8698E_DEVID) {
        new b = superio_inb(REG_2E, 0x24);
        if ((b & 0x20) != 0) {
            size = 0x2000;
            base = (((b | 0xF0) << 24) | (b >> 6 << 20));
        }
    }
    superio_exit(REG_2E);
}

find_superio_mmio_4e(&base, &size, &chipid) {
    new id = superio_inw(REG_4E, SIO_CHIP_ID);
    if (id == 0xFFFF) {
        superio_enter(REG_4E);
        id = superio_inw(REG_4E, SIO_CHIP_ID);
    }
    chipid = id;
    if (id == IT8790E_DEVID || id == IT8792E_DEVID) {
        superio_outb(REG_4E, SIO_LDN, 0x0F);
        new b1 = superio_inb(REG_4E, 0xF5);
        new b2 = superio_inb(REG_4E, 0xF6);
        size = 0x2000;
        base = (((b2 << 4) | (b1 >> 4)) << 12) | 0xFF000000;
    } else if (id == IT87952E_DEVID) {
        superio_outb(REG_4E, SIO_LDN, 0x0F);
        new b1 = superio_inb(REG_4E, 0xF5);
        new b2 = superio_inb(REG_4E, 0xF6);
        new b3 = superio_inb(REG_4E, 0xFC);
        size = 0x2000;
        base = (b3 << 24) | (((b2 << 4) | (b1 >> 4)) << 12) | 0xF0000000;
    }
}

find_superio_mmio() {
    // Double-check to avoid transient issues.
    // The original Gigabyte code doesn't do this, but better safe than sorry.

    new chipid_1, chipid_2;

    new base_1 = 0, size_1 = 0;
    find_superio_mmio_2e(base_1, size_1, chipid_1);
    new base_2 = 0, size_2 = 0;
    find_superio_mmio_4e(base_2, size_2, chipid_2);

    microsleep(1000);

    new base_1_2 = 0, size_1_2 = 0;
    find_superio_mmio_2e(base_1_2, size_1_2, chipid_1);
    new base_2_2 = 0, size_2_2 = 0;
    find_superio_mmio_4e(base_2_2, size_2_2, chipid_2);

    g_superio_chipid = chipid_1;
    g_superio_2_chipid = chipid_2;

    if (base_1 != 0 && size_1 != 0 && base_1 == base_1_2 && size_1 == size_1_2) {
        g_superio_size = size_1;
        g_superio_base = base_1;
    }
    if (base_2 != 0 && size_2 != 0 && base_2 == base_2_2 && size_2 == size_2_2) {
        g_superio_2_size = size_2;
        g_superio_2_base = base_2;
    }
}

bool:have_superio_mmio() {
    return g_superio_base != 0 || g_superio_2_base != 0;
}

/// Find MMIO for SuperIO chips.
///
/// @param out [0] = Slot 0 base [1] = Slot 0 size [2] = Slot 0 ChipID [3] = Slot 1 base [4] = Slot 1 size [5] = Slot 1 ChipID
/// @param out_size Must be 6
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_find_superio_mmio, 0, 6) {
    find_superio_mmio();
    
    debug_print("SuperIO 1 MMIO: Base = 0x%X, Size = 0x%X, ChipID = 0x%X\n", g_superio_base, g_superio_size, g_superio_chipid);
    debug_print("SuperIO 2 MMIO: Base = 0x%X, Size = 0x%X, ChipID = 0x%X\n", g_superio_2_base, g_superio_2_size, g_superio_2_chipid);

    out[0] = g_superio_base;
    out[1] = g_superio_size;
    out[2] = g_superio_chipid;
    out[3] = g_superio_2_base;
    out[4] = g_superio_2_size;
    out[5] = g_superio_2_chipid;

    return STATUS_SUCCESS;
}

// =============================================
// Mapped access
// =============================================

new VA:g_mapped_base = NULL;
new g_mapped_size = 0;
new VA:g_mapped_2_base = NULL;
new g_mapped_2_size = 0;

unmap_superio_mmio() {
    if (g_mapped_base) {
        io_space_unmap(g_mapped_base, g_mapped_size);
        g_mapped_base = NULL;
        g_mapped_size = 0;
    }
    if (g_mapped_2_base) {
        io_space_unmap(g_mapped_2_base, g_mapped_2_size);
        g_mapped_2_base = NULL;
        g_mapped_2_size = 0;
    }
}

NTSTATUS:map_superio_mmio() {
    unmap_superio_mmio();
    if (g_superio_base) {
        g_mapped_base = io_space_map(g_superio_base, g_superio_size);
        if (g_mapped_base == NULL)
            return STATUS_INSUFFICIENT_RESOURCES;
        g_mapped_size = g_superio_size;
        debug_print("Mapped SuperIO 1 MMIO at %p\n", _:g_mapped_base);
    }
    if (g_superio_2_base) {
        g_mapped_2_base = io_space_map(g_superio_2_base, g_superio_2_size);
        if (g_mapped_2_base == NULL) {
            unmap_superio_mmio();
            return STATUS_INSUFFICIENT_RESOURCES;
        }
        g_mapped_2_size = g_superio_2_size;
        debug_print("Mapped SuperIO 2 MMIO at %p\n", _:g_mapped_2_base);
    }
    return STATUS_SUCCESS;
}

/// Map MMIO region.
///
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_map_superio_mmio, 0, 0) {
    return map_superio_mmio();
}

/// Unmap MMIO region.
///
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_unmap_superio_mmio, 0, 0) {
    unmap_superio_mmio();
    return STATUS_SUCCESS;
}

NTSTATUS:virtual_read(VA:va, &v, size) {
    if (size == 1)
        return virtual_read_byte(va, v);
    else if (size == 2)
        return virtual_read_word(va, v);
    else if (size == 4)
        return virtual_read_dword(va, v);
    else if (size == 8)
        return virtual_read_qword(va, v);
    return STATUS_INVALID_PARAMETER;
}

NTSTATUS:virtual_write(VA:va, v, size) {
    if (size == 1)
        return virtual_write_byte(va, v);
    else if (size == 2)
        return virtual_write_word(va, v);
    else if (size == 4)
        return virtual_write_dword(va, v);
    else if (size == 8)
        return virtual_write_qword(va, v);
    return STATUS_INVALID_PARAMETER;
}

const AccessType: {
    Access_Read = 0,
    Access_Write = 1,
    Access_ReadWrite = 2,
    Access_ReadWriteAlways = 3,

    Access_Max
};

NTSTATUS:access_memory(VA:base, size, offset, access_size, AccessType:access_type, value, &old) {
    if (base == NULL || size == 0)
        return STATUS_INVALID_PARAMETER;
    if (offset < 0 || offset + access_size < 0 || offset + access_size > size)
        return STATUS_INVALID_PARAMETER;
    new VA:p = base + offset;
    new NTSTATUS:status = STATUS_SUCCESS;
    if (access_type == Access_Read) {
        return virtual_read(p, old, access_size);
    } else if (access_type == Access_Write) {
        return virtual_write(p, value, access_size);
    } else if (access_type == Access_ReadWrite) {
        if (access_size == 8)
            return STATUS_INVALID_PARAMETER;
        status = virtual_read(p, old, access_size);
        if (!NT_SUCCESS(status))
            return status;
        new newval = mask64on32(old, value);
        if (newval == old)
            return STATUS_SUCCESS;
        return virtual_write(p, newval, access_size);
    } else if (access_type == Access_ReadWriteAlways) {
        if (access_size == 8)
            return STATUS_INVALID_PARAMETER;
        status = virtual_read(p, old, access_size);
        if (!NT_SUCCESS(status))
            return status;
        new newval = mask64on32(old, value);
        return virtual_write(p, newval, access_size);
    }
    return STATUS_INVALID_PARAMETER;
}

/// Access mapped MMIO.
///
/// @param in [0] = Slot (0 = 2E, 1 = 4E) [1] = Offset [2] = Access size [3] = Access type [4] = Value
/// @param in_size Must be 5
/// @param out [0] = Old value
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_access_superio_mmio, 5, 1) {
    new slot = in[0];
    if (slot != 0 && slot != 1)
        return STATUS_INVALID_PARAMETER;
    new offset = in[1];
    if (offset < 0 || offset > 0xFFFFFFFF)
        return STATUS_INVALID_PARAMETER;
    new access_size = in[2];
    if (access_size != 1 && access_size != 2 && access_size != 4 && access_size != 8)
        return STATUS_INVALID_PARAMETER;
    new AccessType:access_type = AccessType:in[3];
    if (access_type < Access_Read || access_type >= Access_Max)
        return STATUS_INVALID_PARAMETER;
    new value = in[4];

    new VA:base, size;
    if (slot == 0) {
        base = g_mapped_base;
        size = g_mapped_size;
    } else {
        base = g_mapped_2_base;
        size = g_mapped_2_size;
    }

    return access_memory(base, size, offset, access_size, access_type, value, out[0]);
}

// =============================================
// AMD stuff
// =============================================

#define AmdConfig [.IoMemPortDecodeEnable, .PCIMemoryStartAddressForLpc, .ROMAddressRange2]

NTSTATUS:amd_get_config(out[AmdConfig]) {
    new NTSTATUS:status = STATUS_SUCCESS;
    status = pci_config_read_dword(0x0, 0x14, 0x3, 0x48, out.IoMemPortDecodeEnable);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(0x0, 0x14, 0x3, 0x60, out.PCIMemoryStartAddressForLpc);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(0x0, 0x14, 0x3, 0x6C, out.ROMAddressRange2);
    if (!NT_SUCCESS(status))
        return status;
    return status;
}

NTSTATUS:amd_set_config(in[AmdConfig]) {
    new NTSTATUS:status = STATUS_SUCCESS;
    status = pci_config_write_dword(0x0, 0x14, 0x3, 0x48, in.IoMemPortDecodeEnable);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_write_dword(0x0, 0x14, 0x3, 0x60, in.PCIMemoryStartAddressForLpc);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_write_dword(0x0, 0x14, 0x3, 0x6C, in.ROMAddressRange2);
    if (!NT_SUCCESS(status))
        return status;
    return status;
}

amd_config_make_mask(out_mask[AmdConfig], slot, mmio_base, bool:enabled) {
    const DecodeEnableBit = 0x20;
    if (!enabled) {
        out_mask.IoMemPortDecodeEnable = (~DecodeEnableBit) << 32;
        out_mask.PCIMemoryStartAddressForLpc = 0xFFFFFFFF00000000;
        out_mask.ROMAddressRange2 = 0xFFFFFFFF00000000;
        return;
    }
    out_mask.IoMemPortDecodeEnable = DecodeEnableBit;
    if (slot == 0) {
        // 2E/2F
        new pciAddressStart = (mmio_base >> 16) & 0xFF00;
        new pciAddressEnd = pciAddressStart + 1;
        out_mask.PCIMemoryStartAddressForLpc = ((pciAddressEnd << 16) | pciAddressStart) & 0xFFFFFFFF;
        out_mask.ROMAddressRange2 = 0xFFFFFF00;
    } else {
        // 4E/4F
        new pciAddressStart = (mmio_base >> 16) & 0xFFFF;
        new pciAddressEnd = pciAddressStart + 1;
        out_mask.PCIMemoryStartAddressForLpc = ((pciAddressEnd << 16) | pciAddressStart) & 0xFFFFFFFF;
        out_mask.ROMAddressRange2 = (0xFFFF0000 | pciAddressEnd) & 0xFFFFFFFF;
    }
}

amd_config_apply_mask(out[AmdConfig], in[AmdConfig], in_mask[AmdConfig]) {
    out.IoMemPortDecodeEnable = mask64on32(in.IoMemPortDecodeEnable, in_mask.IoMemPortDecodeEnable);
    out.PCIMemoryStartAddressForLpc = mask64on32(in.PCIMemoryStartAddressForLpc, in_mask.PCIMemoryStartAddressForLpc);
    out.ROMAddressRange2 = mask64on32(in.ROMAddressRange2, in_mask.ROMAddressRange2);
}

bool:amd_config_is_equal(in[AmdConfig], other[AmdConfig]) {
    return in.IoMemPortDecodeEnable == other.IoMemPortDecodeEnable
        && in.PCIMemoryStartAddressForLpc == other.PCIMemoryStartAddressForLpc
        && in.ROMAddressRange2 == other.ROMAddressRange2;
}

bool:amd_config_test_mask(in[AmdConfig], in_mask[AmdConfig]) {
    new applied[AmdConfig];
    amd_config_apply_mask(applied, in, in_mask);
    return amd_config_is_equal(applied, in);
}

bool:amd_config_is_enabled(in[AmdConfig], slot, mmio_base) {
    new mask[AmdConfig];
    amd_config_make_mask(mask, slot, mmio_base, true);
    return amd_config_test_mask(in, mask);
}

bool:amd_config_is_disabled(in[AmdConfig]) {
    new mask[AmdConfig];
    amd_config_make_mask(mask, 0, 0, false);
    return amd_config_test_mask(in, mask);
}

MMIOState:amd_config_to_state(in[AmdConfig], base, base2) {
    if (amd_config_is_disabled(in))
        return MMIO_Disabled;
    if (base && amd_config_is_enabled(in, 0, base))
        return MMIO_Enabled2E;
    if (base2 && amd_config_is_enabled(in, 1, base2))
        return MMIO_Enabled4E;
    return MMIO_Original;
}

new g_amd_original_config[AmdConfig];
new bool:g_amd_original_config_saved = false;
new bool:g_amd_should_restore = false;

NTSTATUS:amd_ensure_config_saved() {
    if (g_amd_original_config_saved)
        return STATUS_SUCCESS;
    new NTSTATUS:status = amd_get_config(g_amd_original_config);
    if (NT_SUCCESS(status))
        g_amd_original_config_saved = true;
    return status;
}

NTSTATUS:amd_restore_config() {
    if (!g_amd_original_config_saved)
        return STATUS_SUCCESS;
    new NTSTATUS:status = amd_set_config(g_amd_original_config);
    if (NT_SUCCESS(status))
        g_amd_should_restore = false;
    return status;
}

NTSTATUS:amd_get_original_state(&MMIOState:mmio_state) {
    new NTSTATUS:status = amd_ensure_config_saved();
    if (!NT_SUCCESS(status))
        return status;

    mmio_state = amd_config_to_state(g_amd_original_config, g_superio_base, g_superio_2_base);
    return STATUS_SUCCESS;
}

NTSTATUS:amd_get_current_state(&MMIOState:mmio_state) {
    new config[AmdConfig];
    new NTSTATUS:status = amd_get_config(config);
    if (!NT_SUCCESS(status))
        return status;

    mmio_state = amd_config_to_state(config, g_superio_base, g_superio_2_base);
    return STATUS_SUCCESS;
}

NTSTATUS:amd_set_state(MMIOState:mmio_state) {
    if (mmio_state == MMIO_Original)
        return amd_restore_config();

    new NTSTATUS:status = amd_ensure_config_saved();
    if (!NT_SUCCESS(status))
        return status;

    new config[AmdConfig];
    status = amd_get_config(config);
    if (!NT_SUCCESS(status))
        return status;

    new target[AmdConfig];
    if (mmio_state == MMIO_Disabled) {
        amd_config_make_mask(target, 0, 0, false);
        amd_config_apply_mask(target, config, target);
    } else if (mmio_state == MMIO_Enabled2E) {
        if (g_superio_base == 0)
            return STATUS_INVALID_PARAMETER;
        amd_config_make_mask(target, 0, g_superio_base, true);
        amd_config_apply_mask(target, config, target);
    } else if (mmio_state == MMIO_Enabled4E) {
        if (g_superio_2_base == 0)
            return STATUS_INVALID_PARAMETER;
        amd_config_make_mask(target, 1, g_superio_2_base, true);
        amd_config_apply_mask(target, config, target);
    } else {
        return STATUS_INVALID_PARAMETER;
    }

    status = amd_set_config(target);
    if (NT_SUCCESS(status) && !amd_config_is_equal(target, g_amd_original_config))
        g_amd_should_restore = true;
    return status;
}

NTSTATUS:amd_test_support() {
    // see D14F3x https://www.amd.com/content/dam/amd/en/documents/archived-tech-docs/programmer-references/55072_AMD_Family_15h_Models_70h-7Fh_BKDG.pdf
    
    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0x0, 0x14, 0x3, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;

    // Something AMD
    if ((didvid & 0xFFFF) != 0x1022)
        return STATUS_NOT_SUPPORTED;

    new classrev;
    status = pci_config_read_dword(0x0, 0x14, 0x3, 8, classrev);
    if (!NT_SUCCESS(status))
        return status;
    
    // ISA bridge
    if (classrev >> 8 != 0x060100)
        return STATUS_NOT_SUPPORTED;
    
    return STATUS_SUCCESS;
}

// =============================================
// Intel stuff
// =============================================

#define IntelConfig [.BIOSDecodeEnable, .LPCGenericMemoryRange]

NTSTATUS:intel_get_config(out[IntelConfig]) {
    new NTSTATUS:status = STATUS_SUCCESS;
    status = pci_config_read_dword(0x0, 0x1F, 0x0, 0xD8, out.BIOSDecodeEnable);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_read_dword(0x0, 0x1F, 0x0, 0x98, out.LPCGenericMemoryRange);
    if (!NT_SUCCESS(status))
        return status;
    return status;
}

NTSTATUS:intel_set_config(in[IntelConfig]) {
    new NTSTATUS:status = STATUS_SUCCESS;
    status = pci_config_write_dword(0x0, 0x1F, 0x0, 0xD8, in.BIOSDecodeEnable);
    if (!NT_SUCCESS(status))
        return status;

    status = pci_config_write_dword(0x0, 0x1F, 0x0, 0x98, in.LPCGenericMemoryRange);
    if (!NT_SUCCESS(status))
        return status;
    return status;
}

intel_bios_mask_for_data_space(base) {
    if ((base & ~0xFFFFF) == 0xFF400000)
        return 0x0001;
    if ((base & ~0xFFFFF) == 0xFF500000)
        return 0x0002;
    if ((base & ~0xFFFFF) == 0xFF600000)
        return 0x0004;
    if ((base & ~0xFFFFF) == 0xFF700000)
        return 0x0008;
    if ((base & ~0xFFFF) == 0xE0000)
        return 0x0040;
    if ((base & ~0xFFFF) == 0xF0000)
        return 0x0080;
    if ((base & ~0x7FFFF) == 0xFFC00000)
        return 0x0100;
    if ((base & ~0x7FFFF) == 0xFFC80000)
        return 0x0200;
    if ((base & ~0x7FFFF) == 0xFFD00000)
        return 0x0400;
    if ((base & ~0x7FFFF) == 0xFFD80000)
        return 0x0800;
    if ((base & ~0x7FFFF) == 0xFFE00000)
        return 0x1000;
    if ((base & ~0x7FFFF) == 0xFFE80000)
        return 0x2000;
    if ((base & ~0x7FFFF) == 0xFFF00000)
        return 0x4000;
    if ((base & ~0x7FFFF) == 0xFFF80000)
        return 0x8000;
    return 0;
}

intel_bios_mask_for_feat_space(base) {
    if ((base & ~0xFFFFF) == 0xFF000000)
        return 0x0001;
    if ((base & ~0xFFFFF) == 0xFF100000)
        return 0x0002;
    if ((base & ~0xFFFFF) == 0xFF200000)
        return 0x0004;
    if ((base & ~0xFFFFF) == 0xFF300000)
        return 0x0008;
    if ((base & ~0x7FFFF) == 0xFF800000)
        return 0x0100;
    if ((base & ~0x7FFFF) == 0xFF880000)
        return 0x0200;
    if ((base & ~0x7FFFF) == 0xFF900000)
        return 0x0400;
    if ((base & ~0x7FFFF) == 0xFF980000)
        return 0x0800;
    if ((base & ~0x7FFFF) == 0xFFA00000)
        return 0x1000;
    if ((base & ~0x7FFFF) == 0xFFA80000)
        return 0x2000;
    if ((base & ~0x7FFFF) == 0xFFB00000)
        return 0x4000;
    if ((base & ~0x7FFFF) == 0xFFB80000)
        return 0x8000;
    return 0;
}

intel_config_make_mask(out_mask[IntelConfig], slot, mmio_base, bool:enabled) {
    if (!enabled) {
        out_mask.BIOSDecodeEnable = 0xFFFFFFFF00000001;
        out_mask.LPCGenericMemoryRange = 0xFFFFFFFF00000000;
        return;
    }
    if (slot == 0) {
        // 2E/2F
        new pciAddressStart = (mmio_base >> 16) & 0xFFFF;
        out_mask.BIOSDecodeEnable = 0xFFFFFFFE00000000;
        out_mask.LPCGenericMemoryRange = (pciAddressStart << 16) | 1;
    } else {
        // 4E/4F
        new pciAddressStart = (mmio_base >> 16) & 0xFFFF;
        new mask = intel_bios_mask_for_data_space(mmio_base);
        if (mask == 0)
            mask = intel_bios_mask_for_feat_space(mmio_base);
        if (mask == 0)
            mask = 0x0001;
        out_mask.BIOSDecodeEnable = ~mask << 32;
        out_mask.LPCGenericMemoryRange = (pciAddressStart << 16) | 1;
    }
}

intel_config_apply_mask(out[IntelConfig], in[IntelConfig], in_mask[IntelConfig]) {
    out.BIOSDecodeEnable = mask64on32(in.BIOSDecodeEnable, in_mask.BIOSDecodeEnable);
    out.LPCGenericMemoryRange = mask64on32(in.LPCGenericMemoryRange, in_mask.LPCGenericMemoryRange);
}

bool:intel_config_is_equal(in[IntelConfig], other[IntelConfig]) {
    return in.BIOSDecodeEnable == other.BIOSDecodeEnable
        && in.LPCGenericMemoryRange == other.LPCGenericMemoryRange;
}

bool:intel_config_test_mask(in[IntelConfig], in_mask[IntelConfig]) {
    new applied[IntelConfig];
    intel_config_apply_mask(applied, in, in_mask);
    return intel_config_is_equal(applied, in);
}

bool:intel_config_is_enabled(in[IntelConfig], slot, mmio_base) {
    new mask[IntelConfig];
    intel_config_make_mask(mask, slot, mmio_base, true);
    return intel_config_test_mask(in, mask);
}

/*bool:intel_config_is_disabled(in[IntelConfig]) {
    new mask[IntelConfig];
    intel_config_make_mask(mask, 0, 0, false);
    return intel_config_test_mask(in, mask);
}*/

MMIOState:intel_config_to_state(in[IntelConfig], base, base2) {
    new bool:enabled_2e = base && intel_config_is_enabled(in, 0, base);
    new bool:enabled_4e = base2 && intel_config_is_enabled(in, 1, base2);
    switch (_:enabled_2e << 1 | _:enabled_4e) {
        case 0: return MMIO_Original;
        case 1: return MMIO_Enabled4E;
        case 2: return MMIO_Enabled2E;
        case 3: return MMIO_EnabledBoth;
    }
    return MMIO_Original;
}

new g_intel_original_config[IntelConfig];
new bool:g_intel_original_config_saved = false;
new bool:g_intel_should_restore = false;

NTSTATUS:intel_ensure_config_saved() {
    if (g_intel_original_config_saved)
        return STATUS_SUCCESS;
    new NTSTATUS:status = intel_get_config(g_intel_original_config);
    if (NT_SUCCESS(status))
        g_intel_original_config_saved = true;
    return status;
}

NTSTATUS:intel_restore_config() {
    if (!g_intel_original_config_saved)
        return STATUS_SUCCESS;
    new NTSTATUS:status = intel_set_config(g_intel_original_config);
    if (NT_SUCCESS(status))
        g_intel_should_restore = false;
    return status;
}

NTSTATUS:intel_get_original_state(&MMIOState:mmio_state) {
    new NTSTATUS:status = intel_ensure_config_saved();
    if (!NT_SUCCESS(status))
        return status;

    mmio_state = intel_config_to_state(g_intel_original_config, g_superio_base, g_superio_2_base);
    return STATUS_SUCCESS;
}

NTSTATUS:intel_get_current_state(&MMIOState:mmio_state) {
    new config[IntelConfig];
    new NTSTATUS:status = intel_get_config(config);
    if (!NT_SUCCESS(status))
        return status;

    mmio_state = intel_config_to_state(config, g_superio_base, g_superio_2_base);
    return STATUS_SUCCESS;
}

NTSTATUS:intel_set_state(MMIOState:mmio_state) {
    if (mmio_state == MMIO_Original)
        return intel_restore_config();

    new NTSTATUS:status = intel_ensure_config_saved();
    if (!NT_SUCCESS(status))
        return status;

    new config[IntelConfig];
    status = intel_get_config(config);
    if (!NT_SUCCESS(status))
        return status;

    new target[IntelConfig];
    if (mmio_state == MMIO_Disabled) {
        intel_config_make_mask(target, 0, 0, false);
        intel_config_apply_mask(target, config, target);
    } else if (mmio_state == MMIO_Enabled2E) {
        if (g_superio_base == 0)
            return STATUS_INVALID_PARAMETER;
        intel_config_make_mask(target, 0, g_superio_base, true);
        intel_config_apply_mask(target, config, target);
    } else if (mmio_state == MMIO_Enabled4E) {
        if (g_superio_2_base == 0)
            return STATUS_INVALID_PARAMETER;
        intel_config_make_mask(target, 1, g_superio_2_base, true);
        intel_config_apply_mask(target, config, target);
    } else if (mmio_state == MMIO_EnabledBoth) {
        if (g_superio_base == 0 || g_superio_2_base == 0)
            return STATUS_INVALID_PARAMETER;
        intel_config_make_mask(target, 0, g_superio_base, true);
        intel_config_apply_mask(target, config, target);
        new second_mask[IntelConfig];
        intel_config_make_mask(second_mask, 1, g_superio_2_base, true);
        intel_config_apply_mask(target, target, second_mask);
    } else {
        return STATUS_INVALID_PARAMETER;
    }

    status = intel_set_config(target);
    if (NT_SUCCESS(status) && !intel_config_is_equal(target, g_intel_original_config))
        g_intel_should_restore = true;
    return status;
}

NTSTATUS:intel_test_support() {
    new didvid;
    new NTSTATUS:status = pci_config_read_dword(0x0, 0x1F, 0x0, 0, didvid);
    if (!NT_SUCCESS(status))
        return status;

    // Something Intel
    if ((didvid & 0xFFFF) != 0x8086)
        return STATUS_NOT_SUPPORTED;

    new classrev;
    status = pci_config_read_dword(0x0, 0x1F, 0x0, 8, classrev);
    if (!NT_SUCCESS(status))
        return status;
    
    // ISA bridge
    if (classrev >> 8 != 0x060100)
        return STATUS_NOT_SUPPORTED;
    
    return STATUS_SUCCESS;
}

// =============================================
// AMD/Intel wrappers
// =============================================

NTSTATUS:test_support() {
    if (!have_superio_mmio())
        return STATUS_NOT_SUPPORTED;
    return g_is_intel ? intel_test_support() : amd_test_support();
}

NTSTATUS:get_original_state(&MMIOState:mmio_state) {
    return g_is_intel ? intel_get_original_state(mmio_state) : amd_get_original_state(mmio_state);
}

NTSTATUS:get_current_state(&MMIOState:mmio_state) {
    return g_is_intel ? intel_get_current_state(mmio_state) : amd_get_current_state(mmio_state);
}

NTSTATUS:set_state(MMIOState:mmio_state) {
    return g_is_intel ? intel_set_state(mmio_state) : amd_set_state(mmio_state);
}

NTSTATUS:restore_config_if_needed() {
    if (g_is_intel) {
        if (g_intel_should_restore)
            return intel_restore_config();
    } else {
        if (g_amd_should_restore)
            return amd_restore_config();
    }
    return STATUS_SUCCESS;
}

/// Get original state of MMIO configuration.
///
/// @param out [0] = Original state
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_iomem_mmio_get_org_state, 0, 1) {
    new NTSTATUS:status = test_support();
    if (!NT_SUCCESS(status))
        return status;

    return get_original_state(MMIOState:out[0]);
}

/// Get current state of MMIO configuration.
///
/// @param out [0] = Current state
/// @param out_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_iomem_mmio_get_cur_state, 0, 1) {
    new NTSTATUS:status = test_support();
    if (!NT_SUCCESS(status))
        return status;

    return get_current_state(MMIOState:out[0]);
}

/// Set state of MMIO configuration.
///
/// @param in [0] = New state
/// @param in_size Must be 1
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_iomem_mmio_set_state, 1, 0) {
    new NTSTATUS:status = test_support();
    if (!NT_SUCCESS(status))
        return status;

    return set_state(MMIOState:in[0]);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;
    
    switch (get_cpu_vendor()) {
        case CpuVendor_AMD:
            g_is_intel = false;
        case CpuVendor_Intel:
            g_is_intel = true;
        default:
            return STATUS_NOT_SUPPORTED;
    }

    return STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    // Ignore result
    restore_config_if_needed();
    unmap_superio_mmio();
    return STATUS_SUCCESS;
}