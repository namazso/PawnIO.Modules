// Fixed DDR5 observation module; research candidate, never a tuning API.
// i801 register definitions and controller approach derived from PawnIO.Modules 0.2.11.
// Copyright (C) 2025 Steve-Tech <me@stevetech.au>
// SPDX-License-Identifier: LGPL-2.1-or-later
// Distributed without warranty; see COPYING and the pinned corresponding source.
#include <pawnio.inc>

#define MAGIC 0x004d435244445235
#define REPLY_WORDS 32
#define STS 0
#define CNT 2
#define CMD 3
#define ADDR 4
#define DATA0 5
#define DATA1 6
#define AUX 13
#define BUSY 0x01
#define INTR 0x02
#define DEV_ERR 0x04
#define BUS_ERR 0x08
#define FAILED 0x10
#define INUSE 0x40
#define BYTE_DONE 0x80
#define OWN_FLAGS 0x9e
#define START 0x40
#define KILL 0x02

new VA:g_map;
new g_bus;
new g_ids;
new g_subsystem;
new g_pa;
new g_sequence;
new bool:g_poison;
new bool:g_active;
new bool:g_acquired;
new bool:g_saved_valid;
new bool:g_dirty;
new bool:g_started;
new bool:g_recovering;
new g_saved[6];
new g_registers[6] = [CNT, CMD, ADDR, DATA0, DATA1, AUX];
new NTSTATUS:g_cleanup;
new g_stage;
new g_deadline;

NTSTATUS:now_ms(&milliseconds) {
    new ticks, multiplier;
    new NTSTATUS:status = virtual_read_qword(KUSER_SHARED_DATA + 0x0320, ticks);
    if (!NT_SUCCESS(status)) return status;
    status = virtual_read_dword(KUSER_SHARED_DATA + 0x0004, multiplier);
    if (!NT_SUCCESS(status)) return status;
    if (ticks < 0 || multiplier <= 0 || multiplier > 0xffffffff || ticks > cellmax / multiplier)
        return STATUS_DATA_ERROR;
    milliseconds = (ticks * multiplier) >>> 24;
    return STATUS_SUCCESS;
}

NTSTATUS:rd(reg, &value) {
    new NTSTATUS:status = virtual_read_byte(g_map + reg, value);
    if (!NT_SUCCESS(status)) return status;
    if (value < 0 || value > 255) return STATUS_DATA_ERROR;
    return STATUS_SUCCESS;
}

NTSTATUS:wr(reg, value) {
    if (value < 0 || value > 255) return STATUS_INVALID_PARAMETER;
    return virtual_write_byte(g_map + reg, value);
}

cleanup_error(NTSTATUS:status) {
    if (!NT_SUCCESS(status) && NT_SUCCESS(g_cleanup)) g_cleanup = status;
}

NTSTATUS:read_bar(bus, &base) {
    new raw;
    new NTSTATUS:status = pci_config_read_qword(bus, 31, 4, 0x10, raw);
    if (!NT_SUCCESS(status)) return status;
    if (raw & 1) return STATUS_NOT_SUPPORTED;
    if ((raw & 6) == 0) raw &= 0xffffffff;
    else if ((raw & 6) != 4) return STATUS_NOT_SUPPORTED;
    base = raw & 0xffffffffffffff00;
    if (base == 0) return STATUS_NOT_SUPPORTED;
    return STATUS_SUCCESS;
}

NTSTATUS:controller_identity() {
    new value;
    new NTSTATUS:status = pci_config_read_dword(g_bus, 31, 4, 0, value);
    if (!NT_SUCCESS(status)) return status;
    if (value != g_ids) return STATUS_DEVICE_CONFIGURATION_ERROR;
    status = pci_config_read_dword(g_bus, 31, 4, 0x2c, value);
    if (!NT_SUCCESS(status)) return status;
    if (value != g_subsystem) return STATUS_DEVICE_CONFIGURATION_ERROR;
    status = pci_config_read_word(g_bus, 31, 4, 4, value);
    if (!NT_SUCCESS(status)) return status;
    // Never enable PCI decode as a hidden side effect of a sensor read.
    if (!(value & 2)) return STATUS_NOT_SUPPORTED;
    status = read_bar(g_bus, value);
    if (!NT_SUCCESS(status)) return status;
    if (value != g_pa) return STATUS_DEVICE_CONFIGURATION_ERROR;
    return STATUS_SUCCESS;
}

snapshot(values[6]) {
    new packed;
    for (new i; i < 6; i++) packed |= values[i] << (i * 8);
    return packed;
}

NTSTATUS:begin_controller() {
    new hststs;
    new NTSTATUS:status = rd(STS, hststs);
    if (!NT_SUCCESS(status)) return status;
    if (hststs & INUSE) return STATUS_DEVICE_BUSY;
    // Reading a clear INUSE bit acquires the hardware semaphore.
    g_acquired = true;
    if (hststs & (BUSY | OWN_FLAGS)) return STATUS_DEVICE_BUSY;
    for (new i; i < 6; i++) {
        status = rd(g_registers[i], g_saved[i]);
        if (!NT_SUCCESS(status)) return status;
    }
    g_saved_valid = true;
    if (g_saved[0] & (START | KILL)) return STATUS_DEVICE_BUSY;
    g_dirty = true; // A failed write may still have reached the controller.
    status = wr(AUX, g_saved[5] & ~3);
    if (!NT_SUCCESS(status)) return status;
    status = wr(CNT, g_saved[0] & ~1);
    if (!NT_SUCCESS(status)) return status;
    new readback;
    status = rd(AUX, readback);
    if (!NT_SUCCESS(status)) return status;
    if (readback != (g_saved[5] & ~3)) return STATUS_DEVICE_PROTOCOL_ERROR;
    return STATUS_SUCCESS;
}

NTSTATUS:stop_own_transaction() {
    new hststs;
    new NTSTATUS:status = rd(STS, hststs);
    if (!NT_SUCCESS(status)) return status;
    if (!(hststs & BUSY)) {
        if (!g_started) return STATUS_SUCCESS;
        // An earlier status read can fail after START while the bus later completes.
        // Clear only our terminal flags before restoring/releasing the controller.
        status = wr(STS, hststs & OWN_FLAGS);
        if (NT_SUCCESS(status)) g_started = false;
        return status;
    }
    if (!g_started) return STATUS_DEVICE_BUSY;
    if (hststs & BYTE_DONE) {
        status = wr(STS, BYTE_DONE);
        if (!NT_SUCCESS(status)) return status;
    }
    status = wr(CNT, KILL);
    if (!NT_SUCCESS(status)) return status;
    new NTSTATUS:waited = microsleep(1000);
    status = wr(CNT, 0);
    if (!NT_SUCCESS(status)) return status;
    if (!NT_SUCCESS(waited)) return waited;
    status = rd(STS, hststs);
    if (!NT_SUCCESS(status)) return status;
    if (hststs & BUSY) return STATUS_DEVICE_BUSY;
    status = wr(STS, hststs & OWN_FLAGS);
    if (!NT_SUCCESS(status)) return status;
    g_started = false;
    return STATUS_SUCCESS;
}

NTSTATUS:byte_transfer(address, reg, bool:write, &value, bool:word=false) {
    // The only word transaction is a fixed read of the SPD5118 temperature pair.
    if (word && (write || address < 0x50 || address > 0x57 || reg != 0x31))
        return STATUS_ACCESS_DENIED;
    new now;
    new NTSTATUS:status = now_ms(now);
    if (!NT_SUCCESS(status)) return status;
    if (!g_recovering && now >= g_deadline) return STATUS_IO_TIMEOUT;
    new hststs;
    status = rd(STS, hststs);
    if (!NT_SUCCESS(status)) return status;
    if (hststs & (BUSY | OWN_FLAGS)) return STATUS_DEVICE_BUSY;
    status = wr(CMD, reg);
    if (!NT_SUCCESS(status)) return status;
    status = wr(ADDR, (address << 1) | (write ? 0 : 1));
    if (!NT_SUCCESS(status)) return status;
    if (write) {
        status = wr(DATA0, value);
        if (!NT_SUCCESS(status)) return status;
    }
    g_started = true;
    status = wr(CNT, (word ? 0x0c : 0x08) | START);
    if (!NT_SUCCESS(status)) return status;
    status = now_ms(now);
    if (!NT_SUCCESS(status)) return status;
    new deadline = now + 80;
    new bool:completed;
    for (new poll; poll < 320; poll++) {
        status = rd(STS, hststs);
        if (!NT_SUCCESS(status)) return status;
        status = now_ms(now);
        if (!NT_SUCCESS(status)) return status;
        if (now >= deadline) return STATUS_IO_TIMEOUT;
        if (!(hststs & BUSY) && (hststs & (INTR | DEV_ERR | BUS_ERR | FAILED))) {completed=true;break;}
        status = microsleep(250);
        if (!NT_SUCCESS(status)) return status;
    }
    if (!completed) return STATUS_IO_TIMEOUT;
    g_started = false;
    if (hststs & DEV_ERR) status = STATUS_DEVICE_NOT_CONNECTED;
    else if (hststs & (BUS_ERR | FAILED)) status = STATUS_IO_DEVICE_ERROR;
    else if (!write) {
        status = rd(DATA0, value);
        if (NT_SUCCESS(status) && word) {
            new high;
            status = rd(DATA1, high);
            if (NT_SUCCESS(status)) value |= high << 8;
        }
    }
    new NTSTATUS:cleared = wr(STS, hststs & OWN_FLAGS);
    cleanup_error(cleared);
    if (NT_SUCCESS(status) && !NT_SUCCESS(cleared)) status = cleared;
    return status;
}

NTSTATUS:read_byte(address, reg, &value) {
    return byte_transfer(address, reg, false, value);
}

NTSTATUS:write_selector(address, reg, value) {
    // Only two observational selectors are writable; no VID, VR enable or EEPROM.
    if (!((address >= 0x50 && address <= 0x57 && reg == 0x0b) ||
          (address >= 0x48 && address <= 0x4f && reg == 0x30))) return STATUS_ACCESS_DENIED;
    return byte_transfer(address, reg, true, value);
}

NTSTATUS:restore_selector(address, reg, original) {
    g_recovering = true;
    new NTSTATUS:status = stop_own_transaction();
    if (!NT_SUCCESS(status)) return status;
    new NTSTATUS:written = write_selector(address, reg, original);
    new readback;
    status = read_byte(address, reg, readback);
    // A failed restore write is retained even if a later read happens to match.
    if (!NT_SUCCESS(written)) return written;
    if (!NT_SUCCESS(status)) return status;
    return readback == original ? STATUS_SUCCESS : STATUS_DEVICE_PROTOCOL_ERROR;
}

end_controller(&after) {
    if (g_dirty && g_saved_valid) {
        new NTSTATUS:status = stop_own_transaction();
        cleanup_error(status);
        if (NT_SUCCESS(status)) {
            // Restore non-triggering data first; control/interrupt hststs last.
            new order[6] = [1,2,3,4,5,0];
            for (new i; i < 6; i++) {
                new index = order[i];
                status = wr(g_registers[index], g_saved[index]);
                cleanup_error(status);
            }
            new observed[6];
            for (new i; i < 6; i++) {
                status = rd(g_registers[i], observed[i]);
                cleanup_error(status);
                if (NT_SUCCESS(status) && observed[i] != g_saved[i]) cleanup_error(STATUS_DEVICE_PROTOCOL_ERROR);
            }
            after = snapshot(observed);
        }
    } else if (g_saved_valid) after = snapshot(g_saved);
    if (g_acquired && NT_SUCCESS(g_cleanup)) {
        // Do not read INUSE after the final release: that would reacquire it.
        new NTSTATUS:released = wr(STS, INUSE);
        cleanup_error(released);
        if (NT_SUCCESS(released)) g_acquired = false;
    }
    if (!NT_SUCCESS(g_cleanup)) g_poison = true;
}

NTSTATUS:hub_identity(slot, identity[5]) {
    g_stage = 10;
    new NTSTATUS:status = read_byte(0x50+slot, 0, identity[0]);
    if (!NT_SUCCESS(status)) return status;
    g_stage = 11;
    status = read_byte(0x50+slot, 1, identity[1]);
    if (!NT_SUCCESS(status)) return status;
    if (identity[0] != 0x51 || identity[1] != 0x18) return STATUS_NOT_SUPPORTED;
    g_stage = 11;
    for (new i=2; i < 5; i++) {
        status = read_byte(0x50+slot, i, identity[i]);
        if (!NT_SUCCESS(status)) return status;
    }
    return STATUS_SUCCESS;
}

NTSTATUS:pmic_identity(slot, identity[3], &enabled) {
    g_stage = 20;
    for (new i; i < 3; i++) {
        new NTSTATUS:status = read_byte(0x48+slot, 0x3b+i, identity[i]);
        if (!NT_SUCCESS(status)) return status;
    }
    return read_byte(0x48+slot, 0x32, enabled);
}

put_byte(out[REPLY_WORDS], offset, value) {
    out[16 + offset / 8] |= (value & 255) << ((offset % 8) * 8);
}

NTSTATUS:observe_identity(slot, out[REPLY_WORDS]) {
    new hub[5], pmic[3], enabled, original;
    new NTSTATUS:status = hub_identity(slot, hub);
    if (!NT_SUCCESS(status)) return status;
    status = pmic_identity(slot, pmic, enabled);
    if (!NT_SUCCESS(status)) return status;
    status = read_byte(0x50+slot, 0x0b, original);
    if (!NT_SUCCESS(status)) return status;
    for (new i; i < 5; i++) put_byte(out, i, hub[i]);
    for (new i; i < 3; i++) put_byte(out, 5+i, pmic[i]);
    put_byte(out, 8, enabled); put_byte(out, 9, original);
    out[14] = 16;
    return STATUS_SUCCESS;
}

NTSTATUS:observe_page(slot, page, out[REPLY_WORDS]) {
    new hub[5], original;
    new NTSTATUS:status = hub_identity(slot, hub);
    if (!NT_SUCCESS(status)) return status;
    g_stage = 30;
    status = read_byte(0x50+slot, 0x0b, original);
    if (!NT_SUCCESS(status)) return status;
    new desired = (original & ~15) | page;
    new bool:changed = desired != original;
    if (changed) status = write_selector(0x50+slot, 0x0b, desired);
    if (NT_SUCCESS(status)) {
        new observed;
        status = read_byte(0x50+slot, 0x0b, observed);
        if (NT_SUCCESS(status) && observed != desired) status = STATUS_DEVICE_PROTOCOL_ERROR;
    }
    g_stage = 31;
    for (new i; i < 128 && NT_SUCCESS(status); i++) {
        new value;
        status = read_byte(0x50+slot, 0x80+i, value);
        if (NT_SUCCESS(status)) put_byte(out, i, value);
    }
    if (changed) cleanup_error(restore_selector(0x50+slot, 0x0b, original));
    else {
        new final_page;
        new NTSTATUS:verified = read_byte(0x50+slot, 0x0b, final_page);
        cleanup_error(verified);
        if (NT_SUCCESS(verified) && final_page != original) cleanup_error(STATUS_DEVICE_PROTOCOL_ERROR);
    }
    out[14] = 128;
    return status;
}

NTSTATUS:observe_adc(slot, selector, out[REPLY_WORDS]) {
    new hub[5], pmic[3], enabled, original;
    new NTSTATUS:status = hub_identity(slot, hub);
    if (!NT_SUCCESS(status)) return status;
    status = pmic_identity(slot, pmic, enabled);
    if (!NT_SUCCESS(status)) return status;
    if ((pmic[1] & 127) != 10 || (pmic[2] & 127) != 12 || !(enabled & 0x80)) return STATUS_NOT_SUPPORTED;
    g_stage = 40;
    status = read_byte(0x48+slot, 0x30, original);
    if (!NT_SUCCESS(status)) return status;
    if (original & 4) return STATUS_NOT_SUPPORTED;
    new desired = (original & 3) | 0x80 | (selector << 3);
    // Restoration is required even when the acquisition write reports failure.
    status = write_selector(0x48+slot, 0x30, desired);
    new before, value, after;
    if (NT_SUCCESS(status)) {
        status = microsleep(9000);
        if (NT_SUCCESS(status)) status = read_byte(0x48+slot, 0x30, before);
        if (NT_SUCCESS(status) && before != desired) status = STATUS_DEVICE_PROTOCOL_ERROR;
    }
    if (NT_SUCCESS(status)) status = read_byte(0x48+slot, 0x31, value);
    if (NT_SUCCESS(status)) {
        status = read_byte(0x48+slot, 0x30, after);
        if (NT_SUCCESS(status) && after != desired) status = STATUS_DEVICE_PROTOCOL_ERROR;
    }
    cleanup_error(restore_selector(0x48+slot, 0x30, original));
    put_byte(out,0,original);put_byte(out,1,before);put_byte(out,2,value);
    put_byte(out,3,after);put_byte(out,4,original);
    for (new i; i < 3; i++) put_byte(out,5+i,pmic[i]);
    out[14] = 8;
    return status;
}

NTSTATUS:temperature_payload(slot, hub[5], out[REPLY_WORDS]) {
    new capability, config, value, capability_after, config_after, after[5];
    g_stage = 50;
    new NTSTATUS:status = read_byte(0x50+slot, 0x05, capability);
    if (!NT_SUCCESS(status)) return status;
    status = read_byte(0x50+slot, 0x1A, config);
    if (!NT_SUCCESS(status)) return status;
    // JESD300/ SPD5118: only capability bits 0..1 and temperature-disable bit 0
    // are defined. Never enable a disabled sensor or clear its alarms here.
    if (capability & ~3 || config & ~1) return STATUS_DEVICE_PROTOCOL_ERROR;
    new bool:present = (capability & 2) != 0 && (config & 1) == 0;
    if (present) {
        status = byte_transfer(0x50+slot, 0x31, false, value, true);
        if (!NT_SUCCESS(status)) return status;
    }
    status = read_byte(0x50+slot, 0x05, capability_after);
    if (!NT_SUCCESS(status)) return status;
    status = read_byte(0x50+slot, 0x1A, config_after);
    if (!NT_SUCCESS(status)) return status;
    status = hub_identity(slot, after);
    g_stage = 51;
    if (!NT_SUCCESS(status)) return status;
    if (capability_after != capability || config_after != config) return STATUS_DEVICE_CONFIGURATION_ERROR;
    for (new i; i < 5; i++) {
        if (after[i] != hub[i]) return STATUS_DEVICE_CONFIGURATION_ERROR;
        put_byte(out, i, hub[i]);
        put_byte(out, 14+i, after[i]);
    }
    put_byte(out, 5, capability); put_byte(out, 6, config); put_byte(out, 7, _:present);
    put_byte(out, 8, value); put_byte(out, 9, value >>> 8);
    put_byte(out, 10, capability_after); put_byte(out, 11, config_after);
    out[14] = 24;
    return STATUS_SUCCESS;
}

NTSTATUS:observe_temperature(slot, out[REPLY_WORDS]) {
    new hub[5], original;
    new NTSTATUS:status = hub_identity(slot, hub);
    if (!NT_SUCCESS(status)) return status;
    status = read_byte(0x50+slot, 0x0b, original);
    if (!NT_SUCCESS(status)) return status;
    new desired = original & ~15;
    new bool:changed = desired != original;
    g_stage = 50;
    if (changed) status = write_selector(0x50+slot, 0x0b, desired);
    if (NT_SUCCESS(status)) {
        new selected;
        status = read_byte(0x50+slot, 0x0b, selected);
        if (NT_SUCCESS(status) && selected != desired) status = STATUS_DEVICE_PROTOCOL_ERROR;
    }
    if (NT_SUCCESS(status)) status = temperature_payload(slot, hub, out);
    if (changed) cleanup_error(restore_selector(0x50+slot, 0x0b, original));
    new restored;
    new NTSTATUS:verified = read_byte(0x50+slot, 0x0b, restored);
    cleanup_error(verified);
    if (NT_SUCCESS(verified) && restored != original) cleanup_error(STATUS_DEVICE_PROTOCOL_ERROR);
    put_byte(out, 12, original); put_byte(out, 13, restored);
    return status;
}

/// Observe one fixed Intel i801 DDR5 operation and its cleanup receipt.
///
/// Callers must hold Access_SMBUS.HTP.Method for their whole observation session.
/// Operations 1-4 issue SMBus transactions and may temporarily select MR11/R30;
/// restoration errors are retained independently of the acquisition status.
/// No voltage setting, EEPROM write, sensor enabling, or arbitrary address API.
///
/// @param in [0] sequence (starts at 1; increments once per accepted request),
///           [1] operation (0 controller, 1 identity, 2 SPD page, 3 PMIC ADC,
///           4 SPD5118 temperature), [2] slot (0..7), [3] page/ADC selector or 0.
/// @param in_size Must be 4 cells.
/// @param out A 32-cell schema-2 receipt. [6] primary NTSTATUS, [7] cleanup
///            NTSTATUS, [8] cleanup-confirmed, [14] payload length in bytes;
///            [16..31] packed payload. See tests/intel-memory/README.md.
/// @param out_size Must be 32 cells (256 bytes).
/// @return STATUS_SUCCESS means a receipt was returned, not measurement success.
///         Inspect both embedded statuses and cleanup-confirmed before use.
/// @note PMIC manufacturer/VR-enable checks do not establish the exact PMIC
///       model. Operation 3 still requires model qualification before release.
DEFINE_IOCTL_SIZED(ioctl_ddr5, 4, REPLY_WORDS) {
    new sequence=in[0], operation=in[1], slot=in[2], argument=in[3];
    if (g_active || g_poison || sequence != g_sequence+1 || sequence > 0xffffffff ||
        operation < 0 || operation > 4 || slot < 0 || slot > 7 || argument < 0 || argument > 9)
        return STATUS_INVALID_PARAMETER;
    if ((operation == 0 && (slot != 0 || argument != 0)) ||
        (operation == 1 && argument != 0) || (operation == 2 && argument > 7) ||
        (operation == 3 && argument != 0 && argument != 1 && argument != 2 &&
         argument != 3 && argument != 5 && argument != 8 && argument != 9))
        return STATUS_INVALID_PARAMETER;
    if (operation == 4 && argument != 0) return STATUS_INVALID_PARAMETER;
    for (new i; i < REPLY_WORDS; i++) out[i]=0;
    g_sequence=sequence;g_active=true;g_cleanup=STATUS_SUCCESS;g_stage=1;
    g_acquired=false;g_saved_valid=false;g_dirty=false;g_started=false;g_recovering=false;
    new now;
    new NTSTATUS:primary=now_ms(now);
    g_deadline=now+2500;
    if (NT_SUCCESS(primary)) primary=controller_identity();
    if (NT_SUCCESS(primary) && operation != 0) {
        primary=begin_controller();
        if (NT_SUCCESS(primary)) {
            switch (operation) {
                case 1: primary=observe_identity(slot,out);
                case 2: primary=observe_page(slot,argument,out);
                case 3: primary=observe_adc(slot,argument,out);
                case 4: primary=observe_temperature(slot,out);
            }
        }
    }
    if (g_saved_valid) out[12]=snapshot(g_saved);
    end_controller(out[13]);
    new NTSTATUS:identity_after=controller_identity();
    if (NT_SUCCESS(primary) && !NT_SUCCESS(identity_after)) primary=identity_after;
    new NTSTATUS:clock_after=now_ms(now);
    if (NT_SUCCESS(primary) && !NT_SUCCESS(clock_after)) primary=clock_after;
    if (NT_SUCCESS(primary) && now > g_deadline) primary=STATUS_IO_TIMEOUT;
    out[0]=MAGIC;out[1]=2;out[2]=sequence;out[3]=operation;out[4]=slot;out[5]=argument;
    out[6]=_:primary;out[7]=_:g_cleanup;out[8]=NT_SUCCESS(g_cleanup) ? 1 : 0;out[9]=g_stage;
    out[10]=g_ids | (g_subsystem << 32);out[11]=(g_bus << 16) | 0x1f04;
    if (!NT_SUCCESS(primary) || !NT_SUCCESS(g_cleanup)) {
        for (new i=16; i < REPLY_WORDS; i++) out[i]=0;
    }
    g_active=false;
    return STATUS_SUCCESS; // Transport success only; the reply carries both terminal statuses.
}

NTSTATUS:main() {
    if (get_arch()!=ARCH_X64 || get_cpu_vendor()!=CpuVendor_Intel) return STATUS_NOT_SUPPORTED;
    new found;
    for (new n; n < 2; n++) {
        new bus=n*128, ids;
        new NTSTATUS:status=pci_config_read_dword(bus,31,4,0,ids);
        if (!NT_SUCCESS(status)) {
            if (status==STATUS_NO_SUCH_DEVICE || status==STATUS_DEVICE_DOES_NOT_EXIST || status==STATUS_DEVICE_NOT_CONNECTED) continue;
            return status;
        }
        if (ids!=0x7f238086) continue;
        new class;
        status=pci_config_read_word(bus,31,4,0x0a,class);
        if (!NT_SUCCESS(status)) return status;
        if (class!=0x0c05 || found) return STATUS_DEVICE_CONFIGURATION_ERROR;
        g_bus=bus;g_ids=ids;found=1;
    }
    if (!found) return STATUS_NOT_SUPPORTED;
    new NTSTATUS:status=pci_config_read_dword(g_bus,31,4,0x2c,g_subsystem);
    if (!NT_SUCCESS(status)) return status;
    status=read_bar(g_bus,g_pa);
    if (!NT_SUCCESS(status)) return status;
    status=controller_identity();
    if (!NT_SUCCESS(status)) return status;
    new config;
    status=pci_config_read_byte(g_bus,31,4,0x40,config);
    if (!NT_SUCCESS(status)) return status;
    if (!(config & 1)) return STATUS_NOT_SUPPORTED;
    g_map=io_space_map(g_pa,256);
    return g_map==NULL ? STATUS_INSUFFICIENT_RESOURCES : STATUS_SUCCESS;
}

public NTSTATUS:unload() {
    if (g_active || g_acquired) return STATUS_DEVICE_BUSY;
    if (g_poison) return STATUS_DEVICE_CONFIGURATION_ERROR;
    if (g_map!=NULL) {io_space_unmap(g_map,256);g_map=NULL;}
    return STATUS_SUCCESS;
}
