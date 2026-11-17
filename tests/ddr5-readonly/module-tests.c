// SPDX-License-Identifier: LGPL-2.1-or-later
// Copyright (C) 2026 Hardware Tray contributors.
// Executes the UNMODIFIED compiled production module in a user-mode Pawn VM.
// All hardware natives below are mocks. No Windows driver or hardware access.
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "amx.h"
#include "amxaux.h"

static int native_calls, writes, started, starts, initial_busy, force_timeout, decode_enabled = 1, cases;
static cell bus_address, command, control, ticks;
static void require(int ok, const char *message) {
    if (!ok) { fprintf(stderr, "FAIL %s (case %d)\n", message, cases); exit(1); }
}
static cell AMX_NATIVE_CALL pci_read(AMX *vm, const cell *p) {
    native_calls++;
    require(p[1] == 0 && p[2] == 31 && (p[3] == 3 || p[3] == 4), "unexpected PCI location");
    cell value = 0;
    switch (p[4]) {
        case 0: value = 0x8086; break;
        case 0x0a: value = 0x0c05; break;
        case 4: value = decode_enabled ? 2 : 0; break;
        case 0x40: value = 1; break;
        case 0x10: value = 0x100000; break;
        default: require(0, "unexpected PCI register");
    }
    *amx_Address(vm, p[5]) = value;
    return 0;
}
static cell AMX_NATIVE_CALL map_io(AMX *vm, const cell *p) {
    (void)vm; native_calls++;
    require(p[1] == 0x100000 && p[2] == 0x18, "unexpected mapping");
    return 0x200000;
}
static cell AMX_NATIVE_CALL unmap_io(AMX *vm, const cell *p) {
    (void)vm; native_calls++; require(p[1] == 0x200000 && p[2] == 0x18, "unexpected unmap"); return 0;
}
static cell AMX_NATIVE_CALL read_io(AMX *vm, const cell *p) {
    native_calls++;
    cell value = 0;
    switch (p[1] - 0x200000) {
        case 0: value = initial_busy ? initial_busy : started ? (force_timeout ? 1 : 2) : 0; break;
        case 2: value = control; break;
        case 13: value = 0; break;
        case 5: value = command == 0x31 ? 0x360 : 0; break;
        default: require(0, "unexpected MMIO read");
    }
    *amx_Address(vm, p[2]) = value;
    return 0;
}
static cell AMX_NATIVE_CALL write_io(AMX *vm, const cell *p) {
    (void)vm; native_calls++; writes++;
    switch (p[1] - 0x200000) {
        case 0: require((p[2] & ~0xde) == 0, "unexpected host status write"); break;
        case 2:
            control = p[2];
            require(control == 0 || control == 2 || control == 0x48 || control == 0x4c,
                    "unexpected host transaction type");
            if (control & 0x40) {
                require((bus_address & 1) == 1, "PERIPHERAL WRITE transaction attempted");
                require(bus_address >= 0xa1 && bus_address <= 0xaf, "address escaped DIMM range");
                started = 1; starts++;
            } else started = 0;
            break;
        case 3:
            command = p[2];
            require(command == 0 || command == 3 || command == 5 || command == 0x0b ||
                    command == 0x1a || command == 0x31, "register escaped allowlist");
            break;
        case 4:
            bus_address = p[2];
            require((bus_address & 1) == 1, "write direction reached hardware native");
            break;
        default: require(0, "MMIO write outside controller command/handshake registers");
    }
    return 0;
}
static cell AMX_NATIVE_CALL shared_tick(AMX *vm, const cell *p) {
    native_calls++;
    if ((ucell)p[1] == 0xfffff78000000320ULL) *amx_Address(vm, p[2]) = ticks++;
    else if ((ucell)p[1] == 0xfffff78000000004ULL) *amx_Address(vm, p[2]) = 1 << 24;
    else require(0, "unexpected shared data address");
    return 0;
}
static cell AMX_NATIVE_CALL pause_us(AMX *vm, const cell *p) {
    (void)vm; native_calls++; require(p[1] >= 0 && p[1] <= 1000, "unbounded wait"); return 0;
}
static const AMX_NATIVE_INFO natives[] = {
    {"pci_config_read_word", pci_read}, {"pci_config_read_byte", pci_read}, {"pci_config_read_qword", pci_read},
    {"io_space_map", map_io}, {"io_space_unmap", unmap_io},
    {"virtual_read_byte", read_io}, {"virtual_read_word", read_io}, {"virtual_write_byte", write_io},
    {"virtual_read_qword", shared_tick}, {"virtual_read_dword", shared_tick},
    {"microsleep", pause_us}, {NULL, NULL}
};
static void call(AMX *vm, int function, cell address, cell direction, cell reg, cell protocol,
                 cell in_size, cell out_size, int allowed) {
    cell input[5] = {address, direction, reg, protocol, 0x1234}, output[2] = {0, 0}, result;
    cell *out_ptr;
    native_calls = writes = starts = 0;
    require(amx_Push(vm, out_size) == 0, "push output length");
    require(amx_PushArray(vm, &out_ptr, output, 2) == 0, "push output");
    require(amx_Push(vm, in_size) == 0, "push input length");
    require(amx_PushArray(vm, NULL, input, 5) == 0, "push input");
    require(amx_Exec(vm, &result, function) == 0, "VM execution");
    cases++;
    if (allowed == 1) require(result == 0 && starts == 1, "allowlisted read did not execute once");
    else if (allowed == -1) require(result < 0 && writes == 0, "unavailable controller was changed");
    else if (allowed == -2) require(result < 0 && starts == 1 && native_calls < 4000,
                                    "own transaction timeout did not terminate within bounds");
    else if (allowed == -3) require(result < 0 && writes == 1 && starts == 0,
                                    "busy transaction was disturbed instead of releasing only our claim");
    else {
        require(result < 0, "forbidden request accepted");
        require(native_calls == 0, "forbidden request touched a hardware native");
    }
    require(amx_Release(vm, out_ptr) == 0, "release VM buffers");
}
int main(int argc, char **argv) {
    require(argc == 2, "supply compiled module path");
    AMX vm; int function, count; cell result;
    require(aux_LoadProgram(&vm, argv[1], NULL) == 0, "load compiled AMX");
    require(amx_NumNatives(&vm, &count) == 0, "native inventory");
    for (int i = 0; i < count; i++) {
        char name[128]; int found = 0;
        require(amx_GetNative(&vm, i, name) == 0, "native name");
        for (int j = 0; natives[j].name; j++) if (!strcmp(name, natives[j].name)) found = 1;
        if (!found) fprintf(stderr, "Unexpected native: %s\n", name);
        require(found, "module imports unexpected native capability");
    }
    require(amx_NumPublics(&vm, &count) == 0 && count == 2, "unexpected exported interface");
    for (int i = 0; i < count; i++) {
        char name[128]; ucell address;
        require(amx_GetPublic(&vm, i, name, &address) == 0, "public name");
        require(!strcmp(name, "ioctl_ddr5_read") || !strcmp(name, "unload"), "unexpected export");
    }
    require(amx_Register(&vm, natives, -1) == 0, "bind mocks only");
    require(amx_Exec(&vm, &result, AMX_EXEC_MAIN) == 0 && result == 0, "module initialization");
    require(amx_FindPublic(&vm, "ioctl_ddr5_read", &function) == 0, "read interface missing");
    for (int address = 0; address < 128; address++)
        for (int reg = 0; reg < 256; reg++) {
            int word = reg == 0 || reg == 3 || reg == 0x31;
            int byte = reg == 5 || reg == 0x0b || reg == 0x1a;
            int allowed = address >= 0x50 && address <= 0x57 && (word || byte);
            call(&vm, function, address, 1, reg, word ? 3 : 2, 4, 1, allowed);
            call(&vm, function, address, 0, reg, word ? 3 : 2, 4, 1, 0);
        }
    cell extremes[] = {-1, 2, 0x100000001LL, INT64_MIN, INT64_MAX};
    for (unsigned i = 0; i < sizeof extremes / sizeof extremes[0]; i++) {
        call(&vm, function, 0x50, extremes[i], 0x31, 3, 4, 1, 0);
        call(&vm, function, extremes[i], 1, 0x31, 3, 4, 1, 0);
        call(&vm, function, 0x50, 1, extremes[i], 3, 4, 1, 0);
    }
    for (int size = -1; size <= 9; size++) {
        if (size != 4) call(&vm, function, 0x50, 1, 0x31, 3, size, 1, 0);
        if (size != 1) call(&vm, function, 0x50, 1, 0x31, 3, 4, size, 0);
        if (size != 3) call(&vm, function, 0x50, 1, 0x31, size, 4, 1, 0);
    }
    initial_busy = 0x41;
    call(&vm, function, 0x50, 1, 0x31, 3, 4, 1, -1);
    initial_busy = 1;
    call(&vm, function, 0x50, 1, 0x31, 3, 4, 1, -3);
    initial_busy = 0; decode_enabled = 0;
    call(&vm, function, 0x50, 1, 0x31, 3, 4, 1, -1);
    decode_enabled = 1; force_timeout = 1;
    call(&vm, function, 0x50, 1, 0x31, 3, 4, 1, -2);
    force_timeout = 0;
    require(amx_FindPublic(&vm, "ioctl_smbus_xfer", &function) != 0, "generic SMBus interface exists");
    require(amx_FindPublic(&vm, "unload", &function) == 0, "unload missing");
    require(amx_Exec(&vm, &result, function) == 0 && result == 0, "unload failed");
    aux_FreeProgram(&vm);
    printf("PASS %d compiled-module request cases; no forbidden request reached a hardware native.\n", cases);
    puts("PASS export/native allowlists. User-mode mocked hardware only; not a kernel/hardware acceptance test.");
    return 0;
}
