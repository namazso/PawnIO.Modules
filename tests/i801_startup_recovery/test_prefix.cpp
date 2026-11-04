// SPDX-License-Identifier: LGPL-2.1-or-later
// Offline register model. This program imports no DLLs and has no hardware API.
using cell = long long;
using NTSTATUS = long long;
#define STATUS_SUCCESS 0
#define STATUS_ACCESS_DENIED -1
#define STATUS_NOT_SUPPORTED -2
#define STATUS_DEVICE_NOT_READY -3
#define STATUS_DEVICE_BUSY -4
#define STATUS_IO_DEVICE_ERROR -5
#define NT_SUCCESS(s) ((s) >= 0)
#define PCICMD 4
#define PCICMD_MMBIT 2
#define SMBHSTSTS 0
#define SMBHSTSTS_INUSE_STS 0x40
cell pci_addr[3] = {0, 31, 4};
cell vendor_device, subsystem, pci_command;
cell tick, reads, writes, last_write, pci_reads;
cell sequence[4], sequence_length, sequence_position;
bool pci_failure, read_failure, write_failure;
cell pci_config_read_dword(cell, cell, cell, cell offset, cell& out) {
    ++pci_reads;
    out = offset == 0 ? vendor_device : subsystem;
    return pci_failure ? STATUS_IO_DEVICE_ERROR : STATUS_SUCCESS;
}
cell pci_config_read_word(cell, cell, cell, cell, cell& out) {
    ++pci_reads;
    out = pci_command;
    return pci_failure ? STATUS_IO_DEVICE_ERROR : STATUS_SUCCESS;
}
cell virtual_read_byte(cell, cell& out) {
    ++reads;
    out = sequence[sequence_position];
    if (sequence_position + 1 < sequence_length) ++sequence_position;
    return read_failure ? STATUS_IO_DEVICE_ERROR : STATUS_SUCCESS;
}
cell virtual_write_byte(cell, cell value) {
    ++writes; last_write = value;
    return write_failure ? STATUS_IO_DEVICE_ERROR : STATUS_SUCCESS;
}
cell get_tick_count() { return tick; }
void microsleep(cell value) { tick += value / 1000; }
