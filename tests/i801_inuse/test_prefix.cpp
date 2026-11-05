// SPDX-License-Identifier: LGPL-2.1-or-later
// Offline model only. No headers, runtime libraries, driver or hardware APIs.
using cell = long long;
using NTSTATUS = cell;
#define BIT(n) (cell(1) << (n))
#define NT_SUCCESS(s) ((s) >= 0)
constexpr cell STATUS_SUCCESS=0, STATUS_DEVICE_BUSY=-1, STATUS_IO_TIMEOUT=-2;
constexpr cell STATUS_IO_DEVICE_ERROR=-3, STATUS_NOT_SUPPORTED=-4;
constexpr cell SMBHSTSTS=0, SMBAUXCTL=1, SMBAUXCTL_CRC=1;
constexpr cell I2C_SMBUS_QUICK=0, I2C_SMBUS_BYTE=1, I2C_SMBUS_BYTE_DATA=2;
constexpr cell I2C_SMBUS_WORD_DATA=3, I2C_SMBUS_PROC_CALL=4;
constexpr cell I2C_SMBUS_BLOCK_DATA=5, I2C_SMBUS_BLOCK_PROC_CALL=6, I2C_SMBUS_I2C_BLOCK_DATA=7;
cell status_byte, clock_us, release_at, status_reads, status_writes, last_status_write;
cell transaction_calls, kill_calls, control_reads, control_writes, first_transfer_us;
cell inject_busy_read, control_read_error, control_write_error, transfer_error;
cell status_before_first_write, pre_write_us, transaction_hststs;
cell get_tick_count() { return clock_us / 1000; }
void microsleep(cell us) { clock_us += us; }
template<class... T> void debug_print(const char*, T...) {}
NTSTATUS virtual_read_byte(cell reg, cell& out) {
    if (reg == SMBHSTSTS) {
        ++status_reads;
        if (release_at >= 0 && clock_us >= release_at) {
            status_byte &= ~cell(0x40); release_at = -1;
        }
        if (status_reads == inject_busy_read) status_byte |= 1;
        out = status_byte;
        // INUSE is read-to-acquire: return the previous value, then set it.
        status_byte |= 0x40;
        return STATUS_SUCCESS;
    }
    ++control_reads; out=0;
    return control_read_error;
}
NTSTATUS virtual_write_byte(cell reg, cell value) {
    if (reg == SMBHSTSTS) {
        if (!status_writes) { status_before_first_write=status_byte; pre_write_us=clock_us; }
        ++status_writes; last_status_write=value;
        status_byte &= ~value;
        return STATUS_SUCCESS;
    }
    ++control_writes;
    return control_write_error;
}
NTSTATUS record_transfer(cell& hststs) {
    ++transaction_calls;
    first_transfer_us=clock_us;
    hststs=transaction_hststs;
    return transfer_error;
}
NTSTATUS i801_simple_transaction(cell, cell, cell, cell, cell, cell& out, cell& hststs) {
    out=0; return record_transfer(hststs);
}
NTSTATUS i801_smbus_block_transaction(cell, cell, cell, cell, cell[33], cell[33], cell& hststs) {
    return record_transfer(hststs);
}
void i801_kill() { ++kill_calls; }
NTSTATUS i801_hststs_to_ntstatus(cell hststs) {
    return hststs ? STATUS_IO_DEVICE_ERROR : STATUS_SUCCESS;
}
