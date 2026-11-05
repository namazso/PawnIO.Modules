// SPDX-License-Identifier: LGPL-2.1-or-later
int failures=0;
#define CHECK(c) do { if (!(c)) ++failures; } while (false)
void reset(cell initial=0) {
    status_byte=initial; clock_us=status_reads=status_writes=last_status_write=0;
    release_at=inject_busy_read=-1;
    transaction_calls=kill_calls=control_reads=control_writes=0;
    first_transfer_us=pre_write_us=status_before_first_write=-1;
    control_read_error=control_write_error=transfer_error=transaction_hststs=0;
}
NTSTATUS access(bool block, cell size=-1) {
    cell input[33], output[33], value=0;
    // Avoid CRT-generated memset: only the fake transaction sees these arrays.
    input[0]=output[0]=0;
    if (block) return i801_access_block(0x71,0,3,size<0?I2C_SMBUS_BLOCK_DATA:size,input,output);
    return i801_access_simple(0x71,0,3,size<0?I2C_SMBUS_BYTE_DATA:size,0,value);
}
void check_release() {
    CHECK(!(status_byte & SMBHSTSTS_INUSE_STS));
    CHECK(last_status_write == (SMBHSTSTS_INUSE_STS | STATUS_FLAGS));
}
int main() {
    for (int path=0; path<2; ++path) {
        const bool block=path!=0;
        // 1. Free controller: immediate transfer, then release.
        reset(); CHECK(access(block)==STATUS_SUCCESS);
        CHECK(first_transfer_us==0 && transaction_calls==1 && kill_calls==0);
        CHECK(status_writes==1); check_release();
        // 2. Firmware releases during the grace period.
        reset(0x40); release_at=1250;
        CHECK(access(block)==STATUS_SUCCESS);
        CHECK(first_transfer_us==1250 && transaction_calls==1); check_release();
        // 3. Persistent INUSE: wait the full 80 ms, then transfer (no early reset).
        reset(0x40); CHECK(access(block)==STATUS_SUCCESS);
        CHECK(first_transfer_us==80000 && transaction_calls==1 && status_reads==322);
        CHECK(pre_write_us==80000 && status_before_first_write==0x40);
        CHECK(status_writes==1 && kill_calls==0); check_release();
        // 4. Next transfer has no additional wait after normal release.
        CHECK(access(block)==STATUS_SUCCESS);
        CHECK(clock_us==80000 && transaction_calls==2); check_release();
        // 5. Persistent INUSE plus HOST_BUSY: no transaction or KILL.
        reset(0x41); CHECK(access(block)==STATUS_DEVICE_BUSY);
        CHECK(clock_us==80000 && transaction_calls==0 && kill_calls==0);
        CHECK(control_reads==0 && control_writes==0 && status_byte==1);
        // Existing cleanup still releases INUSE/status on this error path.
        CHECK(status_writes==1); check_release();
        // 6. HOST_BUSY without initial INUSE: also refused immediately.
        reset(1); CHECK(access(block)==STATUS_DEVICE_BUSY);
        CHECK(clock_us==0 && transaction_calls==0 && kill_calls==0); check_release();
        // 7. HOST_BUSY appears between the wait and the preflight check.
        reset(0x40); inject_busy_read=322;
        CHECK(access(block)==STATUS_DEVICE_BUSY);
        CHECK(transaction_calls==0 && kill_calls==0); check_release();
        // 8. Release precisely at the deadline is accepted.
        reset(0x40); release_at=80000;
        CHECK(access(block)==STATUS_SUCCESS && first_transfer_us==80000); check_release();
        // 9. Auxiliary-control read failure: abort, retaining normal cleanup.
        reset(0x40); control_read_error=STATUS_IO_DEVICE_ERROR;
        CHECK(access(block)==STATUS_IO_DEVICE_ERROR);
        CHECK(transaction_calls==0 && control_writes==0 && kill_calls==0); check_release();
        // 10. Auxiliary-control write failure: abort before the transaction.
        reset(0x40); control_write_error=STATUS_IO_DEVICE_ERROR;
        CHECK(access(block)==STATUS_IO_DEVICE_ERROR && transaction_calls==0); check_release();
        // 11. Transfer failure: existing KILL and release paths are retained.
        reset(0x40); transfer_error=STATUS_IO_DEVICE_ERROR;
        CHECK(access(block)==STATUS_IO_DEVICE_ERROR);
        CHECK(transaction_calls==1 && kill_calls==1); check_release();
        // 12. Completion status is still returned through the existing mapper.
        reset(0x40); transaction_hststs=SMBHSTSTS_DEV_ERR;
        CHECK(access(block)==STATUS_IO_DEVICE_ERROR && kill_calls==0); check_release();
        // 13. Unsupported protocol never reaches a transaction.
        reset(0x40); CHECK(access(block,99)==STATUS_NOT_SUPPORTED);
        CHECK(transaction_calls==0 && kill_calls==0); check_release();
        // 14. Stale flags are cleared by preflight, not mistaken for HOST_BUSY.
        reset(0x40 | SMBHSTSTS_DEV_ERR);
        CHECK(access(block)==STATUS_SUCCESS && status_writes==2); check_release();
    }
    return failures ? 1 : 0;
}
