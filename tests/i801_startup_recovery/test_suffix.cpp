int failures = 0;
#define CHECK(condition) do { if (!(condition)) ++failures; } while (false)
// SPDX-License-Identifier: LGPL-2.1-or-later
void reset() {
    vendor_device=0x7F238086; subsystem=0x88EF1043; pci_command=2;
    tick=reads=writes=pci_reads=sequence_position=0; last_write=-1;
    sequence_length=1; sequence[0]=0x40;
    pci_failure=read_failure=write_failure=false;
    i801_recovery_attempted=false;
}
int main() {
    cell result, last, status;
    reset(); status=i801_startup_recovery(0, result, last);
    CHECK(status==STATUS_ACCESS_DENIED && pci_reads==0 && reads==0 && writes==0);
    reset(); vendor_device=0x12348086;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_NOT_SUPPORTED && reads==0 && writes==0);
    reset(); subsystem=0x99991043;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_NOT_SUPPORTED && reads==0 && writes==0);
    reset(); pci_failure=true;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_IO_DEVICE_ERROR && reads==0 && writes==0);
    reset(); pci_command=0;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_DEVICE_NOT_READY && reads==0 && writes==0);
    reset(); read_failure=true;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_IO_DEVICE_ERROR && writes==0);
    const cell refused[]={0x41,0x42,0x44,0x48,0x50,0x60,0xC0,0xFF};
    for (int i=0;i<8;++i) {
        reset(); sequence[0]=refused[i];
        CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_DEVICE_BUSY && writes==0 && result==0);
    }
    reset(); sequence[0]=0;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_SUCCESS && result==1 && writes==1 && last_write==0x40 && tick==0);
    reset(); sequence[1]=0; sequence_length=2;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_SUCCESS && result==1 && writes==1 && last_write==0x40 && tick==1);
    reset(); sequence[1]=0x41; sequence_length=2;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_DEVICE_BUSY && writes==0);
    reset();
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_SUCCESS && result==2 && writes==1 && last_write==0x40 && tick>=250 && tick<=251);
    cell old_reads=reads;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_ACCESS_DENIED && writes==1 && reads==old_reads);
    reset(); write_failure=true;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_IO_DEVICE_ERROR && writes==1 && result==0);
    reset(); sequence[0]=0; write_failure=true;
    CHECK(i801_startup_recovery(I801_RECOVERY_ACK,result,last)==STATUS_IO_DEVICE_ERROR && writes==1 && result==0);
    return failures;
}
