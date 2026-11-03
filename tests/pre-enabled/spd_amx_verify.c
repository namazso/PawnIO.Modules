/* Executes the complete candidate AMX with simulated native I/O only.
 * No driver, device, kernel invocation or physical memory access is present.
 * SPDX-License-Identifier: MIT */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "amx.h"

#define IOERR ((cell)(int32_t)0xc0000185)
#define NOTREADY ((cell)(int32_t)0xc00000a3)
#define INVALID ((cell)(int32_t)0xc000000d)
#define UNSUPPORTED ((cell)(int32_t)0xc00000bb)
#define REQUIRE(x) do { if (!(x)) { fprintf(stderr,"FAIL %s:%d: %s\n",__FILE__,__LINE__,#x); exit(1); } } while (0)
enum { OK, DISABLED_PRE, DISABLED_INNER, PRE_ID_ERR, PRE_CMD_ERR,
       INNER_CMD_ERR, MAIN_ERR, POST_ID_ERR, POST_CMD_ERR, DUAL_ERR,
       ID_CHANGED, CMD_CHANGED, PRE_ID_CHANGED, BOTH_POST_ERR, BUS_ERR, BUS_DUAL_ERR };
static int intel, scenario, initializing, id_reads, cmd_reads, pci_writes;
static int starts, unexpected, ticks, native_calls;
static unsigned char registers[256];
static cell ids, mask, bdf;
static FILE *vectors;

static void output(AMX *amx, cell ptr, cell value) { *amx_Address(amx,ptr)=value; }
static cell pci_read(AMX *amx, const cell *p, int width) {
    cell value=0;
    REQUIRE(p[1]==0 && p[2]==(intel?31:20) && p[3]==(intel?4:0));
    if (initializing) {
        switch (p[4]) {
        case 0: value=ids & 0xffff; break;
        case 2: value=ids >> 16; break;
        case 10: value=0x0c05; break;
        case 8: value=0x51; break;
        case 0x40: value=1; break;
        case 0x10: value=0x10000; break;
        case 0x20: value=0xb01; break;
        default: REQUIRE(0);
        }
    } else if (p[4]==0 && width==4) {
        ++id_reads; value=ids;
        if ((id_reads==1 && scenario==PRE_ID_ERR) ||
            (id_reads==2 && (scenario==POST_ID_ERR || scenario==BOTH_POST_ERR))) return IOERR;
        if ((id_reads==1 && scenario==PRE_ID_CHANGED) ||
            (id_reads==2 && scenario==ID_CHANGED)) value^=0x10000;
    } else if (p[4]==4 && width==2) {
        ++cmd_reads; value=mask|4;
        if ((cmd_reads==1 && scenario==PRE_CMD_ERR) ||
            (cmd_reads==2 && scenario==INNER_CMD_ERR) ||
            (cmd_reads==3 && (scenario==POST_CMD_ERR || scenario==DUAL_ERR || scenario==BOTH_POST_ERR || scenario==BUS_DUAL_ERR))) return IOERR;
        if ((cmd_reads==1 && scenario==DISABLED_PRE) ||
            (cmd_reads==2 && scenario==DISABLED_INNER)) value&=~mask;
        if (cmd_reads==3 && scenario==CMD_CHANGED) value^=4;
    } else { ++unexpected; return IOERR; }
    output(amx,p[5],value); return 0;
}
static cell reg_read(cell address) {
    unsigned offset=(unsigned)(address-(intel?0x10000:0xb00));
    REQUIRE(offset<256);
    if (offset==5) return 0x5a;
    return registers[offset];
}
static void reg_write(cell address, cell value) {
    unsigned offset=(unsigned)(address-(intel?0x10000:0xb00));
    REQUIRE(offset<256);
    if (offset==0) registers[0]&=~(unsigned char)value;
    else registers[offset]=(unsigned char)value;
    if (offset==2 && (value&0x40)) { ++starts; registers[0]=(scenario==BUS_ERR || scenario==BUS_DUAL_ERR)?0x12:2; }
}
static cell dispatch(AMX *amx, const cell *p, const char *name) {
    REQUIRE(++native_calls<10000);
    if (!strcmp(name,"get_arch")) return 1;
    if (!strcmp(name,"debug_print") || !strcmp(name,"microsleep") || !strcmp(name,"io_space_unmap")) return 0;
    if (!strcmp(name,"io_space_map")) return 0x10000; /* simulated address only */
    if (!strcmp(name,"get_proc_address")) return 0x1234; /* never dereferenced */
    if (!strcmp(name,"invoke")) { REQUIRE(p[1]==0x1234); output(amx,p[2],0); return 0; }
    if (!strncmp(name,"pci_config_write_",17)) { ++pci_writes; return IOERR; }
    if (!strcmp(name,"pci_config_read_byte")) return pci_read(amx,p,1);
    if (!strcmp(name,"pci_config_read_word")) return pci_read(amx,p,2);
    if (!strcmp(name,"pci_config_read_dword")) return pci_read(amx,p,4);
    if (!strcmp(name,"pci_config_read_qword")) return pci_read(amx,p,8);
    if (!strcmp(name,"virtual_read_qword")) {
        REQUIRE((uint64_t)p[1]==UINT64_C(0xfffff78000000320));
        output(amx,p[2],++ticks); return 0;
    }
    if (!strcmp(name,"virtual_read_dword")) {
        REQUIRE((uint64_t)p[1]==UINT64_C(0xfffff78000000004));
        output(amx,p[2],1<<24); return 0;
    }
    if (!strcmp(name,"virtual_read_byte") || !strcmp(name,"virtual_read_word")) {
        cell value;
        if (!intel) { REQUIRE(initializing); REQUIRE(p[1]==0x10000 || p[1]==0x10001); value=p[1]==0x10000?0x10:0x0b; }
        else value=reg_read(p[1]);
        output(amx,p[2],value); return 0;
    }
    if (!strcmp(name,"virtual_write_byte") || !strcmp(name,"virtual_write_word")) {
        REQUIRE(intel); reg_write(p[1],p[2]); return 0;
    }
    if (!strcmp(name,"io_in_byte")) return reg_read(p[1]);
    if (!strcmp(name,"io_out_byte")) { reg_write(p[1],p[2]); return 0; }
    fprintf(stderr,"Unexpected native: %s\n",name); ++unexpected; return IOERR;
}
#define NATIVE(name) static cell AMX_NATIVE_CALL stub_##name(AMX*a,const cell*p) { return dispatch(a,p,#name); }
NATIVE(get_arch) NATIVE(debug_print) NATIVE(microsleep) NATIVE(io_space_map)
NATIVE(io_space_unmap) NATIVE(get_proc_address) NATIVE(invoke)
NATIVE(pci_config_read_byte) NATIVE(pci_config_read_word) NATIVE(pci_config_read_dword)
NATIVE(pci_config_read_qword) NATIVE(pci_config_write_word)
NATIVE(virtual_read_byte) NATIVE(virtual_read_word) NATIVE(virtual_read_dword)
NATIVE(virtual_read_qword) NATIVE(virtual_write_byte) NATIVE(virtual_write_word)
NATIVE(io_in_byte) NATIVE(io_out_byte)
#define ENTRY(name) {#name,stub_##name}
static const AMX_NATIVE_INFO natives[]={
    ENTRY(get_arch),ENTRY(debug_print),ENTRY(microsleep),ENTRY(io_space_map),
    ENTRY(io_space_unmap),ENTRY(get_proc_address),ENTRY(invoke),
    ENTRY(pci_config_read_byte),ENTRY(pci_config_read_word),ENTRY(pci_config_read_dword),
    ENTRY(pci_config_read_qword),ENTRY(pci_config_write_word),
    ENTRY(virtual_read_byte),ENTRY(virtual_read_word),ENTRY(virtual_read_dword),
    ENTRY(virtual_read_qword),ENTRY(virtual_write_byte),ENTRY(virtual_write_word),
    ENTRY(io_in_byte),ENTRY(io_out_byte),{NULL,NULL}};

static void *load(AMX *amx,const char *path,int inventory) {
    FILE *f=fopen(path,"rb"); AMX_HEADER header; void *program; int count,i; char name[256];
    REQUIRE(f); REQUIRE(fread(&header,1,sizeof(header),f)==sizeof(header));
    REQUIRE(header.stp>header.size && header.stp<16*1024*1024);
    program=calloc(1,header.stp); REQUIRE(program); rewind(f);
    REQUIRE(fread(program,1,header.size,f)==header.size); fclose(f);
    REQUIRE(amx_Init(amx,program)==0);
    REQUIRE(amx_NumNatives(amx,&count)==0);
    for (i=0;i<count;++i) { REQUIRE(amx_GetNative(amx,i,name)==0); if(inventory) printf("IMPORT %s\n",name); }
    REQUIRE(amx_NumPublics(amx,&count)==0);
    for (i=0;i<count;++i) { ucell address; REQUIRE(amx_GetPublic(amx,i,name,&address)==0); if(inventory) printf("EXPORT %s\n",name); }
    REQUIRE(amx_Register(amx,natives,-1)==0);
    return program;
}
static void run(const char *path,int which,int malformed) {
    AMX amx={0}; cell result,*input,*out; int index,i,flags=31;
    cell request[14]={1,0x12345678,0,0,4,0x50,1,0,2,0,0,0,0,0};
    int in_size=14,out_size=21; void *program;
    scenario=which; initializing=1; id_reads=cmd_reads=pci_writes=starts=unexpected=ticks=native_calls=0;
    memset(registers,0,sizeof(registers));
    program=load(&amx,path,which==0 && malformed==0);
    REQUIRE(amx_Exec(&amx,&result,AMX_EXEC_MAIN)==0); REQUIRE(result==0);
    REQUIRE(pci_writes==0 && unexpected==0); initializing=0;
    request[2]=bdf; request[3]=ids;
    if(which==MAIN_ERR || which==DUAL_ERR) request[8]=999;
    switch(malformed) {
    case 1: in_size=13; break; case 2: out_size=20; break;
    case 3: request[0]=2; break; case 4: request[1]=0; break;
    case 5: request[2]^=1; break; case 6: request[3]=-1; break;
    case 7: request[3]=INT64_C(0x100000000); break;
    case 8: request[4]=3; break; case 9: request[4]=10; break;
    case 10: request[13]=1; break; case 11: request[5]=128; break;
    case 12: request[5]=-1; break; case 13: request[6]=2; break;
    case 14: in_size=15; break; case 15: out_size=22; break;
    }
    REQUIRE(amx_Allot(&amx,23,&out)==0); for(i=0;i<23;++i) out[i]=0x777;
    REQUIRE(amx_Allot(&amx,14,&input)==0); memcpy(input,request,sizeof(request));
    REQUIRE(amx_FindPublic(&amx,"ioctl_xfer_pre_enabled",&index)==0);
    REQUIRE(amx_Push(&amx,out_size)==0); REQUIRE(amx_PushAddress(&amx,out+1)==0);
    REQUIRE(amx_Push(&amx,in_size)==0); REQUIRE(amx_PushAddress(&amx,input)==0);
    REQUIRE(amx_Exec(&amx,&result,index)==0);
    REQUIRE(out[0]==0x777 && out[22]==0x777); ++out;
    if (vectors && !malformed) {
        fprintf(vectors,"%d %016llx",which,(unsigned long long)result);
        for(i=0;i<14;++i) fprintf(vectors," %016llx",(unsigned long long)request[i]);
        for(i=0;i<21;++i) fprintf(vectors," %016llx",(unsigned long long)out[i]);
        fprintf(vectors,"\n");
    }
    REQUIRE(pci_writes==0 && unexpected==0);
    if (malformed) {
        REQUIRE(result==INVALID && starts==0 && id_reads==0 && cmd_reads==0);
        for(i=0;i<21;++i) REQUIRE(out[i]==0x777);
    } else {
        REQUIRE(result==0 && out[0]==1 && out[1]==request[1] && out[2]==bdf && out[3]==mask);
        if(which==PRE_ID_ERR || which==PRE_ID_CHANGED) flags=1;
        if(which==DISABLED_PRE || which==PRE_CMD_ERR) flags=3;
        REQUIRE(out[4]==flags);
        REQUIRE(out[5]==(which==PRE_ID_ERR?IOERR:0));
        REQUIRE(out[7]==(flags==1?NOTREADY:which==PRE_CMD_ERR?IOERR:0));
        REQUIRE(out[9]==(flags!=31 || which==DISABLED_INNER?NOTREADY:
            which==INNER_CMD_ERR || which==BUS_ERR || which==BUS_DUAL_ERR?IOERR:which==MAIN_ERR || which==DUAL_ERR?UNSUPPORTED:0));
        REQUIRE(out[10]==(flags!=31?NOTREADY:which==POST_ID_ERR || which==BOTH_POST_ERR?IOERR:0));
        REQUIRE(out[12]==(flags!=31?NOTREADY:which==POST_CMD_ERR || which==DUAL_ERR || which==BOTH_POST_ERR || which==BUS_DUAL_ERR?IOERR:0));
        REQUIRE(out[14]==(flags==31 && which!=POST_ID_ERR && which!=POST_CMD_ERR &&
            which!=DUAL_ERR && which!=ID_CHANGED && which!=CMD_CHANGED && which!=BOTH_POST_ERR && which!=BUS_DUAL_ERR));
        REQUIRE(id_reads==(flags==31?2:1) && cmd_reads==(flags==31?3:flags==3?1:0));
        REQUIRE(out[15]==(out[9]==0?5:0));
        if(out[9]==0) REQUIRE(out[16]==0x5a && starts==1);
        else { REQUIRE(starts==(which==BUS_ERR || which==BUS_DUAL_ERR?1:0)); for(i=16;i<21;++i) REQUIRE(out[i]==0); }
    }
    printf("PASS scenario=%d malformed=%d flags=%lld pci_writes=%d starts=%d\n",which,malformed,(long long)out[4],pci_writes,starts);
    REQUIRE(amx_Cleanup(&amx)==0); free(program);
}
static void legacy_probe(const char *path, int test) {
    AMX amx={0}; cell result,*input,*out; int index,i;
    cell request[5]={0x50,1,0,2,0};
    int count=4; void *program;
    scenario=OK; initializing=1; id_reads=cmd_reads=pci_writes=starts=unexpected=ticks=native_calls=0;
    memset(registers,0,sizeof(registers));
    program=load(&amx,path,0);
    REQUIRE(amx_Exec(&amx,&result,AMX_EXEC_MAIN)==0 && result==0);
    initializing=0;
    switch(test) {
    case 1: request[3]=3; break; /* word-data read */
    case 2: request[1]=0; request[3]=1; count=5; break; /* send byte */
    case 3: request[1]=0; request[4]=3; count=5; break; /* byte-data write */
    case 4: request[3]=4; request[4]=0x1234; count=5; break; /* process call */
    case 5: request[1]=2; break;
    case 6: request[0]=128; break;
    case 7: request[1]=0; break; /* missing write payload */
    case 8: scenario=BUS_ERR; break;
    case 9: scenario=DISABLED_PRE; break; /* legacy enable-write attempt */
    }
    REQUIRE(amx_Allot(&amx,7,&out)==0); for(i=0;i<7;++i) out[i]=0x777;
    memset(out+1,0,5*sizeof(cell));
    REQUIRE(amx_Allot(&amx,5,&input)==0); memcpy(input,request,sizeof(request));
    REQUIRE(amx_FindPublic(&amx,"ioctl_smbus_xfer",&index)==0);
    REQUIRE(amx_Push(&amx,5)==0); REQUIRE(amx_PushAddress(&amx,out+1)==0);
    REQUIRE(amx_Push(&amx,count)==0); REQUIRE(amx_PushAddress(&amx,input)==0);
    REQUIRE(amx_Exec(&amx,&result,index)==0 && unexpected==0);
    REQUIRE(out[0]==0x777 && out[6]==0x777);
    if(test==0 || test==1) REQUIRE(result==0 && out[1]==0x5a && starts==1);
    if(test==2 || test==3) REQUIRE(result==0 && starts==1);
    if(test==4) REQUIRE(result==(intel?0:UNSUPPORTED) && starts==(intel?1:0));
    if(test==5 || test==6) REQUIRE(result==INVALID && cmd_reads==0 && starts==0);
    if(test==7) REQUIRE(result==(cell)(int32_t)0xc0000023 && starts==0);
    if(test==8) REQUIRE(result==IOERR && starts==1);
    if(test==9) REQUIRE(result==IOERR && starts==0 && pci_writes==1);
    else REQUIRE(pci_writes==0);
    printf("LEGACY case=%d status=%016llx cmd_reads=%d pci_writes=%d starts=%d",test,(unsigned long long)result,cmd_reads,pci_writes,starts);
    for(i=1;i<=5;++i) printf(" %016llx",(unsigned long long)out[i]);
    printf("\n");
    REQUIRE(amx_Cleanup(&amx)==0); free(program);
}
int main(int argc,char **argv) {
    int i; REQUIRE((argc==3 || argc==4) && sizeof(cell)==8);
    if(!strcmp(argv[2],"legacy-i801") || !strcmp(argv[2],"legacy-piix4")) {
        intel=!strcmp(argv[2],"legacy-i801"); ids=intel?0x7f238086:0x790b1022;
        mask=intel?2:1; bdf=intel?0xfc:0xa0;
        for(i=0;i<10;++i) legacy_probe(argv[1],i);
        return 0;
    }
    if(argc==4) { vectors=fopen(argv[3],"wx"); REQUIRE(vectors); }
    REQUIRE(!strcmp(argv[2],"i801") || !strcmp(argv[2],"piix4"));
    intel=!strcmp(argv[2],"i801"); ids=intel?0x7f238086:0x790b1022;
    mask=intel?2:1; bdf=intel?0xfc:0xa0;
    for(i=0;i<=BUS_DUAL_ERR;++i) run(argv[1],i,0);
    for(i=1;i<=15;++i) run(argv[1],OK,i);
    if(vectors) REQUIRE(fclose(vectors)==0);
    printf("SUMMARY module=%s passed=31 hardware_access=false\n",argv[2]); return 0;
}
