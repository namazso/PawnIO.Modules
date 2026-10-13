# SPDX-License-Identifier: LGPL-2.1-or-later
"""Execute compiled AMX with synthetic PCI/MMIO/SMBus only. No native device access."""
from pathlib import Path
import hashlib,json,struct
from amx_runtime import Vm,status,MASK

OUT=Path(__file__).resolve().parent
class Ddr5Vm(Vm):
    def __init__(self,blob):
        super().__init__(blob,{})
        self.mmio=bytearray(256)
        self.mmio[2]=1;self.mmio[3]=0x22;self.mmio[4]=0x66;self.mmio[5]=0x33;self.mmio[6]=0x44;self.mmio[13]=3
        self.original=bytes(self.mmio)
        self.hub=bytearray(128);self.hub[:5]=bytes([0x51,0x18,1,0x80,0xad]);self.hub[5]=2;self.hub[0x1a]=0;self.hub[0x31]=0x7c;self.hub[0x32]=2;self.hub[11]=0x0b
        self.pmic=bytearray(128);self.pmic[0x3b:0x3e]=bytes([0x10,0x8a,0x8c]);self.pmic[0x32]=0x80;self.pmic[0x30]=2
        self.spd=bytes((i*17+3)%256 for i in range(1024))
        self.ticks_us=0;self.selector_at=-10000;self.events=[];self.transfers=[];self.device_writes=[]
        self.fault=None;self.fault_used=False;self.commit_failed_write=False;self.hang=False;self.pending=None
        self.memory_enabled=True;self.sequence=0
        self.bus_fault=None;self.bus_fault_used=False;self.bus_fault_commits=False
        self.controller_changed=False;self.clock_frozen=False
        self.adc_values={0:73,1:74,2:72,3:120,5:72,8:120,9:67}
        self.word_reads=0;self.after_temperature=None
    def fail_now(self,name,args):
        if self.fault and not self.fault_used and self.fault(name,args):
            self.fault_used=True;return True
        return False
    def do_transfer(self):
        addr=self.mmio[4]>>1;read=self.mmio[4]&1;reg=self.mmio[3];value=self.mmio[5]
        self.transfers.append((addr,read,reg,value))
        if addr not in [0x50,0x48]:return 4
        failed=bool(self.bus_fault and not self.bus_fault_used and self.bus_fault(addr,read,reg,value))
        if failed:
            self.bus_fault_used=True
            if not self.bus_fault_commits:return 4
        if not read:
            assert (addr,reg) in [(0x50,11),(0x48,0x30)],('unexpected device write',addr,reg)
            self.device_writes.append((addr,reg,value))
            if addr==0x50:self.hub[reg]=value
            else:self.pmic[reg]=value;self.selector_at=self.ticks_us
        else:
            if addr==0x50:
                if reg>=0x80:
                    assert self.hub[11]&8==0,'EEPROM mode must be one-byte before reading'
                    value=self.spd[(self.hub[11]&7)*128+(reg&127)]
                else:
                    value=self.hub[reg]
                    if reg==0x31:
                        assert self.hub[11]&15==0,'temperature must be read on page zero in byte address mode'
                        assert (self.mmio[2]&0x1c)==0x0c,'temperature must use a single SMBus word transfer'
                        self.mmio[6]=self.hub[0x32]
                        self.word_reads+=1
                        if self.after_temperature:self.after_temperature(self)
            elif reg==0x31:
                assert self.ticks_us-self.selector_at>=9000,'ADC read before settling'
                selector=(self.pmic[0x30]>>3)&15
                values=self.adc_values
                assert selector in values,selector
                value=values[selector]
            else:value=self.pmic[reg]
            self.mmio[5]=value
        return 4 if failed else 2
    def write_mmio(self,reg,value):
        assert 0<=value<=255
        if reg==0:
            self.mmio[0]&=~value
        else:self.mmio[reg]=value
        if reg==2:
            if value&2:
                self.hang=False;self.mmio[0]=(self.mmio[0]&0x40)|0x10
            elif value&0x40:
                self.mmio[2]=value&~0x40
                self.mmio[0]=(self.mmio[0]&0x40)|1
                self.pending=True
    def native(self,index,args):
        name=self.natives[index];self.events.append((name,args))
        if name=='get_arch':return 1
        if name=='cpuid':
            for i,v in enumerate([0,0x756e6547,0x6c65746e,0x49656e69]):self.put(args[2]+8*i,v)
            return 0
        if name in ['virtual_read_qword','virtual_read_dword']:
            expected=0xfffff78000000320 if name=='virtual_read_qword' else 0xfffff78000000004
            assert args[0]==expected
            if self.fail_now(name,args):return status(0xc0000185)
            self.put(args[1],(0 if self.clock_frozen else self.ticks_us//1000) if name=='virtual_read_qword' else 1<<24)
            return 0
        if name=='microsleep':
            if self.fail_now(name,args):return status(0xc0000185)
            self.ticks_us+=args[0];return 0
        if name.startswith('pci_config_read_'):
            bus,dev,fn,reg,p=args
            if self.fail_now(name,args):return status(0xc0000185)
            if (bus,dev,fn)!=(128,31,4):self.put(p,0xffffffff);return 0
            values={0:0x7f238086,4:2 if self.memory_enabled else 0,0xa:0x0c05,0x10:0xfee00004,0x2c:0x14fb1462,0x40:1}
            if self.controller_changed:values[0]=0x12348086
            assert reg in values,('pci',name,reg)
            self.put(p,values[reg]);return 0
        if name=='io_space_map':assert args==(0xfee00000,256);self.maps.append(args);return 0x100000
        if name=='io_space_unmap':self.unmaps.append(args);return 0
        if name=='virtual_read_byte':
            reg=args[0]-0x100000;assert 0<=reg<256
            if self.fail_now(name,(reg,)):return status(0xc0000185)
            if reg==0 and self.pending and not self.hang:
                result=self.do_transfer();self.mmio[0]=(self.mmio[0]&0x40)|result;self.pending=None
            value=self.mmio[reg]
            if reg==0:self.mmio[0]|=0x40
            self.put(args[1],value);return 0
        if name=='virtual_write_byte':
            reg=args[0]-0x100000;value=args[1];assert 0<=reg<256
            fail=self.fail_now(name,(reg,value))
            if not fail or self.commit_failed_write:self.write_mmio(reg,value)
            return status(0xc0000185) if fail else 0
        raise AssertionError(('unexpected native',name,args))
    def exchange(self,operation,slot=0,argument=0,sequence=None):
        self.sequence+=1
        inp=(self.heap+15)&~7;out=inp+64;self.heap=out+256+64
        for i,v in enumerate([self.sequence if sequence is None else sequence,operation,slot,argument]):self.put(inp+8*i,v)
        for i in range(32):self.put(out+8*i,0)
        result=self.run('ioctl_ddr5',(inp,4,out,32))
        raw=bytes(self.mem[out:out+256]);words=struct.unpack('<32Q',raw)
        return result,words,raw[128:]
    def restored(self):
        return all(self.mmio[r]==self.original[r] for r in [2,3,4,5,6,13]) and self.mmio[0]==0

def main():
    import argparse
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--module-dir',type=Path,default=OUT/'out')
    parser.add_argument('--output-dir',type=Path,default=OUT/'out')
    args=parser.parse_args();args.output_dir.mkdir(parents=True,exist_ok=True)
    blob=(args.module_dir/'IntelDdr5.amx').read_bytes();tests=[]
    vm=Ddr5Vm(blob);assert vm.run()==0
    result,w,data=vm.exchange(0);assert result==0 and w[6:9]==(0,0,1) and w[10]==0x14fb14627f238086 and w[11]==0x801f04
    tests.append({'case':'nonzero-bus-controller-identity','passed':True})
    for page in range(8):
        result,w,data=vm.exchange(2,0,page)
        assert result==0 and w[6:9]==(0,0,1),(page,w)
        assert data==vm.spd[page*128:(page+1)*128]
        assert vm.hub[11]==0x0b and vm.restored()
    tests.append({'case':'all-eight-spd-pages-restore-mode-and-controller','passed':True})
    for channel in [0,1,2,3,5,8,9]:
        result,w,data=vm.exchange(3,0,channel)
        assert result==0 and w[6:9]==(0,0,1),(channel,w)
        assert data[0]==data[4]==2 and data[1]==data[3]==(0x82 | channel<<3)
        assert vm.pmic[0x30]==2 and vm.restored()
    tests.append({'case':'all-adc-channels-settle-and-restore','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    result,w,data=vm.exchange(4,0,0)
    assert result==0 and w[1]==2 and w[6:9]==(0,0,1) and w[14]==24
    assert data[:5]==bytes([0x51,0x18,1,0x80,0xad]) and data[5:8]==bytes([2,0,1]) and data[8:10]==bytes([0x7c,2])
    assert data[10:12]==bytes([2,0]) and data[12:14]==b'\x0b\x0b' and data[14:19]==data[:5] and not any(data[19:])
    assert vm.word_reads==1 and vm.hub[11]==0x0b and vm.restored()
    assert sum(a==0x50 and r==1 and c==0x1a for a,r,c,v in vm.transfers)==2
    assert sum(a==0x50 and r==1 and c==4 for a,r,c,v in vm.transfers)==2
    tests.append({'case':'spd5118-temperature-capability-and-word-read','passed':True})
    for capability,config,name in [(0,0,'temperature-unsupported-is-explicit'),(2,1,'temperature-disabled-is-not-enabled')]:
        vm=Ddr5Vm(blob);assert vm.run()==0;vm.hub[5]=capability;vm.hub[0x1a]=config
        result,w,data=vm.exchange(4,0,0)
        assert result==0 and w[6:9]==(0,0,1) and data[7]==0 and data[8:10]==b'\0\0', (name,w,data)
        assert not any(address==0x50 and read==1 and reg in [0x31,0x32] for address,read,reg,value in vm.transfers)
        tests.append({'case':name,'passed':True})
    for raw in [0,0x1ffc,0x1000,0x0ffc]:
        vm=Ddr5Vm(blob);assert vm.run()==0;vm.hub[0x31:0x33]=raw.to_bytes(2,'little')
        vm.after_temperature=lambda vm:vm.hub.__setitem__(slice(0x31,0x33),b'\x00\x03')
        result,w,data=vm.exchange(4)
        assert result==0 and w[6:9]==(0,0,1) and data[8:10]==raw.to_bytes(2,'little') and vm.word_reads==1
    tests.append({'case':'temperature-word-latches-zero-negative-and-boundaries','passed':True})
    for offset in [5,0x1a,4]:
        vm=Ddr5Vm(blob);assert vm.run()==0
        vm.after_temperature=lambda vm,o=offset:vm.hub.__setitem__(o,vm.hub[o]^1)
        result,w,data=vm.exchange(4)
        assert result==0 and w[6]!=0 and w[7]==0 and not any(data) and vm.restored()
    tests.append({'case':'temperature-rechecks-capability-enable-and-hub-after-read','passed':True})
    for capability,config in [(6,0),(2,2)]:
        vm=Ddr5Vm(blob);assert vm.run()==0;vm.hub[5]=capability;vm.hub[0x1a]=config
        result,w,data=vm.exchange(4)
        assert result==0 and w[6]!=0 and w[7]==0 and vm.word_reads==0 and not any(data) and vm.hub[11]==0x0b and vm.restored()
    tests.append({'case':'temperature-reserved-configuration-never-reads-value','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda n,a:n=='virtual_read_byte' and a==(6,) and vm.word_reads==1
    result,w,data=vm.exchange(4)
    assert result==0 and w[6]==status(0xc0000185) and w[7]==0 and not any(data) and vm.restored() and vm.hub[11]==0x0b
    tests.append({'case':'temperature-controller-high-byte-error-keeps-native-status','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.bus_fault=lambda a,r,c,v:a==0x50 and r==0 and c==11 and v==0x0b
    result,w,data=vm.exchange(4)
    assert result==0 and w[6]==0 and w[7]==status(0xc000009d) and w[8]==0 and not any(data)
    tests.append({'case':'temperature-selector-restoration-failure-discards-value','passed':True})
    for op,slot,arg in [(3,0,4),(3,0,6),(3,0,7),(3,8,0),(2,0,8),(0,1,0),(4,0,1),(4,8,0),(5,0,0)]:
        bad=Ddr5Vm(blob);assert bad.run()==0;n=len(bad.events)
        result,_,_=bad.exchange(op,slot,arg)
        assert result==status(0xc000000d) and len(bad.events)==n
    tests.append({'case':'invalid-request-zero-native-dispatch','passed':True})
    vm=Ddr5Vm(blob);vm.memory_enabled=False
    assert vm.run()==status(0xc00000bb) and not vm.maps
    tests.append({'case':'disabled-memory-decode-is-not-enabled','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.mmio[0]=0x40;result,w,_=vm.exchange(1)
    assert result==0 and w[6]==status(0x80000011) and w[7]==0
    assert vm.mmio[0]&0x40 and not vm.device_writes
    tests.append({'case':'foreign-inuse-is-not-released','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    result,w,_=vm.exchange(1,7)
    assert result==0 and w[6]==status(0xc000009d) and w[7]==0 and w[9]==10 and vm.restored()
    tests.append({'case':'address-nack-is-explicit-and-clean','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0;vm.hang=True
    result,w,_=vm.exchange(1)
    assert result==0 and w[6]==status(0xc00000b5) and vm.restored(),w
    tests.append({'case':'timeout-kills-only-owned-transaction','passed':True})
    for commit in [False,True]:
        vm=Ddr5Vm(blob);assert vm.run()==0;vm.commit_failed_write=commit
        vm.fault=lambda name,args:name=='virtual_write_byte' and args==(13,0)
        result,w,_=vm.exchange(1)
        assert result==0 and w[6]==status(0xc0000185) and w[7]==0 and vm.restored(),w
    tests.append({'case':'ambiguous-controller-write-restores-even-when-call-failed','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda name,args:name=='virtual_write_byte' and args==(13,3)
    result,w,data=vm.exchange(3,0,0)
    assert result==0 and w[6]==0 and w[7]==status(0xc0000185) and w[8]==0 and not any(data),w
    n=len(vm.events);result,_,_=vm.exchange(0)
    assert result!=0 and len(vm.events)==n
    tests.append({'case':'restore-failure-poisons-and-discards-values','passed':True})
    for commit in [False,True]:
        vm=Ddr5Vm(blob);assert vm.run()==0;vm.bus_fault_commits=commit
        vm.bus_fault=lambda a,r,c,v:a==0x50 and r==0 and c==11 and v==0
        result,w,data=vm.exchange(2,0,0)
        assert result==0 and w[6]==status(0xc000009d) and w[7]==0 and vm.hub[11]==0x0b and vm.restored(),w
        assert not any(data)
    tests.append({'case':'ambiguous-page-selector-write-always-restores-original','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.bus_fault=lambda a,r,c,v:a==0x50 and r==0 and c==11 and v==0x0b
    result,w,data=vm.exchange(2,0,0)
    assert result==0 and w[6]==0 and w[7]==status(0xc000009d) and not any(data)
    assert vm.mmio[0]&0x40
    tests.append({'case':'page-restoration-failure-retains-hardware-ownership','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.bus_fault=lambda a,r,c,v:a==0x48 and r==0 and c==0x30 and v==2
    result,w,data=vm.exchange(3,0,2)
    assert result==0 and w[6]==0 and w[7]==status(0xc000009d) and not any(data)
    tests.append({'case':'adc-restoration-failure-discards-measurement','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda n,a:n=='microsleep' and a==(9000,)
    result,w,data=vm.exchange(3,0,2)
    assert result==0 and w[6]==status(0xc0000185) and w[7]==0 and vm.pmic[0x30]==2 and vm.restored()
    assert not any(a==0x48 and r==1 and c==0x31 for a,r,c,v in vm.transfers)
    tests.append({'case':'failed-settle-does-not-read-stale-adc','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda n,a:n=='virtual_read_qword'
    result,w,data=vm.exchange(1)
    assert result==0 and w[6]==status(0xc0000185) and w[7]==0 and not vm.transfers
    tests.append({'case':'clock-failure-is-not-a-zero-deadline','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda n,a:n=='virtual_write_byte' and a==(0,0x40)
    result,w,data=vm.exchange(1)
    assert result==0 and w[6]==0 and w[7]==status(0xc0000185) and w[8]==0 and not any(data)
    tests.append({'case':'hardware-release-failure-is-not-success','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.bus_fault=lambda a,r,c,v:a==0x50 and r==1 and c==1
    result,w,_=vm.exchange(1)
    assert result==0 and w[6]==status(0xc000009d) and w[9]==11 and vm.restored()
    tests.append({'case':'partial-hub-identity-nack-is-not-an-empty-slot','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.fault=lambda n,a:n=='virtual_read_byte' and a==(0,) and vm.pending
    result,w,_=vm.exchange(1)
    assert result==0 and w[6]==status(0xc0000185) and w[7]==0 and vm.restored(),w
    tests.append({'case':'post-start-status-failure-clears-only-owned-terminal-flags','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0;vm.hang=True;vm.clock_frozen=True
    result,w,_=vm.exchange(1)
    assert result==0 and w[6]==status(0xc00000b5) and vm.restored(),w
    tests.append({'case':'stalled-clock-cannot-make-polling-unbounded','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0
    vm.bus_fault=lambda a,r,c,v:a==0x50 and r==1 and c==0x80
    vm.fault=lambda n,a:n=='virtual_write_byte' and a==(13,3)
    result,w,data=vm.exchange(2,0,0)
    assert result==0 and w[6]==status(0xc000009d) and w[7]==status(0xc0000185) and w[8]==0 and not any(data),w
    tests.append({'case':'simultaneous-acquisition-and-restoration-failures-retain-both-codes','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0;vm.controller_changed=True
    result,w,_=vm.exchange(3)
    assert result==0 and w[6]==status(0xc0000182) and not vm.transfers,w
    tests.append({'case':'changed-controller-stops-before-first-bus-operation','passed':True})
    vm=Ddr5Vm(blob);assert vm.run()==0;vm.pmic[0x3c]=0x80
    result,w,_=vm.exchange(3)
    assert result==0 and w[6]==status(0xc00000bb) and not vm.device_writes and vm.restored(),w
    tests.append({'case':'unknown-vendor-never-writes-adc-selector','passed':True})
    fixtures=[]
    for operation,slot,argument in [(0,0,0),(1,0,0),(2,0,2),(3,0,0),(3,0,5),(4,0,0),(1,7,0)]:
        vm=Ddr5Vm(blob);assert vm.run()==0
        result,w,_=vm.exchange(operation,slot,argument)
        assert result==0
        fixtures.append({'request':[1,operation,slot,argument],'words':w})
    fixture={'synthetic_only':True,'module_source_sha256':hashlib.sha256((args.module_dir/'IntelDdr5.p').read_bytes()).hexdigest(),'amx_sha256':hashlib.sha256(blob).hexdigest(),'receipts':fixtures}
    fixture['module_source_lf_sha256']=hashlib.sha256((args.module_dir/'IntelDdr5.p').read_text().encode()).hexdigest()
    (args.output_dir/'amx-receipts.json').write_text(json.dumps(fixture,indent=2)+'\n')
    result={'synthetic_only':True,'native_hardware_calls':0,'compiled_amx_sha256':hashlib.sha256(blob).hexdigest(),'cases':tests,'case_count':len(tests)}
    (args.output_dir/'module-replay-results.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result))
if __name__=='__main__':main()
