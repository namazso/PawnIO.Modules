# SPDX-License-Identifier: LGPL-2.1-or-later
"""Offline execution of actual AMX bytes with bounded synthetic native callbacks.
No DLL, driver, device, port, PCI or MMIO operation is performed by this harness.
Opcode semantics follow the pinned PawnPP amx.h used in the root-cause investigation.
"""
from pathlib import Path
import hashlib,json,re,struct

OUT=Path(__file__).resolve().parent
REF=OUT/'reference/pawnpp-amx.h'
assert hashlib.sha256(REF.read_bytes().replace(b'\r\n', b'\n')).hexdigest() == 'b7a562a7a16d1de6b66ec4cfddc29b369972828c3630c02fc3d4b51660357fda', 'opcode reference changed'
text=REF.read_text()
NAMES=re.findall(r'\bOP_[A-Z_]+\b',text[text.index('OP_NOP = 0'):text.index('OP_NUM_OPCODES')])
blocks=re.split(r'case (OP_[A-Z_]+):',text[text.index('switch (opcode)'):])
ARGS={blocks[i] for i in range(1,len(blocks),2) if 'OPERAND();' in blocks[i+1]}
MASK=(1<<64)-1
def signed(value): return value if value<(1<<63) else value-(1<<64)
def status(value): return (value-(1<<32))&MASK if value&(1<<31) else value

class Vm:
    def __init__(self,blob,devices,fail=None,map_fail=False):
        h=struct.unpack_from('<IHBBHH12I',blob)
        assert h[1]==0xf1e1 and h[4]==0
        self.code=blob[h[6]:h[7]]; self.main=h[10]
        self.mem=bytearray(h[9]-h[7]); self.mem[:h[8]-h[7]]=blob[h[7]:h[8]]
        self.stp=len(self.mem)-8; self.heap=h[8]-h[7]
        self.natives=[]; self.publics={}
        for off in range(h[12],h[13],8):
            _,name=struct.unpack_from('<II',blob,off); self.natives.append(blob[name:blob.index(b'\0',name)].decode())
        for off in range(h[11],h[12],8):
            addr,name=struct.unpack_from('<II',blob,off); self.publics[blob[name:blob.index(b'\0',name)].decode()]=addr
        self.devices=devices; self.fail=fail; self.map_fail=map_fail; self.reads=[]; self.maps=[]; self.unmaps=[]; self.steps=0
    def get(self,at):
        assert 0<=at<=len(self.mem)-8 and at%8==0,at
        return struct.unpack_from('<Q',self.mem,at)[0]
    def put(self,at,value):
        assert 0<=at<=len(self.mem)-8 and at%8==0,at
        struct.pack_into('<Q',self.mem,at,value&MASK)
    def native(self,index,args):
        name=self.natives[index]
        if name=='get_arch': return 1
        if name=='cpuid':
            for i,v in enumerate([0,0x756e6547,0x6c65746e,0x49656e69]): self.put(args[2]+8*i,v)
            return 0
        if name in ['pci_config_read_dword','pci_config_read_word']:
            bus,dev,fn,offset,pointer=args; self.reads.append((bus,dev,fn,offset))
            if self.fail and self.fail[:4]==(bus,dev,fn,offset): return status(self.fail[4])
            entry=self.devices.get((bus,dev,fn))
            if entry is None: return status(0xc00000c0)
            values={0:(entry['id']<<16)|0x8086,4:2,0x10:entry.get('bar',0x10000000),0x14:0}
            assert offset in values,offset
            self.put(pointer,values[offset]); return 0
        if name=='io_space_map': self.maps.append(args); return 0 if self.map_fail else 0x100000
        if name=='io_space_unmap': self.unmaps.append(args); return 0
        if name in ['virtual_read_dword','virtual_read_word']:
            self.put(args[1],0x200|46 if name.endswith('dword') else 192); return 0
        raise AssertionError(('unexpected native',name,args))
    def run(self,entry=None,args=()):
        self.ip=self.main if entry is None else self.publics[entry]
        self.stk=self.stp; self.frm=0; self.pri=0; self.alt=0
        def push(v): self.stk-=8; self.put(self.stk,v)
        def pop():
            v=self.get(self.stk); self.stk+=8; return v
        for v in reversed(args): push(v)
        push(len(args)*8); push(MASK)
        while self.ip!=MASK:
            self.steps+=1; assert self.steps<30_000_000,'instruction budget'
            at=self.ip; op=struct.unpack_from('<Q',self.code,self.ip)[0]; self.ip+=8
            name=NAMES[op]; arg=0
            if name in ARGS: arg=struct.unpack_from('<q',self.code,self.ip)[0]; self.ip+=8
            if name in ['OP_NOP','OP_BREAK']: pass
            elif name=='OP_CONST_PRI': self.pri=arg&MASK
            elif name=='OP_CONST_ALT': self.alt=arg&MASK
            elif name=='OP_LOAD_PRI': self.pri=self.get(arg)
            elif name=='OP_LOAD_ALT': self.alt=self.get(arg)
            elif name=='OP_LOAD_S_PRI': self.pri=self.get(self.frm+arg)
            elif name=='OP_LOAD_S_ALT': self.alt=self.get(self.frm+arg)
            elif name=='OP_LREF_S_PRI': self.pri=self.get(self.get(self.frm+arg))
            elif name=='OP_LREF_S_ALT': self.alt=self.get(self.get(self.frm+arg))
            elif name=='OP_LOAD_I': self.pri=self.get(self.pri)
            elif name=='OP_ADDR_PRI': self.pri=self.frm+arg
            elif name=='OP_ADDR_ALT': self.alt=self.frm+arg
            elif name=='OP_STOR': self.put(arg,self.pri)
            elif name=='OP_STOR_S': self.put(self.frm+arg,self.pri)
            elif name=='OP_SREF_S': self.put(self.get(self.frm+arg),self.pri)
            elif name=='OP_STOR_I': self.put(self.alt,self.pri)
            elif name=='OP_XCHG': self.pri,self.alt=self.alt,self.pri
            elif name in ['OP_PUSH_PRI','OP_PUSHR_PRI']: push(self.pri)
            elif name=='OP_PUSH_ALT': push(self.alt)
            elif name=='OP_POP_PRI': self.pri=pop()
            elif name=='OP_POP_ALT': self.alt=pop()
            elif name=='OP_PICK': self.pri=self.get(self.stk+arg)
            elif name=='OP_STACK': self.stk+=arg; self.alt=self.stk
            elif name=='OP_HEAP': self.alt=self.heap; self.heap+=arg
            elif name=='OP_PROC': push(self.frm); self.frm=self.stk
            elif name in ['OP_RET','OP_RETN']:
                self.frm=pop(); self.ip=pop()
                if name=='OP_RETN': self.stk+=self.get(self.stk)+8
            elif name=='OP_CALL': push(self.ip); self.ip=at+arg
            elif name=='OP_JUMP': self.ip=at+arg
            elif name=='OP_JZER':
                if self.pri==0: self.ip=at+arg
            elif name=='OP_JNZ':
                if self.pri!=0: self.ip=at+arg
            elif name=='OP_SHL': self.pri=(self.pri<<self.alt)&MASK if self.alt<64 else 0
            elif name=='OP_SHR': self.pri=self.pri>>self.alt if self.alt<64 else 0
            elif name=='OP_SSHR': self.pri=(signed(self.pri)>>min(self.alt,64))&MASK
            elif name=='OP_SHL_C_PRI': self.pri=(self.pri<<arg)&MASK
            elif name=='OP_SHL_C_ALT': self.alt=(self.alt<<arg)&MASK
            elif name=='OP_SMUL': self.pri=(self.pri*self.alt)&MASK
            elif name=='OP_ADD': self.pri=(self.pri+self.alt)&MASK
            elif name=='OP_SUB': self.pri=(self.alt-self.pri)&MASK
            elif name=='OP_SDIV':
                divisor=signed(self.pri); dividend=signed(self.alt); assert divisor!=0
                self.pri=(dividend//divisor)&MASK; self.alt=(dividend%divisor)&MASK
            elif name=='OP_AND': self.pri&=self.alt
            elif name=='OP_OR': self.pri|=self.alt
            elif name=='OP_XOR': self.pri^=self.alt
            elif name=='OP_NOT': self.pri=int(self.pri==0)
            elif name=='OP_NEG': self.pri=(-self.pri)&MASK
            elif name=='OP_INVERT': self.pri=(~self.pri)&MASK
            elif name=='OP_EQ': self.pri=int(self.pri==self.alt)
            elif name=='OP_NEQ': self.pri=int(self.pri!=self.alt)
            elif name=='OP_SLESS': self.pri=int(signed(self.pri)<signed(self.alt))
            elif name=='OP_SLEQ': self.pri=int(signed(self.pri)<=signed(self.alt))
            elif name=='OP_SGRTR': self.pri=int(signed(self.pri)>signed(self.alt))
            elif name=='OP_SGEQ': self.pri=int(signed(self.pri)>=signed(self.alt))
            elif name=='OP_INC_PRI': self.pri=(self.pri+1)&MASK
            elif name=='OP_INC_ALT': self.alt=(self.alt+1)&MASK
            elif name=='OP_INC_I': self.put(self.pri,self.get(self.pri)+1)
            elif name=='OP_DEC_PRI': self.pri=(self.pri-1)&MASK
            elif name=='OP_DEC_ALT': self.alt=(self.alt-1)&MASK
            elif name=='OP_DEC_I': self.put(self.pri,self.get(self.pri)-1)
            elif name=='OP_FILL':
                for i in range(0,arg,8): self.put(self.alt+i,self.pri)
            elif name=='OP_MOVS': self.mem[self.alt:self.alt+arg]=self.mem[self.pri:self.pri+arg]
            elif name=='OP_SYSREQ':
                count=self.get(self.stk)//8; values=tuple(self.get(self.stk+8+i*8) for i in range(count)); self.pri=self.native(arg,values)&MASK
            elif name=='OP_SWITCH':
                table=at+arg; count=struct.unpack_from('<q',self.code,table+8)[0]; default=struct.unpack_from('<q',self.code,table+16)[0]; self.ip=table+8+default
                for i in range(count):
                    pos=table+24+i*16; val,dest=struct.unpack_from('<qq',self.code,pos)
                    if self.pri==(val&MASK): self.ip=pos+dest; break
            elif name=='OP_BOUNDS': assert 0<=signed(self.pri)<=arg
            elif name=='OP_HALT': return self.pri
            elif name=='OP_SWAP_PRI': value=self.get(self.stk); self.put(self.stk,self.pri); self.pri=value
            elif name=='OP_SWAP_ALT': value=self.get(self.stk); self.put(self.stk,self.alt); self.alt=value
            else: raise AssertionError(('unsupported instruction',hex(at),name,arg))
        return self.pri
