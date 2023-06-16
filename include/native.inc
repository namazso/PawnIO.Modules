//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2023  namazso <admin@namazso.eu>
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

native debug_print(const fmt[], ...)

native get_arch();

const {
    ARCH_X64 = 1,
    ARCH_A64 = 2
}

native cpu_count();
native cpu_set_affinity(which, old[2]);
native cpu_restore_affinity(old[2]);

native msr_read(msr, &value);
native msr_write(msr, value);

native interrupts_disable();
native interrupts_enable();

native physical_read_byte(pa, &value);
native physical_read_word(pa, &value);
native physical_read_dword(pa, &value);
native physical_read_qword(pa, &value);

native physical_write_byte(pa, value);
native physical_write_word(pa, value);
native physical_write_dword(pa, value);
native physical_write_qword(pa, value);

//native io_space_map(pa, size);
//native io_space_unmap(va, size);

native virtual_read_byte(va, &value);
native virtual_read_word(va, &value);
native virtual_read_dword(va, &value);
native virtual_read_qword(va, &value);

native virtual_write_byte(va, value);
native virtual_write_word(va, value);
native virtual_write_dword(va, value);
native virtual_write_qword(va, value);

native virtual_cmpxchg_byte(va, exchange, comparand);
native virtual_cmpxchg_word(va, exchange, comparand);
native virtual_cmpxchg_dword(va, exchange, comparand);
native virtual_cmpxchg_qword(va, exchange, comparand);

native virtual_alloc(size);
native virtual_free(va);

native pci_config_read_byte(bus, device, function, offset, &value);
native pci_config_read_word(bus, device, function, offset, &value);
native pci_config_read_dword(bus, device, function, offset, &value);
native pci_config_read_qword(bus, device, function, offset, &value);

native pci_config_write_byte(bus, device, function, offset, value);
native pci_config_write_word(bus, device, function, offset, value);
native pci_config_write_dword(bus, device, function, offset, value);
native pci_config_write_qword(bus, device, function, offset, value);

native get_proc_address(name[]);
native invoke(address, &retval, a0=0, a1=0, a2=0, a3=0, a4=0, a5=0, a6=0, a7=0, a8=0, a9=0, a10=0, a11=0, a12=0, a13=0, a14=0, a15=0);

//native microsleep(us);

// order: eax ecx edx ebx esi edi
native query_dell_smm(const in[6], out[6]);

native io_out_byte(port, value);
native io_out_word(port, value);
native io_out_dword(port, value);

native io_in_byte(port);
native io_in_word(port);
native io_in_dword(port);

native llwpcb(addr);
native slwpcb();

native cpuid(leaf, subleaf, out[4]);

native cr_read(cr);
native cr_write(cr, value);

native dr_read(dr);
native dr_write(dr, value);

native xcr_read(xcr, &value);
native xcr_write(xcr, value);

native invlpg(va);
native invpcid(type, descriptor);

native readpmc(pmc, value);

native rdtsc();
native rdtscp(&pid);

native rdrand(&v);
native rdseed(&v);

native lidt(limit, base);
native sidt(&limit, &base);

native lgdt(limit, base);
native sgdt(&limit, &base);

native mxcsr_read();
native mxcsr_write(v);

native stac();
native clac();

native halt();

native ud2();

native int3();

native int2c();

native wbinvd();
