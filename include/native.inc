//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2025  namazso <admin@namazso.eu>
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
//
//  SPDX-License-Identifier: LGPL-2.1-or-later

/// Send a message to the kernel debugger.
///
/// @param fmt Format string, only integers are supported
/// @param ... Arguments to format string
native debug_print(const fmt[], ...);

/// Get the current architecture.
///
/// @return The current architecture
/// @retval ARCH_X64 x86-64
/// @retval ARCH_A64 ARM64
native get_arch();

const {
    ARCH_X64 = 1,
    ARCH_A64 = 2
};

/// Get total CPU count.
/// @return CPU count
native cpu_count();

/// Set affinity to specific CPU.
///
/// @param which Which CPU to pin to
/// @param old Old affinity, for restoring later
/// @warning Only takes effect on passive IRQL / interrupts enabled
native cpu_set_affinity(which, old[2]);

/// Restore affinity.
///
/// @param old Old affinity to restore
/// @warning Only takes effect on passive IRQL / interrupts enabled
native cpu_restore_affinity(old[2]);

/// Read MSR.
///
/// @param msr MSR Index
/// @param value Output value
/// @return An NTSTATUS
native msr_read(msr, &value);

/// Write MSR.
///
/// @param msr MSR Index
/// @param value New value
/// @return An NTSTATUS
native msr_write(msr, value);

/// Disable interrupts, effectively raises IRQL to maximum.
native interrupts_disable();

/// Enable interrupts.
native interrupts_enable();

/// Read a byte from physical memory.
///
/// @param pa Physical address
/// @param value Value read
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_read_byte(pa, &value);

/// Read a word from physical memory.
///
/// @param pa Physical address
/// @param value Value read
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_read_word(pa, &value);

/// Read a double word from physical memory.
///
/// @param pa Physical address
/// @param value Value read
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_read_dword(pa, &value);

/// Read a quad word from physical memory.
///
/// @param pa Physical address
/// @param value Value read
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_read_qword(pa, &value);

/// Write a byte to physical memory.
///
/// @param pa Physical address
/// @param value Value to write
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_write_byte(pa, value);

/// Write a word to physical memory.
///
/// @param pa Physical address
/// @param value Value to write
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_write_word(pa, value);

/// Write a double word to physical memory.
///
/// @param pa Physical address
/// @param value Value to write
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_write_dword(pa, value);

/// Write a quad word to physical memory.
///
/// @param pa Physical address
/// @param value Value to write
/// @return An NTSTATUS
/// @note Only works on ranges already mapped by the kernel
native physical_write_qword(pa, value);

/// Map IO space.
///
/// Map physical memory to virtual, which can be accessed by the `virtual_*`
///   family of functions.
///
/// @param pa Physical address, page aligned
/// @param size Size to map, page aligned
/// @warning The mapping is a resource that must be freed (unmapped)
native io_space_map(pa, size);

/// Unmap IO space.
///
/// @param pa Physical address, page aligned
/// @param size Size to unmap, page aligned
native io_space_unmap(va, size);

/// Read a byte from virtual memory.
///
/// @param va Virtual address
/// @param value Value read
/// @return An NTSTATUS
native virtual_read_byte(va, &value);

/// Read a word from virtual memory.
///
/// @param va Virtual address
/// @param value Value read
/// @return An NTSTATUS
native virtual_read_word(va, &value);

/// Read a double word from virtual memory.
///
/// @param va Virtual address
/// @param value Value read
/// @return An NTSTATUS
native virtual_read_dword(va, &value);

/// Read a quad word from virtual memory.
///
/// @param va Virtual address
/// @param value Value read
/// @return An NTSTATUS
native virtual_read_qword(va, &value);

/// Write a byte to virtual memory.
///
/// @param va Virtual address
/// @param value Value to write
/// @return An NTSTATUS
native virtual_write_byte(va, value);

/// Write a word to virtual memory.
///
/// @param va Virtual address
/// @param value Value to write
/// @return An NTSTATUS
native virtual_write_word(va, value);

/// Write a double word to virtual memory.
///
/// @param va Virtual address
/// @param value Value to write
/// @return An NTSTATUS
native virtual_write_dword(va, value);

/// Write a quad word to virtual memory.
///
/// @param va Virtual address
/// @param value Value to write
/// @return An NTSTATUS
native virtual_write_qword(va, value);

// These are broken and don't return the result of the operation
//native virtual_cmpxchg_byte(va, exchange, comparand);
//native virtual_cmpxchg_word(va, exchange, comparand);
//native virtual_cmpxchg_dword(va, exchange, comparand);
//native virtual_cmpxchg_qword(va, exchange, comparand);

/// Allocate non-paged virtual memory.
///
/// @param size Size to allocate
/// @return Allocation virtual address
/// @retval 0 Out of memory
/// @warning The allocation is a resource that must be freed
native virtual_alloc(size);

/// Free a virtual allocation.
///
/// @param va Allocation virtual address
native virtual_free(va);

/// Read a byte from PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Output value
/// @return An NTSTATUS
native pci_config_read_byte(bus, device, function, offset, &value);

/// Read a word from PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Output value
/// @return An NTSTATUS
native pci_config_read_word(bus, device, function, offset, &value);

/// Read a double word from PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Output value
/// @return An NTSTATUS
native pci_config_read_dword(bus, device, function, offset, &value);

/// Read a quad word from PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Output value
/// @return An NTSTATUS
native pci_config_read_qword(bus, device, function, offset, &value);

/// Write a byte to PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Value to write
/// @return An NTSTATUS
native pci_config_write_byte(bus, device, function, offset, value);

/// Write a word to PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Value to write
/// @return An NTSTATUS
native pci_config_write_word(bus, device, function, offset, value);

/// Write a double word to PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Value to write
/// @return An NTSTATUS
native pci_config_write_dword(bus, device, function, offset, value);

/// Write a quad word to PCI configuration space.
///
/// @param bus Bus number
/// @param device Device number
/// @param function Function number
/// @param offset Byte offset
/// @param value Value to write
/// @return An NTSTATUS
native pci_config_write_qword(bus, device, function, offset, value);

/// Get virtual address of a kernel function.
///
/// @param name Function name
native get_proc_address(name[]);

/// Invoke an arbitrary function pointer.
///
/// @param address Function address
/// @param retval Return value
/// @param a0 Parameter to pass through
/// @param a1 Parameter to pass through
/// @param a2 Parameter to pass through
/// @param a3 Parameter to pass through
/// @param a4 Parameter to pass through
/// @param a5 Parameter to pass through
/// @param a6 Parameter to pass through
/// @param a7 Parameter to pass through
/// @param a8 Parameter to pass through
/// @param a9 Parameter to pass through
/// @param a10 Parameter to pass through
/// @param a11 Parameter to pass through
/// @param a12 Parameter to pass through
/// @param a13 Parameter to pass through
/// @param a14 Parameter to pass through
/// @param a15 Parameter to pass through
/// @return NTSTATUS of any exception that occurred, or STATUS_SUCCESS
native invoke(address, &retval, a0=0, a1=0, a2=0, a3=0, a4=0, a5=0, a6=0, a7=0, a8=0, a9=0, a10=0, a11=0, a12=0, a13=0, a14=0, a15=0);

/// Sleep.
///
/// @param us Microseconds to sleep
native microsleep(us);

/// Invoke DELL SMM.
///
/// @param in Input registers in order eax ecx edx ebx esi edi
/// @param out Output registers in order eax ecx edx ebx esi edi
/// @return Whether the request succeeded
native bool:query_dell_smm(const in[6], out[6]);

/// Write byte to IO port.
///
/// @param port Port to write to
/// @param value Value to write
native io_out_byte(port, value);

/// Write word to IO port.
///
/// @param port Port to write to
/// @param value Value to write
native io_out_word(port, value);

/// Write double word to IO port.
///
/// @param port Port to write to
/// @param value Value to write
native io_out_dword(port, value);

/// Read byte from IO port.
///
/// @param port Port to read from
/// @return Value read
native io_in_byte(port);

/// Read word from IO port.
///
/// @param port Port to read from
/// @return Value read
native io_in_word(port);

/// Read double word from IO port.
///
/// @param port Port to read from
/// @return Value read
native io_in_dword(port);

// Used to be AMD Lightweight Profiling, however it was discontinued and
//   removed from all supported CPUs due to Spectre.
//native llwpcb(addr);
//native slwpcb();

/// Execute cpuid.
///
/// @param leaf Passed in eax
/// @param subleaf Passed in ecx
/// @param out Output eax, ebc, ecx, edx
native cpuid(leaf, subleaf, out[4]);

/// Read control register.
///
/// @param cr Control register number to read
/// @return Value read
native cr_read(cr);

/// Write control register.
///
/// @param cr Control register number to write
/// @param value Value to write
native cr_write(cr, value);

/// Read debug register.
///
/// @param dr Debug register number to read
/// @return Value read
native dr_read(dr);

/// Write debug register.
///
/// @param dr Debug register number to write
/// @param value Value to write
native dr_write(dr, value);

/// Read extended control register.
///
/// @param xcr Extended control register number to read
/// @param value Value read
/// @return An NTSTATUS
native xcr_read(xcr, &value);


/// Write control register.
///
/// @param cr Control register number to write
/// @param value Value to write
/// @return An NTSTATUS
native xcr_write(xcr, value);

/// Invalidate TLB.
///
/// @param va Virtual address
native invlpg(va);

/// Invalidate TLB for PCID.
///
/// @param type Type
/// @param descriptor Pointer to descriptor
native invpcid(type, descriptor);

/// Read performance counter.
///
/// @param pmc Performance counter to read
/// @param value Value read
/// @return An NTSTATUS
native readpmc(pmc, value);

/// Read TSC.
///
/// @return TSC
native rdtsc();

/// Read TSC and Processor ID
///
/// @param pid Processor ID read
/// @return TSC
native rdtscp(&pid);

/// Invoke 64 bit RDRAND.
///
/// @param v Random number
/// @return Whether the call succeeded
native rdrand(&v);

/// Invoke 64 bit RDSEED.
///
/// @param v Random number
/// @return Whether the call succeeded
native rdseed(&v);

/// Load IDT.
///
/// @param limit Limit
/// @param base Base
native lidt(limit, base);

/// Store IDT.
///
/// @param limit Limit
/// @param base Base
native sidt(&limit, &base);

/// Load GDT.
///
/// @param limit Limit
/// @param base Base
native lgdt(limit, base);

/// Store GDT.
///
/// @param limit Limit
/// @param base Base
native sgdt(&limit, &base);

/// Read MXCSR.
///
/// @return MXCSR
native mxcsr_read();

/// Write MXCSR.
///
/// @param v MXCSR
native mxcsr_write(v);

/// Set AC in EFLAGS.
native stac();

/// Clear AC in EFLAGS.
native clac();

/// Invoke HALT.
native halt();

/// Invoke UD2.
native ud2();

/// Invoke INT3.
native int3();

/// Invoke INT 2C.
native int2c();

/// Write Back and Invalidate Cache.
native wbinvd();
