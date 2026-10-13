# SPDX-License-Identifier: LGPL-2.1-or-later
"""Check real compiled CAPID bytecode using synthetic CPUID and fixed PCI reads."""
import argparse
import hashlib
import json
from pathlib import Path
import struct

from amx_runtime import MASK, Vm, status

OFFSETS = (0xE4, 0xE8, 0xEC, 0xF0)
DEVICE = 0x7D018086
IO_ERROR = 0xC0000185
IDENTITY_ERROR = 0xC0000182


class CapidVm(Vm):
    def __init__(self, blob, values=(0x12345678, 0xABCDEF01, 0x87654321, 0xFFAA5500)):
        super().__init__(blob, {})
        self.values = values
        self.identity = DEVICE
        self.reads = []
        self.events = []
        self.failure_at = None
        self.identity_after_values = None

    def native(self, index, args):
        name = self.natives[index]
        self.events.append((name, args))
        if name in ('get_arch', 'cpuid'):
            return super().native(index, args)
        assert name == 'pci_config_read_dword', (name, args)
        bus, device, function, offset, pointer = args
        assert (bus, device, function) == (0, 0, 0)
        assert offset in (0, *OFFSETS)
        self.reads.append(offset)
        if len(self.reads) == self.failure_at:
            return status(IO_ERROR)
        value = self.identity if offset == 0 else self.values[OFFSETS.index(offset)]
        self.put(pointer, value)
        if offset == OFFSETS[-1] and self.identity_after_values is not None:
            self.identity = self.identity_after_values
        return 0

    def exchange(self, request=(1, 0, 0, 0), input_size=4, output_size=32):
        start = (self.heap + 15) & ~7
        output = start + 64
        self.heap = output + 256 + 64
        for index, value in enumerate(request):
            self.put(start + index * 8, value)
        for index in range(32):
            self.put(output + index * 8, MASK)
        result = self.run('ioctl_capid', (start, input_size, output, output_size))
        raw = bytes(self.mem[output:output + 256])
        return result, struct.unpack('<32Q', raw), raw


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--module-dir', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    blob = (args.module_dir / 'IntelCapid.amx').read_bytes()
    cases = []
    vm = CapidVm(blob)
    assert set(vm.publics) == {'ioctl_capid', 'unload'}
    assert set(vm.natives) <= {'get_arch', 'cpuid', 'pci_config_read_dword'}
    cases.append('only-fixed-pci-read-native-and-capid-export')

    patterns = [tuple([value] * 4) for value in (0, 0xFFFFFFFF, 0x80000000, 0xAAAAAAAA)]
    patterns += [(0x12345678, 0xABCDEF01, 0x87654321, 0xFFAA5500)]
    patterns += [tuple((1 << bit) if lane == index else 0 for lane in range(4))
                 for index in range(4) for bit in range(32)]
    for values in patterns:
        vm = CapidVm(blob, values)
        assert vm.run() == 0
        result, words, raw = vm.exchange()
        assert result == 0 and words[0:6] == (0x004D434150494432, 1, 1, 0, 0, 0)
        assert words[6:9] == (0, 0, 1)
        assert words[10:15] == (DEVICE, 0, DEVICE, DEVICE, 16)
        assert raw[128:144] == struct.pack('<4I', *values), (values, raw[128:144].hex())
        assert not any(raw[144:]), 'nonzero reserved tail'
        assert vm.reads == [0, 0, *OFFSETS, 0]
        assert vm.run('unload') == 0
    cases.append('133-patterns-preserve-all-four-dwords-and-zero-tail')

    invalid = [(0, 0, 0, 0), (2, 0, 0, 0), (0xFFFFFFFF, 0, 0, 0),
               (1 << 32, 0, 0, 0), (MASK, 0, 0, 0), (1, 1, 0, 0),
               (1, 0, 1, 0), (1, 0, 0, 1), (1, 1 << 32, 0, 0)]
    for request in invalid:
        vm = CapidVm(blob)
        assert vm.run() == 0
        count = len(vm.events)
        result, _, _ = vm.exchange(request)
        assert result == status(0xC000000D) and len(vm.events) == count
    cases.append('invalid-request-zero-native-dispatch')
    for input_size, output_size in [(3, 32), (5, 32), (4, 31), (4, 33)]:
        vm = CapidVm(blob)
        assert vm.run() == 0
        count = len(vm.events)
        result, _, _ = vm.exchange(input_size=input_size, output_size=output_size)
        assert result == status(0xC000000D) and len(vm.events) == count
    cases.append('incorrect-buffer-size-zero-native-dispatch')

    vm = CapidVm(blob)
    assert vm.run() == 0 and vm.exchange()[0] == 0
    count = len(vm.events)
    assert vm.exchange()[0] == status(0xC000000D) and len(vm.events) == count
    assert vm.exchange((2, 0, 0, 0))[1][6] == 0
    cases.append('sequence-cannot-repeat-or-skip')

    for failure_at in range(2, 8):
        vm = CapidVm(blob)
        assert vm.run() == 0
        vm.failure_at = failure_at
        result, words, raw = vm.exchange()
        assert result == 0 and words[6] == status(IO_ERROR) and words[7] == 0
        assert not any(raw[128:]) and len(vm.reads) == failure_at
    cases.append('each-pci-failure-preserves-status-and-discards-payload')

    vm = CapidVm(blob)
    assert vm.run() == 0
    vm.identity = 0x46688086
    result, words, raw = vm.exchange()
    assert result == 0 and words[6] == status(IDENTITY_ERROR) and not any(raw[128:])
    assert vm.reads == [0, 0]
    cases.append('identity-change-before-read-rejects-before-capid-registers')

    vm = CapidVm(blob)
    assert vm.run() == 0
    vm.identity_after_values = 0x46688086
    result, words, raw = vm.exchange()
    assert result == 0 and words[6] == status(IDENTITY_ERROR) and not any(raw[128:])
    cases.append('identity-change-after-read-is-an-explicit-failure')

    for identity in [0xFFFFFFFF, 0x7D011022, 0x00008086]:
        vm = CapidVm(blob)
        vm.identity = identity
        assert vm.run() == status(0xC00000BB)
    cases.append('missing-or-non-intel-host-refused')
    args.output_dir.mkdir(parents=True, exist_ok=True)
    report = {'synthetic_only': True, 'native_hardware_calls': 0,
              'amx_sha256': hashlib.sha256(blob).hexdigest(),
              'payload_patterns': len(patterns), 'case_count': len(cases), 'cases': cases}
    (args.output_dir / 'capid-results.json').write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
    print(json.dumps(report))


if __name__ == '__main__':
    main()
