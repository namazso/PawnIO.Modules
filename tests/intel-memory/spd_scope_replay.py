# SPDX-License-Identifier: LGPL-2.1-or-later
"""Test the SPD-only contract against compiled AMX, using memory callbacks only."""
import argparse
import hashlib
import json
from pathlib import Path
import struct

from amx_runtime import MASK, status
from ddr5_replay import Ddr5Vm


def exchange_raw(vm, request, input_size=4, output_size=32):
    start = (vm.heap + 15) & ~7
    output = start + 64
    vm.heap = output + 320
    for index, value in enumerate(request):
        vm.put(start + index * 8, value)
    for index in range(32):
        vm.put(output + index * 8, MASK)
    result = vm.run('ioctl_ddr5', (start, input_size, output, output_size))
    raw = bytes(vm.mem[output:output + 256])
    return result, struct.unpack('<32Q', raw)


def new_vm(blob, slot=0):
    vm = Ddr5Vm(blob)
    vm.spd_address = 0x50 + slot
    vm.pmic_present = False
    assert vm.run() == 0
    return vm


def spd_only(vm):
    assert all(address == vm.spd_address for address, _, _, _ in vm.transfers), vm.transfers
    assert all((address, reg) == (vm.spd_address, 0x0b)
               for address, reg, _ in vm.device_writes), vm.device_writes


def identity_without_pmic(blob):
    vm = new_vm(blob)
    result, words, data = vm.exchange(1)
    assert result == 0 and words[6:9] == (0, 0, 1), words
    spd_only(vm)
    assert vm.restored() and not vm.device_writes


def identity_receipt_has_no_invented_pmic(blob):
    vm = new_vm(blob)
    vm.pmic_present = True
    result, words, data = vm.exchange(1)
    assert result == 0 and words[1] == 3 and words[14] == 8, words
    assert data[:6] == bytes(vm.hub[:5]) + bytes([vm.hub[11]])
    assert not any(data[6:]), data.hex()
    spd_only(vm)


def removed_adc_is_rejected_before_native(blob):
    for slot in range(8):
        for selector in range(10):
            vm = new_vm(blob, slot)
            count = len(vm.events)
            result, words = exchange_raw(vm, (1, 3, slot, selector))
            assert result == status(0xc000000d), (slot, selector, result)
            assert len(vm.events) == count and words == (MASK,) * 32


def rejected_adc_does_not_consume_sequence(blob):
    vm = new_vm(blob)
    result, _ = exchange_raw(vm, (1, 3, 0, 0))
    assert result == status(0xc000000d)
    result, words = exchange_raw(vm, (1, 1, 0, 0))
    assert result == 0 and words[2] == 1 and words[6:9] == (0, 0, 1), words


def all_slots_need_only_their_spd_address(blob):
    for slot in range(8):
        vm = new_vm(blob, slot)
        for operation, argument, payload_size in [(1, 0, 8), (2, 7, 128), (4, 0, 24)]:
            result, words, data = vm.exchange(operation, slot, argument)
            assert result == 0 and words[6:9] == (0, 0, 1), (slot, operation, words)
            assert words[1] == 3 and words[14] == payload_size
            if operation == 2:
                assert data == vm.spd[7 * 128:8 * 128]
            spd_only(vm)
            assert vm.hub[11] == 0x0b and vm.restored()


def unsupported_hub_never_selects_page(blob):
    for operation in (1, 2, 4):
        vm = new_vm(blob)
        vm.hub[0] = 0x52
        result, words, data = vm.exchange(operation)
        assert result == 0 and words[6] == status(0xc00000bb) and words[7] == 0
        assert not any(data) and not vm.device_writes and vm.restored()
        spd_only(vm)


def request_bounds_preserve_output_and_sequence(blob):
    invalid = [(0, 0, 0, 0), (2, 0, 0, 0), (1 << 32, 0, 0, 0),
               (MASK, 0, 0, 0), (1, MASK, 0, 0), (1, 5, 0, 0),
               (1, 1, 8, 0), (1, 1, MASK, 0), (1, 2, 0, 8),
               (1, 2, 0, MASK), (1, 1, 0, 1), (1, 4, 0, 1),
               (1, 0, 1, 0), (1, 0, 0, 1)]
    for request in invalid:
        vm = new_vm(blob)
        count = len(vm.events)
        result, words = exchange_raw(vm, request)
        assert result == status(0xc000000d) and len(vm.events) == count, request
        assert words == (MASK,) * 32
        result, words = exchange_raw(vm, (1, 0, 0, 0))
        assert result == 0 and words[6:9] == (0, 0, 1), words


def invalid_sizes_do_not_dispatch(blob):
    for input_size, output_size in [(3, 32), (5, 32), (4, 31), (4, 33)]:
        vm = new_vm(blob)
        count = len(vm.events)
        result, words = exchange_raw(vm, (1, 1, 0, 0), input_size, output_size)
        assert result != 0 and len(vm.events) == count
        assert words == (MASK,) * 32


def selector_changes_preserve_unrelated_mr11_bits(blob):
    for original in (0, 7, 8, 15, 0x80, 0x8b, 0xf7, 0xff):
        vm = new_vm(blob)
        vm.hub[11] = original
        for page in (0, 7):
            result, words, data = vm.exchange(2, 0, page)
            assert result == 0 and words[6:9] == (0, 0, 1), (original, page, words)
            assert data == vm.spd[page * 128:(page + 1) * 128]
            assert vm.hub[11] == original and vm.restored()
            assert all(value & 0xf0 == original & 0xf0 for _, _, value in vm.device_writes)
            spd_only(vm)


def native_surface_stays_fixed(blob):
    vm = Ddr5Vm(blob)
    assert set(vm.publics) == {'ioctl_ddr5', 'unload'}
    assert set(vm.natives) <= {
        'get_arch', 'cpuid', 'pci_config_read_byte', 'pci_config_read_word',
        'pci_config_read_dword', 'pci_config_read_qword', 'io_space_map', 'io_space_unmap',
        'virtual_read_byte', 'virtual_write_byte', 'virtual_read_dword', 'virtual_read_qword',
        'microsleep',
    }, vm.natives


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--module-dir', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    blob = (args.module_dir / 'IntelDdr5.amx').read_bytes()
    cases = []
    for check in (identity_without_pmic, identity_receipt_has_no_invented_pmic,
                  removed_adc_is_rejected_before_native, rejected_adc_does_not_consume_sequence,
                  all_slots_need_only_their_spd_address, unsupported_hub_never_selects_page,
                  request_bounds_preserve_output_and_sequence, invalid_sizes_do_not_dispatch,
                  selector_changes_preserve_unrelated_mr11_bits, native_surface_stays_fixed):
        record = {'case': check.__name__}
        try:
            check(blob)
            record['passed'] = True
        except AssertionError as error:
            record.update(passed=False, error='AssertionError: ' + str(error))
        cases.append(record)
    report = {'synthetic_only': True, 'native_hardware_calls': 0,
              'compiled_amx_sha256': hashlib.sha256(blob).hexdigest(),
              'cases': cases, 'case_count': len(cases)}
    (args.output_dir / 'spd-scope-results.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report))
    raise SystemExit(0 if all(case['passed'] for case in cases) else 1)


if __name__ == '__main__':
    main()
