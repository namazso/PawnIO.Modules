# SPDX-License-Identifier: LGPL-2.1-or-later
"""Build the two candidates and test compiled AMX in a synthetic, bounded VM."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--compiler', type=Path, required=True)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    root = here.parent.parent
    output = args.output or here / 'out' / datetime.datetime.now().strftime('%Y%m%d-%H%M%S')
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    compiler = args.compiler.resolve(strict=True)
    base = [str(compiler), '-i' + str(root / 'include'), '-C64', '-;+', '-(+', '-p']
    results = []
    env = os.environ.copy()
    env['PYTHONDONTWRITEBYTECODE'] = '1'

    def run(name, command, directory, expected=0):
        p = subprocess.run(command, cwd=directory, env=env, capture_output=True, timeout=120)
        (directory / (name + '.stdout.log')).write_bytes(p.stdout)
        (directory / (name + '.stderr.log')).write_bytes(p.stderr)
        text = (p.stdout + p.stderr).decode('utf-8', 'replace')
        record = {'name': name, 'exit': p.returncode, 'expected_exit': expected,
                  'stdout_sha256': hashlib.sha256(p.stdout).hexdigest(),
                  'stderr_sha256': hashlib.sha256(p.stderr).hexdigest()}
        results.append(record)
        assert p.returncode == expected, (name, text)
        assert not re.search(r'warning\s+\d+', text, re.IGNORECASE), (name, text)
        print(name, p.returncode, flush=True)
        return text

    for flavor, flags in [('ci-default', []), ('replay', ['-O0'])]:
        directory = output / flavor
        directory.mkdir()
        for module in ['IntelDdr5', 'IntelCapid']:
            shutil.copyfile(root / (module + '.p'), directory / (module + '.p'))
            run('build-' + module, [*base, module + '.p', *flags], directory)
    replay = output / 'replay'
    for name, script in [('ddr5', 'ddr5_replay.py'), ('capid', 'capid_replay.py')]:
        run(name, [sys.executable, '-B', str(here / script), '--module-dir', str(replay),
                   '--output-dir', str(replay)], replay)

    ddr_cases = [
        ('skip-aux-restore', 'status = wr(g_registers[index], g_saved[index]);',
         'status = index == 5 ? STATUS_SUCCESS : wr(g_registers[index], g_saved[index]);'),
        ('skip-ambiguous-page-restore',
         'if (changed) cleanup_error(restore_selector(0x50+slot, 0x0b, original));\n    else {',
         'if (changed && NT_SUCCESS(status)) cleanup_error(restore_selector(0x50+slot, 0x0b, original));\n    else {'),
        ('discard-cleanup-status', 'if (!NT_SUCCESS(status) && NT_SUCCESS(g_cleanup)) g_cleanup = status;',
         'if (!NT_SUCCESS(status) && NT_SUCCESS(g_cleanup)) g_cleanup = STATUS_SUCCESS;'),
        ('leave-completed-flags', '''if (!(hststs & BUSY)) {
        if (!g_started) return STATUS_SUCCESS;
        // An earlier status read can fail after START while the bus later completes.
        // Clear only our terminal flags before restoring/releasing the controller.
        status = wr(STS, hststs & OWN_FLAGS);
        if (NT_SUCCESS(status)) g_started = false;
        return status;
    }''', 'if (!(hststs & BUSY)) return STATUS_SUCCESS;'),
        ('skip-temperature-capability',
         'new bool:present = (capability & 2) != 0 && (config & 1) == 0;',
         'new bool:present = (config & 1) == 0;'),
        ('skip-temperature-disable',
         'new bool:present = (capability & 2) != 0 && (config & 1) == 0;',
         'new bool:present = (capability & 2) != 0;'),
        ('temperature-byte-instead-of-word',
         'byte_transfer(0x50+slot, 0x31, false, value, true);',
         'byte_transfer(0x50+slot, 0x31, false, value, false);'),
        ('copy-hub-instead-of-reread', 'status = hub_identity(slot, after);',
         'for (new i; i < 5; i++) after[i]=hub[i];\n    status = STATUS_SUCCESS;'),
        ('ignore-temperature-state-drift', 'if (capability_after != capability || config_after != config)',
         'if (capability_after == 256 || config_after == 256)'),
        ('skip-ambiguous-temperature-restore',
         'if (changed) cleanup_error(restore_selector(0x50+slot, 0x0b, original));\n    new restored;',
         'if (changed && NT_SUCCESS(status)) cleanup_error(restore_selector(0x50+slot, 0x0b, original));\n    new restored;'),
    ]
    capid_cases = [
        ('capid-byte-position-regression', '''for (new byte_index; byte_index < 4; byte_index++) {
        new position = offset + byte_index;
        out[16 + position / 8] |= ((value >>> (byte_index * 8)) & 255) << ((position % 8) * 8);
    }''', '''for (new byte_index; byte_index < 4; byte_index++) {
        out[16 + byte_index + offset / 8] |= ((value >>> (byte_index * 8)) & 255) << ((offset % 8) * 8);
    }'''),
        ('capid-ignore-initial-identity', 'if (NT_SUCCESS(primary) && before != g_vendor_device)',
         'if (NT_SUCCESS(primary) && before < 0 && g_vendor_device < 0)'),
        ('capid-ignore-final-identity', 'if (NT_SUCCESS(primary) && after != before)',
         'if (NT_SUCCESS(primary) && after < 0)'),
        ('capid-repeat-sequence', 'sequence != g_sequence + 1',
         '(sequence == 0 && g_sequence == -1)'),
    ]
    for module, script, cases in [('IntelDdr5', 'ddr5_replay.py', ddr_cases),
                                   ('IntelCapid', 'capid_replay.py', capid_cases)]:
        original = (root / (module + '.p')).read_text(encoding='utf-8')
        for name, old, new in cases:
            directory = output / name
            directory.mkdir()
            assert original.count(old) == 1, (name, original.count(old))
            (directory / (module + '.p')).write_text(original.replace(old, new), encoding='utf-8', newline='\n')
            run('build-' + name, [*base, module + '.p', '-O0'], directory)
            text = run('reject-' + name, [sys.executable, '-B', str(here / script),
                       '--module-dir', str(directory), '--output-dir', str(directory)], directory, expected=1)
            assert 'AssertionError' in text and 'SyntaxError' not in text
    report = {'synthetic_only': True, 'native_hardware_calls': 0,
              'compiler_sha256': hashlib.sha256(compiler.read_bytes()).hexdigest(),
              'module_sources': {m: hashlib.sha256((root / (m + '.p')).read_bytes()).hexdigest()
                                 for m in ['IntelDdr5', 'IntelCapid']},
              'mutations_effective': len(ddr_cases) + len(capid_cases),
              'baseline_builds': 4, 'results': results}
    (output / 'results.json').write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
    print('All checks passed:', output, flush=True)


if __name__ == '__main__':
    main()
