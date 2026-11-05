# SPDX-License-Identifier: LGPL-2.1-or-later
"""Extract actual Pawn control flow into a fake-register C++ model, never a driver.

This is not Pawn VM execution or hardware validation. The separate build job
compiles the complete Pawn module without loading it.
"""
import argparse
import re
import struct
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent


def extract(source, name):
    match = re.search(r'NTSTATUS:' + name + r'\([^)]*\)\s*\{', source)
    if not match:
        raise ValueError('Missing function: ' + name)
    depth = 1
    end = match.end()
    while depth:
        depth += (source[end] == '{') - (source[end] == '}')
        end += 1
    return source[match.start():end]


def translate(source):
    signatures = {
        'i801_inuse': 'NTSTATUS i801_inuse(bool inuse)',
        'i801_check_pre': 'NTSTATUS i801_check_pre()',
        'i801_access_simple': 'NTSTATUS i801_access_simple(cell addr, cell read_write, cell command, cell size, cell in, cell& out)',
        'i801_access_block': 'NTSTATUS i801_access_block(cell addr, cell read_write, cell command, cell size, cell in[33], cell out[33])',
    }
    functions = []
    for name, signature in signatures.items():
        body = extract(source, name)
        body = re.sub(r'NTSTATUS:' + name + r'\([^)]*\)', signature, body, count=1)
        body = body.replace('new NTSTATUS:', 'NTSTATUS ').replace('new bool:', 'bool ')
        body = re.sub(r'\bnew (\w+)', r'cell \1', body)
        body = re.sub(r"''(.*?)''", lambda m: '"' + m[1] + '"', body, flags=re.S)
        # Pawn switch arms do not fall through; the selected functions have
        # one multi-label transaction arm followed by an explicit goto default.
        body = re.sub(r'case ([\w, ]+):',
                      lambda m: ' '.join('case ' + v.strip() + ':' for v in m[1].split(',')), body)
        body = body.replace('default:', 'break;\n        default:')
        functions.append(body)
    constants = source[source.index('/* I801 Hosts Status register bits */'):source.index('// PCI slot addresses')]
    return constants + '\n' + '\n\n'.join(functions)


def run(cl, source, name):
    build = HERE / 'build'
    build.mkdir(exist_ok=True)
    src, exe = build / (name + '.cpp'), build / (name + '.exe')
    src.write_text((HERE / 'test_prefix.cpp').read_text() + '\n' + translate(source)
                   + '\n' + (HERE / 'test_suffix.cpp').read_text())
    result = subprocess.run([
        str(cl), '/nologo', '/std:c++17', '/W4', '/WX', '/GS-', '/Zl', '/Od',
        str(src), '/Fo' + str(build / (name + '.obj')), '/Fe' + str(exe),
        '/link', '/nodefaultlib', '/entry:main', '/subsystem:console',
    ], capture_output=True, text=True)
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)
    data = exe.read_bytes()
    pe = struct.unpack_from('<I', data, 0x3c)[0]
    optional = pe + 24
    assert data[pe:pe+4] == b'PE\0\0'
    assert struct.unpack_from('<H', data, optional)[0] == 0x20b
    for index in (1, 13):  # import and delay-import directories
        assert struct.unpack_from('<II', data, optional + 112 + 8*index) == (0, 0)
    return subprocess.run([str(exe)], timeout=10).returncode


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cl', required=True, type=Path)
    args = parser.parse_args()
    source = (HERE.parents[1] / 'SmbusI801.p').read_text()
    assert 'ioctl_startup_recovery' not in source
    assert 'SmbusI801StartupRecovery.inc' not in source
    assert run(args.cl, source, 'baseline') == 0
    print('PASS: 14 scenarios on each of simple/block paths (28 cases); no DLL imports')
    mutations = [
        ('restore_timeout', "debug_print(''SMBus INUSE did not clear; continuing after timeout\\n'');",
         "debug_print(''SMBus INUSE did not clear; continuing after timeout\\n'');\n            return STATUS_IO_TIMEOUT;"),
        ('shorten_wait', 'get_tick_count() + MAX_TIMEOUT', 'get_tick_count() + 1'),
        ('skip_preflight', 'status = i801_check_pre();', 'status = STATUS_SUCCESS;'),
        ('ignore_busy', 'return STATUS_DEVICE_BUSY;', 'return STATUS_SUCCESS;'),
        ('skip_release', 'if (acquired)', 'if (!acquired)'),
        ('skip_kill', '        i801_kill();', '        /* missing kill */'),
    ]
    for name, before, after in mutations:
        assert before in source, name
        code = run(args.cl, source.replace(before, after), name)
        assert code != 0, 'Escaped mutation: ' + name
        print('PASS: rejected ' + name)


if __name__ == '__main__':
    main()
