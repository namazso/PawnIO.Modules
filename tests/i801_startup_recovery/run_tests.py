# SPDX-License-Identifier: LGPL-2.1-or-later
"""Offline candidate logic tests; no PawnIO binary is compiled or executed.

Extract the actual Pawn helper body and mechanically translate cell declarations
to C++. All register operations resolve to fake functions. Verify PE has no DLL
imports before execution. This does not prove firmware coexistence or safety.
"""
import argparse
import re
import struct
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent

def translate(source):
    source = source[:source.index('DEFINE_IOCTL_SIZED')]
    source = source.replace('NTSTATUS:i801_startup_recovery(ack, &result, &last_hststs)',
                            'NTSTATUS i801_startup_recovery(cell ack, cell& result, cell& last_hststs)')
    source = source.replace('new NTSTATUS:', 'NTSTATUS ').replace('new bool:', 'bool ')
    return re.sub(r'\bnew (\w+)', r'cell \1', source)

def run(cl, source, name):
    build = HERE / 'build'
    build.mkdir(exist_ok=True)
    src = build / (name + '.cpp')
    exe = build / (name + '.exe')
    src.write_text((HERE/'test_prefix.cpp').read_text() + '\n' + translate(source) +
                   '\n' + (HERE/'test_suffix.cpp').read_text())
    args = [str(cl), '/nologo', '/std:c++17', '/W4', '/WX', '/GS-', '/Zl', '/Od',
            str(src), '/Fo'+str(build/(name+'.obj')), '/Fe'+str(exe), '/link',
            '/nodefaultlib', '/entry:main', '/subsystem:console']
    p = subprocess.run(args, capture_output=True, text=True)
    if p.returncode:
        raise RuntimeError(p.stdout + p.stderr)
    data = exe.read_bytes()
    pe = struct.unpack_from('<I', data, 0x3c)[0]
    optional = pe+24
    assert data[pe:pe+4] == b'PE\0\0'
    assert struct.unpack_from('<H', data, optional)[0] == 0x20b
    for index in (1,13):
        assert struct.unpack_from('<II', data, optional+112+8*index) == (0,0)
    return subprocess.run([str(exe)]).returncode

if __name__ == '__main__':
    parser=argparse.ArgumentParser()
    parser.add_argument('--cl', required=True, type=Path)
    args=parser.parse_args()
    source=(HERE.parents[1]/'SmbusI801StartupRecovery.inc').read_text()
    assert run(args.cl,source,'baseline') == 0
    print('PASS: 21 scenarios; extracted candidate helper, fake registers, no DLL imports')
    mutations=[
        ('no_ack','if (ack != I801_RECOVERY_ACK)','if (ack == -1)'),
        ('repeat','if (i801_recovery_attempted)','if (false)'),
        ('wrong_controller','if (ids != 0x7F238086)','if (false)'),
        ('wrong_board','if (ids != 0x88EF1043)','if (false)'),
        ('disabled_mmio','if (!(ids & PCICMD_MMBIT))','if (false)'),
        ('busy_allowed','if (last_hststs != SMBHSTSTS_INUSE_STS)','if (false)'),
        ('clear_all_flags','virtual_write_byte(SMBHSTSTS, SMBHSTSTS_INUSE_STS)',
                           'virtual_write_byte(SMBHSTSTS, 0xFF)'),
        ('no_quiet_time','get_tick_count() + I801_RECOVERY_QUIET_MS','get_tick_count()'),
    ]
    for name,before,after in mutations:
        assert before in source
        rc=run(args.cl,source.replace(before,after),name)
        assert rc != 0, f'Escaped mutation: {name}'
        print(f'PASS: rejected {name}')
