# SPDX-License-Identifier: LGPL-2.1-or-later
"""Compile the real module for review; never load it or open hardware."""
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
BUILD = HERE / 'build' / 'pawn'


def main():
    BUILD.mkdir(parents=True, exist_ok=True)
    shutil.copytree(ROOT / 'include', BUILD / 'include', dirs_exist_ok=True)
    for name in ('SmbusI801.p', 'sleepmode.inc'):
        shutil.copyfile(ROOT / name, BUILD / name)
    command = ['pawncc', 'SmbusI801.p', '-iinclude', '-C64', '-;+', '-(+', '-p']
    run = subprocess.run(command, cwd=BUILD, capture_output=True, text=True)
    log = run.stdout + run.stderr
    (BUILD / 'compiler.txt').write_text(log)
    print(log)
    if run.returncode or re.search(r'\bwarning [0-9]+:', log):
        raise RuntimeError('Pawn compiler failed or emitted warnings')
    data = (BUILD / 'SmbusI801.amx').read_bytes()
    if not data:
        raise RuntimeError('Empty module output')
    revision = subprocess.check_output(
        ['git', '-c', 'safe.directory=' + str(ROOT), 'rev-parse', 'HEAD'],
        cwd=ROOT, text=True).strip()
    result = dict(revision=revision, compiler_command=command,
                  bytes=len(data), sha256=hashlib.sha256(data).hexdigest(),
                  signed=False, hardware_loaded=False)
    (BUILD / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
