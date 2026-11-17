# DDR5 read-only module: mock checks

This is an **unsigned, unvalidated hardware candidate**, not an installation recommendation. The checks execute the actual compiled `Ddr5ReadOnly.p` bytecode in a standalone 64-bit-cell Pawn VM. Every hardware native is a mock. No driver is loaded, and no hardware is accessed.

## Contract

The only hardware export, `ioctl_ddr5_read`, takes exactly four cells
`[address, direction, register, protocol]` and exactly one output cell.

| Address | Direction | Registers | Protocol |
| --- | --- | --- | --- |
| `0x50..0x57` | `1` (read) | `0x00`, `0x03`, `0x31` | `3` (word) |
| `0x50..0x57` | `1` (read) | `0x05`, `0x0b`, `0x1a` | `2` (byte) |

Validation uses full-width cells before any hardware native call. There is no generic SMBus transfer export or caller-supplied write payload. A client should check SPD5118 identity, capabilities, page and sensor state before interpreting the raw temperature register. This module does not identify individual DIMMs or convert temperatures for the caller.

"Read-only" describes peripheral transactions, not an absence of controller writes: the transport necessarily writes i801 address/command/control/status registers to issue a read and release its claim. It does not write peripheral configuration/data, change the SPD page, enable a sensor/controller, or write PCI configuration. The intended policy is enforced by module code executing inside PawnIO's kernel interpreter, rather than by trusting client-side validation.

This restriction applies to **this module's exported operations only**. Stock PawnIO may load other signed modules with broader capabilities. This is not a dedicated read-only driver or a system-wide write prohibition, and it does not protect against an administrator using a different driver/module.

## Reproduce on Windows without a driver

Prerequisites: PowerShell and an existing MinGW GCC/CMake toolchain. From the repository root:

```powershell
./tests/ddr5-readonly/build.ps1
# If the toolchain is not on PATH:
./tests/ddr5-readonly/build.ps1 -MinGwBin 'C:\toolchains\mingw64\bin'
# Reuse the verified local source archive without network access:
./tests/ddr5-readonly/build.ps1 -Offline
```

The script uses this checkout's `include/` and verifies the upstream Pawn 4.1.7152 source ZIP SHA256 before extraction. Build/cache output stays in `tests/ddr5-readonly/.build/` by default; `-CachePath` can select a dedicated cache directory. Nothing is installed, signed or deployed. The unsigned AMX hash is printed.

The CMake harness generates two compatibility corrections in its build directory: ineffective `const` on legacy void-return function-pointer declarations, and cell-width shifts for the 64-bit VM on Windows LLP64. It does not patch module bytecode. These checks are separate from the repository's standard Linux compiler CI; no claim of CI success is made by this script.

Checks cover 65,585 requests, including the 7-bit address/8-bit register space in read/write directions, invalid full-width values, mismatched buffer lengths/protocols, already-owned/busy controllers, disabled PCI decoding and a bounded transaction timeout. The harness checks the compiled public/native allowlists and requires denied requests to reach no hardware native. Allowed writes are checked against fixed controller offsets, command values and the forced read-direction bit.

## Outstanding acceptance work

- No execution in the Windows kernel or on actual hardware has been performed.
- Compatibility is not established by PCI vendor/class matching: current discovery is limited to Intel bus 0, device 31, function 4/3 with MMIO decoding and SMBus host mode already enabled.
- Real MMIO ordering, controller ownership/ACPI concurrency, read/abort failure cleanup, firmware interaction and suspend/resume need review and hardware validation.
- The caller must hold the shared `Access_SMBUS.HTP.Method` mutex; that is not protection from uncooperative firmware or other clients.
- Mock success does not prove kernel safety, correct readings, complete fault handling or universal hardware support. Do not test this candidate on a daily-use machine.

## References and licensing

- [PawnIO.Modules contribution requirements](https://github.com/namazso/PawnIO.Modules/wiki/Contribution-guidelines).
- [Upstream i801 transport reference, pinned base](https://github.com/namazso/PawnIO.Modules/blob/52a7e536dff3e53c96917a28caac5e0fa6510696/SmbusI801.p), copyright Steve-Tech, LGPL-2.1-or-later. Attribution is retained in the new module.
- [Linux SPD5118 protocol reference](https://github.com/torvalds/linux/blob/master/drivers/hwmon/spd5118.c), used as a register/protocol reference, not copied into this implementation.
- [Pawn compiler source](https://www.compuphase.com/pawn/pawn-4.1.7152.zip), pinned by SHA256 in the script; original upstream notices remain in the downloaded source.

The added module and mock-harness code are LGPL-2.1-or-later; see the repository's `COPYING`. Acceptance, any requested hardware testing, signing and release remain upstream decisions.
