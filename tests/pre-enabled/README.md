# Isolated tests for pre-enabled SMBus transfers

The complete I801 and PIIX4 AMX modules run `main` and
`ioctl_xfer_pre_enabled` using the Pawn 4.1.7152 C interpreter. Native PCI,
MMIO, port I/O and timing operations are simulated; the harness does not open
a device, invoke kernel functions or access hardware.

Each module has 31 cases: success, pre/internal disabled decoding, individual
PCI read failures, unsupported protocol, controller transaction failure,
simultaneous primary/post failures, changed identity/Command, and 15 malformed
requests. Assertions cover the phase statuses, read counts, output bounds,
payload absence on failure and zero PCI writes on tested new-entry paths.
The success transfer is SMBus byte-data read, not complete protocol coverage.

Build on Windows x64 with CMake and MSVC, using the original compiler sources
extracted from `_pawn/pawn-4.1.7152-1.el9.src.rpm` (Git blob
`7f9d43cd80aeb141d272efad28ae6253ef3a15cf`). Extract the contained ZIP too,
so PAWN_SOURCE contains `compiler/` and `amx/`. Source is unchanged; the recipe
omits the Windows icon/version resource absent from that source archive.

```powershell
cmake -S tests/pre-enabled -B build-review -G 'Visual Studio 18 2026' -A x64 '-DPAWN_SOURCE=C:/path/to/extracted/pawn'
cmake --build build-review --config Release --parallel 2
./build-review/Release/pawncc.exe SmbusI801.p -iinclude -C64 '-;+' '-(+' -p
./build-review/Release/pawncc.exe SmbusPIIX4.p -iinclude -C64 '-;+' '-(+' -p
./build-review/Release/spd_amx_verify.exe SmbusI801.amx i801
./build-review/Release/spd_amx_verify.exe SmbusPIIX4.amx piix4
```

Both runs must exit zero with `passed=31`. An optional fourth argument writes
valid-envelope test vectors to a new file (decimal scenario, hex outer status,
14 request cells and 21 response cells), useful for independent client tests.
The runtime uses AMX64, the ANSI interpreter, no dynamic loading and no packed
opcodes; both compiled modules initialize and execute under those settings.
This avoids the unused packed-opcode path's 32-bit unsigned-long shift on
Windows. These tests do not establish signed-driver compatibility, hardware
timing, concurrency or DDR5 paging.

For a legacy ABI differential, also compile the unmodified upstream modules
in a separate directory with the same toolchain. Run this harness on both
baseline and candidate AMX using `legacy-i801` or `legacy-piix4` as argument 2,
and compare complete stdout. Ten cases per module cover byte/word reads,
send-byte/byte-data writes, process call (unsupported on PIIX4), malformed
direction/address/payload, controller failure and disabled PCI enable attempt.
The latter stub returns an error instead of performing any write. Both modules
produced equal baseline/candidate records for status, payload, read/write counts
and transaction starts. This is bounded regression evidence, not all-protocol
equivalence or hardware validation.

## V1 wire contract

Input is exactly 14 64-bit cells: version 1, nonzero caller nonce, expected BDF
`(bus<<8)|(device<<3)|function`, expected vendor/device DWORD, legacy input
count 4..9, and nine legacy input cells with unused cells zero.
The controller is independently discovered by the original module; the BDF
is checked, never used to authorize an arbitrary PCI target.

Output is exactly 21 cells:

| Cells | Meaning |
|---|---|
| 0..3 | version, echoed nonce, discovered BDF, required PCI Command bit mask |
| 4 | attempted phases: pre ID=1, pre Command=2, main=4, post ID=8, post Command=16 |
| 5,6 | pre ID status/value |
| 7,8 | pre Command status/value |
| 9 | main transfer status |
| 10,11 | post ID status/value |
| 12,13 | post Command status/value |
| 14 | post ID equals expected ID and post Command equals pre Command |
| 15 | legacy output cell count (5 on main success, 0 otherwise) |
| 16..20 | legacy result cells |

Outer success only means an envelope is present. Untaken phases have
DEVICE_NOT_READY status and clear attempt flags. A caller accepts transfer
success only with matching version/nonce/BDF/IDs/mask, flags=31, successful
statuses, enabled pre bit, equal pre/post Command and valid protocol payload.
Main failure is preserved even if either post observation also fails. Matching
post observations do not assert successful transfer or atomic exclusion of
firmware/other clients. A mismatched or missing post observation must stop
acquisition, never fall back to the legacy API.

The existing global SMBus mutex remains required over the complete operation.
This API does not authorize EEPROM writes, relax application restrictions or
replace page/PMIC restoration. It avoids PCI Command writes only on the new
entry; the unchanged legacy export retains its existing behavior. Disabled
controllers remain unsupported by the new entry, and PIIX4's protocol set is
unchanged. No hardware validation is claimed.
