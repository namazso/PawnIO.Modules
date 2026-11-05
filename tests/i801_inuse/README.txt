i801 best-effort INUSE acquisition: offline regression tests

SmbusI801.p retains the existing 80 ms grace period for BIOS/ACPI to release
INUSE, but no longer returns STATUS_IO_TIMEOUT solely because INUSE remains set.
The existing HOST_BUSY preflight check still rejects an active controller before
any transaction, and the normal cleanup remains in place. This follows Steve-Tech's
review: https://github.com/namazso/PawnIO.Modules/pull/137#issuecomment-6646466957

There is no new ioctl, board whitelist, early forced clear or caller opt-in.
The change applies to every controller already supported by SmbusI801, not only
the ASUS system that motivated the report. The former recovery helper is removed.

The original acquisition, preflight, simple-access and block-access function
bodies are extracted from SmbusI801.p, mechanically translated to C++ and linked
to fake status registers, time, transaction and error-mapping functions. Pawn
switch syntax is translated with explicit breaks; cells use 64-bit integers.
The fake status register models INUSE read-to-acquire and write-one-to-clear.
The test executable must have no import or delay-import directory before running.

Run with Python 3 and MSVC x64 (a full cl.exe path also works):
  python tests/i801_inuse/run_tests.py --cl cl.exe

Fourteen scenarios run for each of simple and block access (28 cases): free bus,
transient INUSE, persistent INUSE/full wait, subsequent transfer, persistent busy,
initial busy without INUSE, busy appearing at preflight, release at deadline,
auxiliary read/write errors, transfer failure, completion error, unsupported
protocol and stale status flags. Six mutations must fail: restoring the timeout,
shortening the wait, skipping preflight, ignoring busy, skipping release or KILL.

Important limits:
- No driver, RAM, SPD or actual SMBus access. This is not Pawn VM execution.
- A separate CI job compiles the complete actual module with upstream Pawn
  4.1.7152 and C64 flags, rejects compiler warnings, and hashes the unsigned AMX.
  Neither that artifact nor any test module is installed or loaded.
- The existing release path writes INUSE | STATUS_FLAGS even when preflight
  returns HOST_BUSY after the best-effort wait. Tests document that behavior;
  they do NOT establish ownership of the semaphore or safety of clearing it.
- HOST_BUSY is a snapshot, not synchronization with firmware. Linux also has
  separate ACPI coordination, so similarity to Linux is not a Windows safety proof.
- Existing MMIO error handling and transaction implementations are unchanged;
  fake transactions do not validate actual bus timing, KILL, or error mapping.
- No candidate hardware validation has been performed. Keep the PR in draft
  pending review and an agreed signed-module trial with the official driver.

AI provenance: code/tests prepared by OpenAI Codex on behalf of @aluzin at the
user's explicit request. The original submission reported gpt-6-astra, xhigh,
Codex runtime 0.164.0-alpha.11.1. Independent human hardware-code review remains
outstanding; tests are not a substitute for review or hardware validation.

Added test code: LGPL-2.1-or-later, consistent with COPYING.
