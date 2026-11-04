EXPERIMENTAL RFC: persistent i801 INUSE recovery

This directory validates the helper included by SmbusI801.p. Nothing in normal
module initialization or SMBus transfers invokes the new ioctl. It is not an
automatic startup fix, and no application integration is included.

Why this is proposed:
An 8086:7F23 / 1043:88EF system repeatedly timed out in i801_inuse() after boot
with signed SmbusI801 0.2.11. One separately authorized vendor-driver HSTS clear
restored access with the unchanged signed module. The timeout recurred after a
reboot. That differential result motivates review; it is NOT a test of this code.

Hardware scope is intentionally limited to the observed controller/subsystem.
The operation is available to every caller; there is no application/vendor lock.
The acknowledgement parameter records opt-in, not authorization. Caller-held
Global\Access_SMBUS.HTP.Method only coordinates cooperating users. A quiet HSTS
byte cannot prove that firmware or another driver abandoned the semaphore, and
there remains a race between the final sample and forced clear. A module reload
also resets the attempt limit. These are outstanding design questions, not solved
by the tests. Do not enable this for unattended startup or install unsigned code.

Run the model tests from a Visual Studio x64 tools prompt:
  python tests/i801_startup_recovery/run_tests.py --cl cl.exe

The actual Pawn helper is mechanically translated to C++ and linked only to fake
PCI/MMIO/time functions. The produced PE must have zero import and delay-import
entries before it can be run. 21 cases cover hardware/ack/decode gates, failures,
busy/error/0xFF status, normal ownership/release, forced clear, and attempt reuse.
Eight intentionally broken variants must fail the tests.

This is a logic model, not Pawn bytecode execution or firmware validation.
No hardware API or driver is used by the model. The separate CI job compiles the
complete real SmbusI801.p with the pinned upstream Pawn 4.1.7152 compiler and C64
flags, rejects compiler warnings, and records the unsigned artifact hash. That
job never loads the module. Existing upstream compilation CI is unchanged.

AI provenance: prepared by OpenAI Codex on behalf of @aluzin at the user's
explicit request. Current session model gpt-6-astra, reasoning xhigh, Codex
runtime 0.164.0-alpha.11.1. Independent human hardware-code review is outstanding;
automated tests must not be represented as a substitute for that review.

Added code/test harness: LGPL-2.1-or-later, consistent with COPYING.

References:
https://github.com/namazso/PawnIO.Modules/wiki/Contribution-guidelines
https://github.com/namazso/PawnIO.Modules/blob/52a7e536dff3e53c96917a28caac5e0fa6510696/SmbusI801.p
https://edc.intel.com/content/www/us/en/design/publications/core-ultra-p200s-series-processors-soc-i-o-registers/host-status-register-address-hsts-offset-0/
