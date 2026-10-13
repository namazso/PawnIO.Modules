# Intel memory observation candidates

Status: draft for upstream review and eventual signing. Neither module has been
loaded by a driver or validated against real hardware. The consuming Rust
application keeps both module assets absent, so these paths remain disabled.
This proposal is not a request to distribute an unreviewed binary.

## Scope

`IntelDdr5.p` implements bounded observation transactions for the Intel i801-style
SMBus controller with vendor/device `8086:7F23`, SMBus class `0C05`, at segment-zero
bus 0 or 128, device 31/function 4. It rejects ambiguity, disabled memory decode,
and disabled SMBus host mode. It never enables PCI decode. Other controllers
require separate register/ownership qualification; matching a vendor is not proof.

`IntelCapid.p` reads only host bridge `00:00.0`, offsets `0xE4`, `0xE8`, `0xEC`,
`0xF0`, after an Intel/nonzero-device check, and rechecks identity. It exposes raw
CAPID A/B/C/E words, no layout interpretation, arbitrary PCI API or write. A device
identity change after initialization rejects the next acquisition. Generation
support and reserved-register behavior still need qualification before release.

## DDR5 request and receipt

The single export is `ioctl_ddr5`, with four 64-bit input cells:
`[sequence, operation, slot, argument]`. Sequence starts at one and increments
once per accepted request; slots are 0..7. A rejected request does not consume it.

| Operation | Argument | Observation |
| --- | --- | --- |
| 0 | slot=0, argument=0 | Controller identity, no SMBus transfer |
| 1 | 0 | SPD5118 and PMIC identity, VR-enable state, original MR11 |
| 2 | page 0..7 | 128 SPD bytes; restore original MR11 |
| 3 | selector 0,1,2,3,5,8,9 | Raw PMIC ADC with selector restoration; qualification pending |
| 4 | 0 | SPD5118 capability/configuration and optional temperature |

Slots map to SPD `0x50+slot` and PMIC `0x48+slot`, not firmware DIMM/channel names.

| Output cells | Meaning |
| --- | --- |
| 0,1 | Magic `0x004D435244445235`, schema 2 |
| 2..5 | Exact request echo |
| 6,7 | Primary and cleanup NTSTATUS, separately sign-extended |
| 8,9 | Cleanup confirmed (0/1), stage |
| 10,11 | Subsystem ID in high32 + PCI ID in low32; bus/device/function |
| 12,13 | Before/after CNT,CMD,ADDR,DATA0,DATA1,AUX packed into six bytes |
| 14,15 | Payload byte length; reserved zero |
| 16..31 | Packed little-endian payload, remaining bytes zero |

Transport success means a receipt was delivered. Either nonzero embedded status
rejects the values. Payload is cleared after an acquisition or cleanup error.
Only a first-address NACK with confirmed cleanup may mean an unresponsive slot;
a partial identity read failure is not an absent DIMM.

Temperature checks MR5/MR26, selects page zero and byte-address mode temporarily,
and reads MR49/MR50 in one SMBus Word Data transaction (controller protocol `0x0c`).
Both data bytes belong to that same transfer. Capability, configuration and hub
identity are reread; MR11 is restored and verified. Unsupported and disabled
sensors do not issue a temperature transfer. The module never enables a sensor or
clears its alarms. The 24-byte payload contains:

| Bytes | Meaning |
| --- | --- |
| 0..4 | Hub identity before |
| 5,6,7 | Capability, configuration, temperature-present flag |
| 8,9 | Raw little-endian temperature word, or zero when absent |
| 10,11 | Capability/configuration after |
| 12,13 | Original/restored MR11 |
| 14..18 | Actual hub identity after |
| 19..23 | Reserved zero |

The hub must be identifiable in its original addressing/page state before any
selector change. This may exclude real devices; the code does not write unknown
devices to discover their identity.

## Ownership and restoration

Callers must hold `Access_SMBUS.HTP.Method` across the entire session. The module
also uses the hardware INUSE semaphore and snapshots six controller registers.
It does not clear foreign INUSE or abort foreign traffic. Restore is attempted
even when the acquisition write reports failure, and all restoration failures
remain failures even if a later read matches. Failed cleanup retains ownership
and refuses further requests/unload. This does not prove recovery after forced
process termination, OS failure, firmware concurrency, or power loss.

These are observation operations, not physically write-free ones. DDR5 temporarily
writes transaction/control registers, SPD5118 MR11, and PMIC R30. It has no voltage
setting, EEPROM write, VR-enable write, or arbitrary peripheral write operation.
Each transfer has an 80 ms / 320-poll bound and each operation a 2500 ms cooperative
budget. Recovery is still attempted after the budget. A blocked kernel native
cannot be preempted by these bounds. CAPID has no register writes or mappings.

## Outstanding acceptance work

- Architecture review: related discussion in PR #113 favors existing SmbusI801
  access with caller restrictions. This proposal additionally puts selector and
  controller restoration and a terminal receipt in the module. Maintainer advice
  on integrating those changes into SmbusI801 instead is welcome.
- PMIC operation 3 is a research candidate. Its Richtek manufacturer/VR-enable
  checks do **not** prove RTQ5119A model identity or a valid rail mapping. The
  consuming application currently has no qualified PMIC profiles and dispatches
  no ADC calls. Public module consumers cannot rely on that application's gate;
  exact model admission or removal of operation 3 must be resolved before signing.
- Controller layout, semaphore behavior, host bridge generation, peripheral
  identity, cleanup/unload, firmware concurrency and failure behavior require
  hardware qualification. No live SPD, temperature, PMIC voltage, or CAPID sample
  is claimed; no cross-machine, suspend/TDR, or long-duration test is claimed.
- Local synthetic VM checks are author evidence, not an independent audit or a
  replacement for the PawnIO kernel interpreter and native behavior.

## Reproduce offline checks

Prerequisites: Python 3.10+ and Pawn compiler 4.1.7152. No third-party Python
packages, driver, elevated privileges, or system-setting changes are required.
From this repository:

```powershell
python -B tests/intel-memory/run_offline.py --compiler C:/path/to/pawncc.exe
```

The runner uses the repository's includes. It compiles both modules with the
existing CI flags, then with `-O0` for the limited VM, and replays real AMX bytes.
It does not load an AMX into a driver. Generated files stay in ignored `out/`.
Thirty DDR5 cases cover restoration, native failures, temperature protocol and
state, unknown PMIC manufacturer, timing and ownership. Nine CAPID groups cover
133 raw patterns, exact packed dwords/zero tail, fixed native/export allowlists,
request sizes, sequences, failure preservation and identity drift. Fourteen
compiled behavior mutations must fail their assertions; build failures are not
counted as effective mutations.

The VM implements only exercised instructions with a fixed instruction budget.
Its opcode reference is frozen by SHA-256 in `amx_runtime.py`; it is derived from
the accompanying PawnPP `amx.h`, with its MPL-2.0 license retained. The Python
test harness and added modules are LGPL-2.1-or-later under the repository COPYING.
The upstream i801 attribution in the module is retained. No datasheet or driver
binary is redistributed here.

## References

- [PawnIO SmbusI801 at base 52a7e536](https://github.com/namazso/PawnIO.Modules/blob/52a7e536dff3e53c96917a28caac5e0fa6510696/SmbusI801.p): controller register/transfer reference.
- [Linux SPD5118 reference](https://github.com/torvalds/linux/blob/master/drivers/hwmon/spd5118.c): register and protocol facts; its implementation was not copied.
- [Richtek RTQ5119A DSQ5119A-02](https://www.richtek.com/assets/product_file/RTQ5119A/DSQ5119A-02.pdf): R30/R31 ADC and selector reference, not proof of the installed PMIC.
- [Intel CAPID A](https://edc.intel.com/content/www/th/th/design/publications/13th-generation-core-processors-datasheet-volume-2-of-2/capabilities-a-capid0-a-0-0-0-pci-offset-e4/) and [CAPID E](https://edc.intel.com/content/www/kr/ko/design/publications/13th-generation-core-processor-datasheet-volume-2-of-2/capabilities-e-capid0-e-0-0-0-pci-offset-f0/): fixed host-bridge capability registers, not live voltages.
