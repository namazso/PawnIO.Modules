# PawnIO Modules (Softhe fork)

Modules for [PawnIO](https://pawnio.eu), compiled with Pawn `4.1.7152` (`pawncc -C64 -;+ -(+ -p`, see `.github/workflows/ci.yml` — CI is green with no warnings).

> **Fork notice.** This is `Softhe/PawnIO.Modules`, a staging fork of the upstream
> [namazso/PawnIO.Modules](https://github.com/namazso/PawnIO.Modules) (base `52a7e53` = upstream `0.2.11`).
> Staging PR upstream: [namazso#115](https://github.com/namazso/PawnIO.Modules/pull/115) (draft, cherry-pick per area).
> XHCI relates to upstream [namazso#111](https://github.com/namazso/PawnIO.Modules/pull/111) (draft).
> Signed drivers ship only via upstream Releases; assets here are **unsigned CI `.amx` builds**.
> Upstream docs: [Wiki](https://github.com/namazso/PawnIO.Modules/wiki).

## Releases

- `0.2.14` (current): `0.2.13` + explicit MMIO straddle checks + this README.
- `0.2.13`: XHCI five-ID module, Family10h multiplier rework, 7-issue audit hardening.
- `XhciImodAmd-test1` (pre-release, superseded for the module itself): test harness
  (`pawnio-module-test.ps1`, `rust-remake-src.tar.gz`) — still useful for hardware validation.

## Breaking changes since upstream 0.2.11 (consumers must update)

- `AMDFamily10 ioctl_measure_tsc_multiplier`: `out_size 2 → 5`, order
  `[COFVID, ctrl_TSC, ctrl_CTR, meas_TSC, meas_CTR]`. Old 2-cell callers fail the size check.
- `DellSMM ioctl_query_smm`: allowlist `0x0025/a069/00a3/02a3/03a3/05a3/10a3/11a3/fea3/ffa3` only.
  Fan-set `0x01a3` and auto-fan `0x30a3/31a3/34a3/35a3` now `ACCESS_DENIED`.
- `LpcIO ioctl_superio_outb`: writes to `0x07/0x30/0x60–0x63` (LDSEL/ENABLE/BARs) denied —
  they allowed BAR-repoint → `find_bars` PIO-allowlist escape. Reads unchanged.
- `SmbusI801/PIIX4/SkylakeIMC xfer`: address `0` (General Call broadcast) now `INVALID_PARAMETER`.
- `AMDFamily17 ioctl_read_smn`: unaligned offsets now `INVALID_PARAMETER`.
- `AMDFamily10 ioctl_read_miscctl`: `cpu_idx` clamped `0..7` (devices 24–31).

## Trust model (admin-only)

These modules run in kernel; only trusted admin callers may open them. Broad-by-design,
documented per-ioctl: Family17 SMN arbitrary aligned reads (full allowlist TODO),
CrOS EC arbitrary commands (version `0..FF`, command `0..FFFF` validated; set itself open),
Intel/AMD MSR OC writes (`0x150/0x601/0x607/0x608/0x610`, PSTATE/HWCR/LS_CFG…).
Do not expose to untrusted callers. Hardware validation on real AMD/Dell/CrOS/XHCI boxes
still needed — CI only compiles.

## Toolchain note

Vendored `_pawn/pawn-4.1.7152` matches GitHub `compuphase/pawn` latest, but the project has
moved to Codeberg with `4.1.7487` (2025-08-25). Follow-up: evaluate rebuilding the EL9 RPM
from Codeberg source and re-validating `.amx` output before adopting.
