# ADR-0019: Nix, toolchain, and dev environment

**Status:** Accepted (amended)

## Context
All dependencies and tests through Nix; owner uses fenix. Originally
proposed dual-channel (stable default + nightly for fuzz/Miri); owner
amended to nightly-only.

## Decision (as amended)
- **Single pinned nightly** via fenix, with Miri and rust-analyzer from
  the same snapshot. **Policy: nightly toolchain, stable-compatible
  source** -- no `#![feature]` without a ratified ADR exception; dropping
  to stable stays a one-line flake change. Pin bumps are deliberate PRs
  with the T1 determinism suite as canary.
- **crane** for builds: per-crate caching, one shared deps derivation,
  one flake check per fast-tier test category. Nightly tier runs outside
  `flake check` on a schedule.
- **TargetTools seam:** external tools (Verilator, Yosys, solvers) reach
  tests via explicit configuration populated from store paths -- never
  PATH -- making the T2 Verilator link sandbox-runnable and giving
  non-Nix contributors a documented escape hatch.
- **Shell inventory:** verilator, yosys, nextpnr+trellis (ULX3S) /
  icestorm (iCEBreaker), openfpgaloader, surfer/gtkwave; z3 (debug
  export only, per ADR-0020); cargo-{nextest,insta,mutants,fuzz,deny,
  audit}; mdbook, structurizr-cli, treefmt, alejandra.
- **Deferred:** the Sky130 flow (openlane/librelane) is a separate
  `silicon` devShell when L4 wakes -- heavy closure, no v0.1 value.

## Alternatives rejected
Dual-channel (complexity without benefit once source stays
stable-compatible); naersk/cargo2nix (less/more machinery than needed).
