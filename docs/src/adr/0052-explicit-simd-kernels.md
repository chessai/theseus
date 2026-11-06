# ADR-0052: Explicit SIMD kernels with scoped unsafe

**Status:** Accepted -- **implemented** (`theseus-core::simd`,
`theseus-netlist::simd`; wired into `theseus-core::bvexpr` / `prim`,
`theseus-netlist::sim_lanes`, and `theseus-tisc::{synth, bveq, compile,
ring_run}`).

**Amends:** ADR-0048 §2.1 ("plain `u64` loops only -- the workspace denies
`unsafe` (no `std::arch`)"). The portable loops stay, as the scalar
references and the non-x86-64 path; ADR-0048 and ADR-0049 are otherwise
unchanged.

## Context

The lane-parallel paths (ADR-0048 netlist simulation, ADR-0049 `BvExpr`
evaluation) and the packed Möbius transform (`synth`) are fixed-size
`[u64; N]` loops. The workspace builds for the baseline x86-64 target, so
LLVM can only auto-vectorize them to SSE2, and two of them are not
vector-shaped at all: the compact simulator branches per cell on control
count and swap-vs-flip, and `eval_expr_lanes` is a recursive walk over
`Box` trees. The host class we run on (Zen 3) has AVX2 and BMI2.

`unsafe_code = "deny"` is a workspace lint, and `core::arch` needs `unsafe`
twice: to call a `#[target_feature]` function from code without that
feature, and to load/store through raw pointers.

## Decision

### 1. Scoped unsafe

- `#![allow(unsafe_code)]` appears on exactly two modules, one per crate
  that owns a kernel: `theseus-core/src/simd/` and
  `theseus-netlist/src/simd.rs`. Each also sets
  `#![deny(clippy::undocumented_unsafe_blocks)]` and
  `#![deny(unsafe_op_in_unsafe_fn)]`; every `unsafe` block carries a
  `// SAFETY:` comment. The workspace lint is unchanged.
- The public API is safe. Unsafe functions are private to the kernel
  modules.
- theseus-tisc has no unsafe: its Möbius kernel lives in theseus-core
  (`simd::mobius_transform`), which also keeps the intrinsics at dev
  opt-level 2 (theseus-tisc is opt-level 0 in dev builds, where every
  intrinsic would be an out-of-line call).

### 2. Safety argument

- **Detection guards every `#[target_feature]` call.** Kernels are entered
  only through `match level.supported() { .. }`; `supported()` caps any
  requested level at `SimdLevel::detected()` (`is_x86_feature_detected!`,
  cached), so a forced `Avx2` on a CPU without AVX2 runs the SSE2 or scalar
  kernel. SSE2 kernels carry `#[target_feature(enable = "sse2")]` too (the
  intrinsics require it even though SSE2 is the x86-64 baseline) and are
  entered the same way. The BMI2 comparison kernels check
  `is_x86_feature_detected!("bmi2")` at the call.
- **Unaligned loads/stores only** (`loadu`/`storeu`): no alignment
  precondition anywhere. (The slot files start on a 64-byte boundary for
  speed, not correctness.)
- **Bounds checked before entering kernels.** The transpose and Möbius
  kernels index through `&[u8; 64]` / `&[u64; 4]` references produced by
  `as_chunks` or checked slicing, so their raw-pointer loads are in bounds
  by construction; shapes (multiple of 64 bytes, at most 8 planes, every
  `b * stride + w` inside the buffer, `2^n` words) are asserted on entry.
  The two cell-program kernels (compact sim, `LaneProgram`) skip per-access
  bounds checks: their programs are validated once at construction
  (`UniformPlan::new`, `MuxProgram::new` check every slot index against the
  slot count, at most `2^26` slots so 32-bit offset arithmetic cannot drop
  a bit; fields private), and `run` asserts the slot file's length before
  the kernel.
- **Validity:** stores into `bool` memory (`bits_to_bools`) write only
  `0x00`/`0x01` bytes.

### 3. Dispatch

`SimdLevel { Scalar, Sse2, Avx2 }`, detected once and cached in an atomic.
`THESEUS_SIMD=scalar|sse2|avx2|auto` lowers the active level (never above
the CPU; an unknown value warns once and means auto); `simd::set_level` overrides
it for benches. Every kernel has a `*_with(level, ..)` twin, so tests force
each level per call without racing on the global. Dispatch is once per
kernel call (a whole loop), never per lane op. Off x86-64 every level
compiles to the scalar path (`cfg(target_arch)`).

### 4. Kernels

Each has SSE2 and AVX2 flavours plus its scalar reference, tested against
it at every level the host supports (fixed-seed splitmix64 inputs, edge
shapes), and run end to end under `THESEUS_SIMD=scalar` and `=sse2`.

| kernel | reference kept | SIMD technique |
|---|---|---|
| bytes ↔ bit planes (`bytes_to_planes`, `planes_to_bytes`, `bools_to_bits`, `bits_to_bools`) | `SPREAD_BITS` sweep (`table_to_regtransfer_reference`), per-bit `pack_lane`/`pack_lanes_scalar`/`pack_regs_scalar` | `vpmovmskb` per plane; inverse by `vpshufb` + `vpcmpeqb` + `vpaddb`/`vpsubb` accumulation; SWAR 8×8 scalar; BMI2 `pext`/`pdep` for comparison |
| compact netlist sim (`CompactKernel`) | ADR-0048 `run_fixed` (still the `Scalar` path) | branch-free uniform cells (3 controls + 2 targets, flips as cswap with a sink), one `__m256i` per slot (N = 4) or two (N = 8), polarity masks from `vpsrad` + broadcast of the cell itself, ASAP level order |
| `BvExpr` lanes (`LaneProgram`) | `eval_expr_lanes` (`eval_lanes_recursive`, `eval_transfer_lanes_recursive`) | compiled once per transfer into a hash-consed DAG of uniform mux cells `e ^ (s & (t ^ e))` with polarity bits (`Not` free), level-ordered, evaluated branch-free |
| Möbius | the one-pass-per-stage loop | in-word stages `vpand`/`vpsllq`/`vpxor`; `half = 1, 2` by `vpslldq`/`vperm2i128`; three more stages across 8 registers in the same pass; radix-8 passes after |

The two cell programs share one design: compile once to a uniform,
branch-free, single-assignment cell list over a slot file, validate it,
then stream it. An interpreter over a postfix stack program (the first
`LaneProgram` attempt) lost to the portable code: its one indirect
dispatch branch mispredicted on ~14% of nodes under AVX2.

## Not done (future work)

- **AVX-512 / NEON.** No AVX-512 kernels: the development host (Zen 3) has
  none, and shipping kernels that cannot be run or measured here would
  violate the "measured honestly" rule. `vpternlogq` would make each mux
  cell one instruction and `vpcompressb`/`VBMI` would shorten the
  transposes; the dispatch enum takes a new variant. NEON (aarch64) is
  likewise unmeasured here; aarch64 builds take the scalar path.
- The timed simulator's tick loop (`TimedPlan::run_fixed`) keeps the
  portable `Lanes<N>` code.

## Miri

`cargo miri test -p theseus-core --lib simd` passes at the default target
(the scalar and SSE2 kernels) and with
`RUSTFLAGS="-C target-feature=+avx2,+bmi2"` (the AVX2 and BMI2 kernels:
under Miri `is_x86_feature_detected!` reports the compile-time features),
as do the `LaneProgram`, row-transpose and `admit` lane tests in
theseus-core and the netlist kernel tests; see the PR description for the
exact matrix. Miri cannot run the integration-scale tests (step netlists,
2^24-row sweeps) in reasonable time.

## Consequences

- `unsafe` is confined to two audited modules, and the safety argument
  is local to them.
- Measured gains (release, Zen 3, AVX2 vs the portable reference; full
  tables, instruction counts and caveats in the PR description) are
  largest where the kernel is the workload:
  - Möbius 2–4× (2.5–4.3× fewer instructions);
  - the register-file transposes 4.7–6.9×, the ring-carrier pack 3.8×;
  - lane programs 2.4–18× over the recursive walk on single instructions
    and a 4-instruction sequence (mostly sharing and no dispatch; AVX2 is
    1.3–2.8× over the same cells in portable Rust);
  - the compact simulator 1.2–1.6× per pass (1.6–1.9× fewer
    instructions); 512-lane blocks amortize the per-cell overhead best;
  - `StepRing::run_many` 1.3–1.5× end to end (1.6–2× fewer instructions),
    T6 ~1.5×, the `[8,8,8]` synth ~1.35×.

  `admit` and `bveq_verify_emission` are dominated by `BTreeMap`/`Value`
  and BDD work and do not move.
- New whole-domain sweeps should compile their transfer once
  (`SpecTransfer::lane_program`, `bveq::transfer_program`) and transpose
  through `pack_rows`/`unpack_rows`.
