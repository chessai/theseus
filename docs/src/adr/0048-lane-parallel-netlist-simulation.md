# ADR-0048: Lane-parallel (bit-sliced) netlist simulation

**Status:** Accepted -- **implemented** (`theseus-netlist::sim_lanes`,
`CellKind::eval_lanes`, `theseus-tisc::ring_run`). The scalar simulators
(`sim::simulate`, `CompactBalanced::simulate`) are unchanged and remain the
reference; the lane simulators are checked against them (ADR-0008/0018).

## 1. Problem

Both netlist simulators are bit-serial: one `Option<bool>` per token line,
one input vector per call, two `Vec<bool>` allocated per cell, and (for
`CompactBalanced::simulate`) the occupancy table recomputed every call. Yet
their heaviest users -- whole-domain oracles and ring runs from many start
states -- evaluate *one* netlist on many inputs: the ROM is baked into the
step netlist, so runs differ only in their input bits.

## 2. Decision

### 2.1 Lanes

`Lanes<N>` is `[u64; N]`: bit `l` of the line's word is its token in
independent run `l`. `Lane = Lanes<4>` (256 lanes) is the default block.
Plain `u64` loops only -- the workspace denies `unsafe` (no `std::arch`)
and the source stays stable-compatible (ADR-0019; no `std::simd`). Each
word op advances 64 lanes; whether LLVM also vectorizes the array loops
depends on the target and is not relied on.

Every cell is a pure bitwise function, so one more method next to
`CellKind::eval` carries all lane semantics, `CellKind::eval_lanes`:

| cell | ports | lane function |
|---|---|---|
| `Kk` | `c₁..c_k, a, b` | `s = ∧cᵢ; d = (a ⊕ b) ∧ s; a' = a ⊕ d; b' = b ⊕ d` |
| `Rk` | `c₁..c_k, t` | `t' = t ⊕ ∧cᵢ` (ADR-0040) |
| `K0` | `a` | identity |

### 2.2 Two lane simulators, each the image of a scalar reference

- **Timed** (`simulate_lanes`, `simulate_rails_lanes`) mirrors
  `sim::simulate`: same preflight and refusal order, same tick schedule
  (`sim::TickSchedule`, shared, so both check pairs in the same order),
  same per-tick INV-TOKEN assertion -- now `line0 ⊕ line1 = 1` on every
  active lane. Drivenness is lane-independent (the netlist is static), so
  it is one `bool` per line, not an `Option` per lane. A violation reports
  the lowest failing lane of the earliest failing check, carrying exactly
  the `SimError` the scalar sim reports on that lane's input
  (`LaneSimError::Lane { lane, error }`); lane-independent refusals are
  `LaneSimError::Refused`.
- **Compact** (`CompactBalanced::simulate_lanes`) mirrors the untimed
  topological sim over a flat single-assignment *slot plan* built once per
  netlist and cached in the `CompactBalanced`. Plan construction resolves
  K0 buffers, pass-through controls and rail-role swaps (NOT-in-Permute,
  ADR-0031) into slot aliases with a polarity bit, so the hot loop touches
  only target slots of computing cells and never allocates.

### 2.3 Fault injection

A valid, balanced netlist cannot violate INV-TOKEN through `simulate`
(every cell write is dual-rail). `sim::simulate_rails` and
`simulate_rails_lanes` drive input pairs raw as `(line0, line1)`, which is
the one way to reach the dynamic check end to end -- and to test that a
lane fault is reported on the right lane.

### 2.4 Monomorphization boundary

A generic function is compiled in the crate that instantiates it, at that
crate's opt-level. Called from `theseus-tisc` (opt-level 0 in dev builds),
the generic hot loop ran ~80× slower than in `theseus-netlist` (opt-level 2,
ADR-0020 profile). The public API stays generic over `N` (1..=8, checked
at compile time) but crosses into the netlist crate as flat `u64` words;
the hot loops are instantiated there for every supported width.

### 2.5 Many-start rings

`StepRing`, `ClosedStepRing`, `SystemRing` and `TwoBodyRing` gain
`run_many` (and `run_open_many` where `run_open` exists): up to 256 start
states per pass. The single-start `run`/`run_open` are the same code with
one lane, so every ring oracle test now exercises the lane simulator
against `eval(step)^R`. States marshal through
`StateVals::encode_bits`/`decode_bits` (`encode_system_bits`/
`decode_system_bits`) -- the `bits_of` image, built without `Value`
trees, which otherwise cost more than a 256-lane pass. The ancilla and bus
probes (`ancilla_mismatches`, `bus_quiescent`, `body_ancilla_mismatches`)
stay on the scalar simulator.

## 3. Verification (T2 links)

- `theseus-netlist/tests/sim_lanes.rs`: over the lowering tests' cell
  templates (K1/K2/K3/R1, K0 chains, output skew) plus 40 seeded random
  compositions of random-permutation prims (Seq/Sym/skewed Times, with
  ancillas), raw and `balance`d, at 64/192/256 lanes: timed lanes ≡
  `simulate` (results and `Unbalanced`/width refusals), compact lanes ≡
  `CompactBalanced::simulate`, and injected rail faults are reported on
  the lowest faulty lane with exactly the scalar error; masking the faulty
  lanes off runs clean.
- `ir::cell` tests: `eval_lanes ≡ eval` on every input combination.
- `sim` tests: the timed whole-domain oracle (`assert_timed_sim_agrees`)
  sweeps lane-parallel, each lane also checked against the scalar sim.
- `theseus-tisc/tests/ring_run.rs::run_many_matches_scalar_reference_and_emulator`:
  256 starts × 3 revolutions, `run_many` ≡ the scalar `simulate` loop the
  old `run` used ≡ the reference emulator.
- `types` tests: bit marshalling ≡ `Value` marshalling.

## 4. Consequences

Measured in the dev profile (the test configuration), scalar = the
pre-lane runner loop over `CompactBalanced::simulate`, same process:

| workload | scalar | lanes |
|---|---|---|
| `step` netlist (3,312 active cells, 20,092 lines): one pass | 0.29 ms per input | 0.10 ms per 256 inputs |
| `StepRing`, 256 starts × 3 revolutions | 296 ms | 2.5 ms |
| S-box `SystemRing` (8,596 active cells), whole domain: 16 inputs × 47 revolutions | 784 ms | 14 ms |
| same ring, 256 starts × 47 revolutions | -- | 21 ms |

Ring *construction* (lowering + compact balancing, unchanged here) now
dominates every ring test's wall time.

- Whole-domain ring authorities become one pass: the S-box authority now
  covers the whole domain plus 240 discriminating packings (was 6 inputs),
  χ adds 221 random full-byte rows to its 32-row domain.
- Lane-parallel runs are only as strong as the reference they are checked
  against; the scalar simulators stay, unoptimized, as that reference.

## 5. Alternatives rejected

- **`std::simd` / `std::arch`** -- nightly-only / `unsafe` (ADR-0019,
  workspace lints).
- **The `wide` crate** -- a new dependency; the portable `[u64; N]` loops
  are the baseline.
- **Per-lane drivenness** -- drivenness is a static property of the
  netlist; a mask per line would be checked work with no information.
- **Replacing the scalar simulators** -- they are the reference.
