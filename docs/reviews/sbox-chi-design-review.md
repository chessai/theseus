# DESIGN REVIEW — TP-5 Keccak-χ, bit-sliced over Flat(8) (adversarial, fresh-context, with POC)

**Reviewer:** fresh-context Fable 5, did not write the design. POC built + run in an isolated
worktree `/tmp/chi-poc` @ `604936a` (7/7 tests green; HEAD unchanged, only an untracked
`chi_poc.rs`; `/home/chessai/rust/theseus` untouched). Reviews `docs/scratch/sbox-chi-design.md`.

*(Advisor's faithful capture, L21. POC preserved at `docs/scratch/chi_poc.rs.ref`; scalar
cross-check at `docs/scratch/chi_check.py.ref`.)*

## VERDICT: SOUND-WITH-CHANGES

χ is the right first nonlinear target, the gate→instruction mapping is correct, and it needs
**zero** lower_trace/recognizer/decoder changes. The POC builds it end-to-end, proves the transform
on the whole per-row domain + a discriminating packing + a Keccak golden vector, and measures the 8×
bit-slicing win. **The one required change:** the design's §2 Option A claim that the 2-register
rolling scratch "returns to 0 after (garbage-free)" is **FALSE** — the POC proves the two ancillas
end holding OLD a0/a1 in 24 of 32 cases. A forward-only χ gadget is *not* garbage-free as specified.
Fix the framing (accept dirty-ancilla-cleaned-by-adjoint, or add χ⁻¹ cleanup) before delegating.

## 1. Target-choice critique

**χ over the AES S-box: correct call.** χ is one masked-AND + one NOT + one XOR per lane (5 lanes),
every nonlinear gate exactly the byte-parallel `Ccnot` the value-width ladder + BVEQ made Flat(8)-wide.
The AES S-box's bulk is GF(2⁸)-inversion + affine plumbing (~115 AND/XOR, ~230+ instrs w/ uncompute) —
stresses composition, not the delivered primitive; poor first bite. χ is the actual nonlinear heart of
SHA-3, not a toy.

**The "honest BVEQ relationship" (§0/§3.5) is accurate.** χ does NOT exercise BVEQ's symbolic-prim
recognition (selection-by-equiv). Confirmed two ways: (a) χ's per-lane step `a ⊕ (¬b ∧ c)` is a
3-instruction body, and selection_by_equiv/generate_arm_candidates only recognize *single-instruction*
arms — a fused prim would not be in the arm library; (b) what χ needs is the Flat(8) byte-parallel
`Ccnot`, which the recognizer already emits and valwidth_masked/symbolic_prim already exercise at full
byte width. So "first useful program that needs what BVEQ delivered, not a test of its recognizer" is
honest. Sharpening: χ is a *Rust-composed* `Vec<Instr>` (feistel-style), assembled directly — it never
enters the lowering path at all (no source Iso, no recognize_prim, no Deferred risk). Even stronger
no-new-machinery guarantee than the design claims.

## 2. The CORRECT reversible schedule (POC's actual, verified order)

Option A is the right skeleton and its transform is correct — 0/32 mismatches in a standalone scalar
model and on the real machine. Register map: r0..r4 = lanes a0..a4 in-place; **r5,r6 = ancilla** (old
a0/a1); negated-control masked-AND = `Xori{ctrl,0xFF}; Ccnot{tgt,ctrl,other}; Xori{ctrl,0xFF}`:

```
Xor  r5, r0            ; r5 = a0   (save)
Xor  r6, r1            ; r6 = a1   (save)
; g0: A0 = a0 ⊕ (¬a1 ∧ a2)      ctrls r1(a1), r2(a2)
Xori r1,0xFF ; Ccnot r0,r1,r2 ; Xori r1,0xFF
; g1: A1 = a1 ⊕ (¬a2 ∧ a3)
Xori r2,0xFF ; Ccnot r1,r2,r3 ; Xori r2,0xFF
; g2: A2 = a2 ⊕ (¬a3 ∧ a4)
Xori r3,0xFF ; Ccnot r2,r3,r4 ; Xori r3,0xFF
; g3: A3 = a3 ⊕ (¬a4 ∧ a0_old)  ctrls r4(a4), r5(saved a0)
Xori r4,0xFF ; Ccnot r3,r4,r5 ; Xori r4,0xFF
; g4: A4 = a4 ⊕ (¬a0_old ∧ a1_old)  ctrls r5(a0), r6(a1)
Xori r5,0xFF ; Ccnot r4,r5,r6 ; Xori r5,0xFF
```

**Why it works:** writer j corrupts a_j; old a_j is consumed by writers (j−1),(j−2). Ascending order
0..4: only a0 (consumed by writers 4,3 — both later) and a1 (consumed by writer 4 — later) are read
after overwrite → save exactly those two. g0/g1/g2 read still-original neighbors; g3 reads r4 (a4, not
yet written) + saved a0; g4 reads saved a0 + saved a1. Every A_i computed from OLD a_{i+1},a_{i+2};
Ccnot preserves controls; each negation undone by paired Xori 0xFF. Confirmed on the machine.

**THE FINDING (L15-class, but in the cleanup claim not the order):** after the forward run,
**r5=old a0 and r6=old a1 are NOT zero.** No instruction can zero them — to uncompute `r5 ^= a0` you
need a0 in a register, but after χ every register holds a *transformed* lane; re-deriving old a0/a1
from live outputs is exactly χ⁻¹ (degree-growing). Test `chi_whole_perrow_domain_packed` confirms
**24/32 rows leave ancillas dirty** (precisely rows where a0 or a1 is set), dirty values = old a0/a1.

Fix (pick one, state in spec):
- **(Rev-A, cheapest, recommended slice-1):** keep the 17-instr in-place χ; garbage-freeness comes from
  the **adjoint bracket** — the whole ROM run backward zeroes the ancillas (proven:
  `chi_reversible_roundtrip_eval`, 17 fwd + 17 adjoint restores input+ancilla exactly). Honest
  reversibility, but state plainly that forward-only leaves 2 dirty ancilla registers; garbage-free
  only as a reversible-and-uncomputed pair, not as a standalone plane step.
- **(Rev-B, true garbage-free in-place):** add χ⁻¹ cleanup over live outputs to zero r5/r6, ~+20 instrs
  (POC pins the estimate). Separate higher-bar slice (re-introduces a nonlinear inverse) — defer.
- Do NOT ship "net-zero garbage / scratch returns to 0" for the plain 17-instr version; it is false.

Register-budget note the design missed: hold ancillas in **free registers r5/r6, not Scratch** — Ccnot
reads registers only; Scratch would force extra Xchg paging for zero benefit. The input-preserving
out-of-place embedding (x,0)→(x,χx) that WOULD be trivially garbage-free needs 5 output registers but
only r5/r6/r7 are free — doesn't fit.

## 3. POC results

POC `/tmp/chi-poc/crates/theseus-tisc/tests/chi_poc.rs` → `docs/scratch/chi_poc.rs.ref`. **7/7 pass**
(5 fast <0.01s; 2 heavy — energy + eval-reversibility — 965s incl. step-net lowering).

- **Lower/recognized/Deferred?** N/A and that's the strong answer: Rust-composed ROM, assembled
  directly, never touches recognize_prim. `assemble(&chi_instrs())` succeeds; all 17 instrs already
  encodable (Xor, Xori, Ccnot). No lowering, no recognition, no Deferred possible.
- **run_on_cpu == independent ref + adjoint round-trip:**
  - `chi_whole_perrow_domain_packed`: all 32 per-row states packed ×8, run_rom_fast == byte-parallel
    ref AND == scalar-χ on bit 0. ✔
  - `chi_discriminating_lane_packing`: 8 distinct rows one-per-bit; each bit position independently ==
    scalar χ of its own row (catches an 8-lane bug) AND whole bytes == byte-parallel ref. ✔
  - `chi_forward_injective_over_domain`: forward map over 32-state domain injective on full extended
    state r0..r6 (reversible embedding). ✔
  - `chi_reversible_roundtrip_eval` (heavy): step(rom) fwd 17 then Iso adjoint 17 restores exact input
    incl. ancillas → 0. ✔
- **Bijection / involution:** χ is a bijection but NOT an involution (χ∘χ≠id) — design's "χ then χ⁻¹
  round-trips" is loose; correct reversibility check is the ROM adjoint, which round-trips exactly.
- **Keccak golden:** `chi_keccak_golden_vector` pins χ(1,0,1,1,0)=(0,0,1,0,0), hand-computed, asserted
  vs scalar ref and ROM. Matched. ✔

## 4. Measured perf win

`chi_energy_and_bitslicing` (real rung-0 char DB, t_ramp=1e-7, balanced_energy on lower_iso(step(χ ROM))):
```
χ ROM: 17 instrs, 5 byte-parallel Ccnots
step_j = 2.4691e-9 J/cycle ;  B = 17 cycles ;  total = 4.1975e-8 J
bit-sliced (Flat8, 8 rows): 5.2468e-9 J / row-χ
un-sliced  (Flat1, 1 row):  4.1975e-8 J / row-χ
→ 8× throughput for identical ROM + identical energy
```
Energy model is data-independent (total_j = B×step_j), so the same 17-instr ROM burns 4.20e-8 J whether
its 8 bit-lanes carry 1 row or 8 → **5.25e-9 J/row-χ vs 4.20e-8 J un-sliced, the 8× win explicit**.
Whole-χ (5 masked-ANDs) at 4.20e-8 J is same order as fib (7.3e-8 J) — a real workload.

## 5. Needs any lower/recognizer/decoder change? NO.

POC is a hand-composed Vec<Instr> of {Xor, Xori, Ccnot} — all encode + execute, byte-parallel Ccnot is
the exact gate valwidth_masked/symbolic_prim ship at Flat(8). Nothing compiled from an Iso →
recognize_prim, selection_by_equiv, lower_trace, decoder all untouched. χ is OFF the daemon
control-flow/lowering hot path — lighter (non-BVEQ-class) review bar, needs only L19 real-run authority.

## 6. Answers to §6 open questions
1. Schedule right + garbage-free? Transform right (0/32); NOT garbage-free (24/32 dirty). Trace/iter
   gives no cleaner in-place χ. Fix: Rev-A for slice-1, Rev-B follow-up. Ancillas in r5/r6, not Scratch.
2. .thse iter/fold vs Rust-composed? **Rust-composed (feistel-style), confirmed** — per-lane control
   sets differ (g3/g4 read saved ancillas), doesn't fit iter's uniform body.
3. Domain teeth without 2^40? Yes — whole per-row 32 (×8 packed) + 8-distinct-row discriminating
   packing (per-bit checked) + golden + injectivity. ~66 ROM runs, no gate-no-timeout risk.
4. Secret lower_trace/recognizer change? NO (§5).
5. Perf-win POC vs Flat(1)? Yes — §4, explicit 8×.

## 7. Recommended delegation scope

**Settled by this POC (worker lifts verbatim):** the exact 17-instr χ schedule; byte-parallel + scalar
refs; whole-per-row/discriminating/golden/injectivity battery; energy harness + 8× accounting; register
map (ancillas r5/r6).

**Delegation builds/decides (one fast-tier delegation, ~90min):**
1. Promote chi_poc.rs → `crates/theseus-tisc/tests/chi.rs`; 5 fast tests fast-tier, eval-reversibility +
   energy `#[ignore]` (heavy tier), mirroring feistel/valwidth_masked.
2. **Resolve garbage-free framing (REQUIRED):** ship Rev-A (17-instr χ, documented garbage-free-under-
   adjoint, ancillas cleaned by reverse run) + module docs + a test assertion that forward-only leaves
   r5/r6 = old a0/a1 (the `chi_whole_perrow_domain_packed` dirty-ancilla assertion is the L20 regression
   test). Do NOT claim standalone garbage-free.
3. Add real-CPU authority tier: one `system_ring(&chi_rom).run(...)` case (like valwidth_masked t_real,
   `#[ignore]`) asserting the ring transform matches + ancillas_restored/bus_quiescent on fwd+adjoint.
   (Not run in POC to respect memory budget; the one remaining authority check.)
4. Optional stretch, OUT of slice-1 (own slice, own review): Rev-B true-in-place χ⁻¹ cleanup (~+20
   instrs); AES S-box; multi-round Keccak-f (θ,ρ,π,ι).

**POC to start from:** `docs/scratch/chi_poc.rs.ref` (entire file; chi_instrs() is the deliverable
schedule; refs + 7-test battery are the L19/L20 authority). Scalar cross-check: `docs/scratch/chi_check.py.ref`.

**POC worktree git status (final):** `/tmp/chi-poc` detached at 604936a (unchanged), single untracked
file chi_poc.rs. Nothing committed. `/home/chessai/rust/theseus` untouched.
