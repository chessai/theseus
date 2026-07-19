# f3.5 impl review — `Ccnot` ISA instruction + masked-XOR dot-reduce (adversarial, static-only)

**Reviewer:** fresh-context Fable, did not write the change. All claims recomputed by
hand from the worktree diff (`01KXWKQQXD1KMY709ECDP577X8` vs `chessai/theseus-v2`). No
cargo/nix run. **Advisor note:** every finding below was CONFIRMED by the advisor's own
scoped `cargo test` run — in particular B-1 (`stage2b_lite_timing_certificate` FAILED:
measured d_pre=3316 vs pinned 2976) and a second clippy failure (unused import in
tests/ccnot.rs) not in the static review.

**Summary verdict: FIX-BEFORE-MERGE.** The core construction — `ccnot_instr()`'s
reversibility, wire mapping, predicate exclusivity, encoding, recognizer, three-way bit-0
agreement — is **correct**; could not be broken. What fails the bar is the *evidence
layer*: the golden timing pin was left un-re-pinned with "NEEDS RE-PIN" comments (that
test fails as committed), the closure `validate()==Ok` obligation (design §7.4) is
asserted nowhere that runs, and the decoder differential oracle never exercises the case
where the Toffoli actually fires.

## 1. `ccnot_instr()` reversibility — VERIFIED CORRECT (the crux holds)
Wire mapping right: 2-field core `[reg_field(0), AUX_PRED_FIELD]` → r0-slot 0..8, s1
8..16; `gate_at(2W, &[W, W+5, W+6, 0], cccnot_prim())` computes `r0slot.bit0 ^= pred ∧
s1.5 ∧ s1.6` (checked gate_at bubble-routing, no off-by-one). Copy brackets read the
register bit as CONTROL, s1 bit as target; `with_target(R_RD, core)` routes the target.
s1 restored to 0 on BOTH predicate paths (rd∉{rs,rt} encode-enforced). No s1 collision
(bits 5/6 free; 0=pred, 1-3=chain, 4=cswap). Zero bleed — only rd.bit0 mutates.
Aliasing caveat (MI-1): a hand-crafted rd∈{rs,rt} word leaves an s1 residue, but
encode+P1 forbid it.

## 2. Predicate exclusivity ✓, closure obligation UNMET
`r_pred(0b1011)` exclusive with all opcodes (full minterm, 1011 was reserved). Op::Ccnot
in both Op::ALL ([Op;15]) and instruction() dispatch. BUT `t7_system_ring_closure` is
`#[ignore]` AND never calls `validate()` (only bus_quiescent + step-oracle); the only
validate() calls with the new op (ring_two_body_timing) fail at the golden pins before
reaching them. Design §7.4 go/no-go (validate()==Ok) is met nowhere.

## 3. Three-way bit-0 agreement ✓, but ISO-path dense test never fires (M-1)
decoder / exec_instr (`^=(rs&rt)&1`) / reference all bit-0. step.rs change = doc count
14→15 (correct). `t_dense_bit0_only` runs on the fast sim only. The four new
`op_samples(Op::Ccnot)` NEVER exercise the firing case — every sample has a control with
bit0=0 (comments on samples 1 & 4 wrong: 0xAA, 0x32 are even). `full_decoder_moves` — the
only default-tier decoder-iso test — passes even if the CCC-NOT gate is dead. Break:
swap `W+5`→`W+7` and every non-ignored decoder test still passes.

## 4. Recognizer target-first ✓, T1 pin missing (M-3)
`check_pattern_3reg` destructures `Pair(t, Pair(c1,c2))`; closure maps leaf0→rd=target.
Target-first correct; no cswap collision (diverge at (1,0,1)). But `t1_single_ccnot_emission`
asserts only pairwise distinctness — any permutation of 3 distinct regs passes (spec §6
T1 required "fails if rd/rs/rt permuted").

## 5. Pre-existing edits — (a)(b) legit, (c) BLOCKER
(a) negative_guards: 0xD800 = opcode 0b1011 rd=rs=rt=0; decode==Ccnot{0,0,0} correct. ✓
(b) decoder reference: 0b1011 arm independently bit-0 (not calling impl). ✓ (comments
aside). (c) **ring_two_body_timing NOT re-pinned** — old goldens (d_pre=2976,
d_locked/period=3304) kept with "NEEDS RE-PIN" comments; netlist growth from Op::Ccnot is
structurally forced → `stage2b_lite_timing_certificate` fails (CONFIRMED: measured
d_pre=3316, d_mem=35, d_post=289, delta1=0, delta2=1, delta_hold=36, d_locked=3644,
period=3644), and its failure suppresses every validate() call.

## 6. dot_reduce — semantics solid, pins missing (M-3)
T5 real (full 192-domain fast-oracle + key=0 acc-unchanged + key=1 XOR-parity + 2-Ccnot
count) — but "verify Ccnot target is acc home" is an `eprintln!`, not an assert. T6a
measures monotonicity + T(d-1)==B but pins NO numeric B/T(k) (spec §7.5 required it) and
ships leftover scratch comments. step.rs = doc-only.

## 7. Scope / docs
Diff = §8 allowlist + step.rs (doc-only, NIT). Stale docs the spec ordered updated still
stale: asm.rs:4 "14 architected", decoder.rs:26 "14 predicated", decoder.rs:30 "Reserved
1011-1111" (now 1100-1111), decoder.rs:46-48 s1 layout omits bits 5-6.

## Findings
| ID | Sev | Finding |
|---|---|---|
| B-1 | BLOCKER | Golden timing pin not re-pinned; test fails as committed + suppresses validate(). Re-pin from measured (d_pre=3316, d_post=289, delta1=0, delta2=1, delta_hold=36, d_locked=3644, period=3644, d_mem=35). |
| B-2 | BLOCKER (advisor-found) | tests/ccnot.rs has an unused import → clippy `-D warnings` fails (second lint beyond the `W+0` the advisor already fixed). |
| M-1 | MAJOR | op_samples(Op::Ccnot) never fire the Toffoli (all controls bit0=0); comments on samples 1,4 wrong. Add a firing sample; a dead gate currently passes. |
| M-2 | MAJOR | T7 never calls validate() despite its doc; closure obligation (§7.4) unmet. |
| M-3 | MAJOR | T1 doesn't pin rd/rs/rt; T5 acc-home is eprintln not assert; T6a pins no measured B/T(k). |
| MI-1 | MINOR | Aliased-Ccnot s1 residue caveat belongs in decoder.rs:46-48 module doc. |
| MI-2 | MINOR | Stale count/reserved-range docs (asm.rs:4, decoder.rs:26/30/46-48). |
| N-1 | NIT | step.rs (doc-only) outside design §8 allowlist — harmless. |

## FIX-BEFORE-MERGE (ordered)
1. B-1: re-pin the golden from measured values; confirm the test reaches + passes
   `validate()` on all three ROMs (this also discharges the closure obligation).
2. B-2: remove the unused import in tests/ccnot.rs.
3. M-1: add ≥1 firing op_samples(Op::Ccnot) (both controls bit0=1 → rd^=1); fix the 2
   wrong comments.
4. M-2: make T7 assert the certificate/validate() on the dot-reduce ROM (or point its
   doc at ring_two_body_timing after 1).
5. M-3: pin T1's exact rd/rs/rt; convert T5's eprintln to an assert; pin T6a's measured
   B and T(k); delete scratch comments.
6. MI-1/MI-2: one doc sweep.

SHIP after the above + a full scoped run INCLUDING the `#[ignore]` tier (t2 real-cpu, T7
closure). The construction is sound; everything here is test/evidence repair, no redesign.
