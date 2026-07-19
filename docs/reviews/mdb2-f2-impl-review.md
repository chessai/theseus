# M-D.b2.f2 — IMPLEMENTATION review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2.f2 (reversible in-memory array map) — IMPLEMENTATION.
- **Branch reviewed:** `maestro/01KXW5SY8D2GK8P0JZ5XTF8T1W` (1ccf14b), base `9e28d99`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change.
- **Verdict:** **SHIP-WITH-FIXES.** No compiled-diverges-from-eval input constructible (the
  reviewer traced the full lowering to the exact 4-instr body and recomputed every pinned
  trajectory). 1 MAJOR merge-blocker (F2-I1: the histogram deliverable is behind `#[ignore]`)
  + 3 MINOR (F2-I2/I3/I4).
- **Disposition:** F2-I1..I4 fixed as advisor-finish (test-only). Committed verbatim per L21.

---

# Adversarial IMPL review — M-D.b2.f2 (reversible in-memory array map, `iter` ∘ `memexch`), branch `maestro/01KXW5SY8D2GK8P0JZ5XTF8T1W` (1ccf14b, base 9e28d99)

- **Reviewed against:** `docs/scratch/mdb2-f2-design.md` (design of record, F2-1..F2-7 folded), `docs/reviews/mdb2-f2-design-review.md`, `docs/reviews/mdb2-impl-review.md` (I-1..I-8), `crates/theseus-tisc/src/compile.rs`, `crates/theseus-tisc/src/energy.rs`, `crates/theseus-eval/src/interp.rs`, `crates/theseus-core/src/{ty.rs,prim.rs}`.
- **Files under review:** `crates/theseus-tisc/src/energy.rs` (+47, `rom_program_energy_resident`), `crates/theseus-tisc/tests/array_map.rs` (+537), `crates/theseus-tisc/tests/memexch.rs` (T7 reroute).
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change. Reasoned from code; cargo not run (per instructions).

## VERDICT: **SHIP-WITH-FIXES** — no compiled-diverges-from-eval input was constructible (I traced the full lowering to the exact 4-instruction body and recomputed every pinned trajectory by hand; the program, the domain coverage, and the d=5 displacement test are all real). One MAJOR vacuity: **the park-tax histogram — the milestone's headline M-E artifact — is asserted only inside an `#[ignore]`d test**, so the default suite pins neither `B=30` nor `T(k)=18+4k`; the trace half of T6 needs no netlist and must move into the default tier. Three MINOR tighten-ups. Merge-blocking: F2-I1 (trivial split). No blocker.

---

## (A) The program is what the design says — **CONFIRMED, traced to the instruction level**

- `array_map_body()` (array_map.rs:66–84) builds exactly `memexch ; (g ⊗ id_{idx,arr}) ; memexch ; (id_data ⊗ (succ ⊗ id_arr))` with `g = succ = Iso::SwapSum(One,One)`. The left-nested `Iso::seq` chain is handled by the `Seq` arm (compile.rs:1742–1747); `match_controlled_idiom` cannot fire (it requires a leading `SwapProd`, compile.rs:1289 — none present).
- **Type agreement:** `memexch_prim(&bit(), 2)` builds carrier `elem × (idx × (elem × (elem × One)))` (compile.rs:1229–1239), which is syntactically identical to the test's `body_ty()` (array_map.rs:30–37). `num_leaves`: `One→0`, `Sum→1+max`, so Bit=1 leaf, `numeral_ty(4)`=4 leaves (compile.rs:72–87) → carrier 8 leaves, head 6 ≤ 14. Homes: counter Reg0–3, data Reg4, idx Reg5, arr `Mem(0),Mem(1)` (`home_of`, compile.rs:361–375).
- **Lowering, traced concretely at d=4:** `allocate_aux` finds free Reg6/Reg7 (compile.rs:600–605) → `aux_pi/aux_po` empty, `dctx = ctx`. `dctx.split(4)` → body sub-ctx `[Reg4, Reg5, Mem0, Mem1]`. memexch recognizer: F-3 verify (1006–1035) and F-2 guard (1038–1048) pass, `with_registers([0,1])` → `MexchReg{4,5}` (1051–1060), no paging (both Reg-homed). `Times(g, Id)` splits at `num_leaves(Bit)=1` (1749–1758); `SwapSum(One,One)` → `Xori{4,1}` (1825–1836). `succ⊗id` → `Xori{5,1}` on the idx sub-ctx. **Body = `[MexchReg{4,5}, Xori{4,1}, MexchReg{4,5}, Xori{5,1}]`** — exactly the T5 slice; `rd=4 ≠ ra=5` (aliasing rejected at 1054–1058 anyway); g is on the data leaf, succ on idx, memexch pair identical and correctly ordered. No `Swapbr` in the body → `has_inner_control` false (731–733) → the unroll accepts (1931). `B = 3×(6-instr spine + 4) = 30`; `wcet = instrs.len()` (no `loop_terms` for iter) ✓; `B+S_max = 30+12 = 42 < 256` ✓.
- **Eval side:** `iter` applies the body exactly k times, counter untouched (interp.rs:190–205); the unroll executes exactly the k thermometer-guarded copies (tags `[1×k, 0×rest]` — verified against the tag+payload encoding, compile.rs:68–107; copy i guarded on tag reg i, 1947–1957; the body never touches Reg0–3/Reg6/Reg7).

## (B) T2 teeth and T3 non-blindness — **CONFIRMED**

- **T2** (array_map.rs:222–258) enumerates the full 64-value domain (asserted), *asserts by scan* that it contains `idx=1` and `k=3` (the g² case, k>n=2 — genuinely reachable: `numeral_ty(4) = 1+(1+(1+(1+0)))`, ty.rs:38–44, so `R³(L())` is a valid inhabitant), and asserts `run_on_cpu_fast_resident == eval` for **all 64**. The fast sim's `MexchReg` is `swap(reg[rd], mem[reg[ra] % 4])` (compile.rs:2794–2797), agreeing with the prim table's `% n` for all representable idx bytes {0,1} (F-4).
- **T3** (`#[ignore]`, array_map.rs:262–288) goes through `run_on_cpu_resident` → `run_rom_on_cpu` → **the real `system_step`** (compile.rs:2704), i.e. the L19 authority. Samples include `k=1,idx=1`, `k=2,idx=1`, `k=3,idx=0` — the address register is genuinely **1** on the real CPU, varies within a run (succ is inside the body), and every sample produces a *visible* memory change (`g=notb` flips `arr[i]` regardless of whether `data == arr[i]`, so there is no all-zeros/no-op blindness). **f1's I-2 mistake is not repeated.**
- T2 leans on the fast sim for breadth with T3 as the ignored authority — the same tiering f1 settled on post-I-2; acceptable.

## (C) T4 pins the right semantics — **CONFIRMED (recomputed independently)**

Trajectory from `idx=0`: iter0 hits `arr[0]`, idx→1; iter1 hits `arr[1]`, idx→0; iter2 hits `arr[0]`, idx→1.
- k=2: `arr_out=(¬arr0,¬arr1)`, `idx_out=0`, data & counter preserved — T4's asserts (array_map.rs:296–312) match exactly, over all 8 (data,arr) combinations.
- k=3: `arr[0]` hit twice → `g²=id` → `arr_out=(arr0,¬arr1)`, `idx_out=1` — T4's asserts (317–336) match exactly.
T4 asserts against `eval` only, but T2's whole-domain `CPU == eval` transfers every pin to the compiled program. Correct expected maps, no wrong-trajectory possibility left open.

## (D) `rom_program_energy_resident` + T6 — **API correct; the histogram has real teeth but they are behind `#[ignore]` → F2-I1**

- The new API (energy.rs:450–490) is a faithful mirror of `rom_program_energy` (400–443): same budget = `wcet()`, same `rom_trace`, same `balanced_energy(step_net)`, substituting `compile_with_metadata_resident` + `marshal_resident` — the F2-2 fix as ordered, and the arrayMap ROM passes the home guard through it (not a hand-composed dodge).
- **T6's teeth are real where it counts:** `trace.steps_t` is a genuine dynamic measurement (first PC into `[rom_len,256)`, energy.rs:338–361) asserted against the hand-derived `T(k)=18+4k`, and `pe.park_tax` (API-internal trace, a second compile+trace) against `(30−T(k))/30` — including **`park_tax(0)=0.4` exactly** (the corrected F2-5 value, not "~max"). I verified the arithmetic: spine 6×3=18 always runs; skipped `Swapbr` jumps `body_len=4`; k=3 → 30=B → tax 0 (rom_trace's no-break default = budget, consistent).
- **Two vacuity notes:** (i) `total_j == B·step_j` is an identity through the API (`total_j := steps_b×step_j`, `steps_b := budget` echoed by `rom_trace`, energy.rs:358) — the real I-5 cure lives in the rerouted memexch T7 (F2-I2). (ii) **The whole test is `#[ignore]`** — see F2-I1.

## (E) f1 T7 reroute — **NOT weakened; strengthened. CONFIRMED CLOSED**

The rerouted T7 (memexch.rs:421–472) keeps every real f1 assertion: MEXCHR-bearing ROM priced > 0 (`pe.total_j > 0`, `pe.step_j > 0`), straight-line `B == T` (`pe.steps_b == pe.steps_t` — `steps_t` is the genuine dynamic measurement), `park_tax == 0`. The old test's "two-path" check was the I-5 tautology (`trace.steps_b := budget` by construction, so trace-total vs wcet-total compared an expression with itself); the new manual path (`balanced_energy` composed by the test vs the API's internal composition) is a real cross-code-path check of the API's plumbing. Net: teeth gained, none lost.

## (F) T-F2-7 (d=5) — **CONFIRMED, real displacement + real oracle**

At d=5: 9 leaves, head 7 → Reg0–6 occupied, only Reg7 free. `allocate_aux` needs 2, excludes counter leaves 0..4; displaceable = {leaf5(data,Reg5), leaf6(idx,Reg6)}; it takes the **last** = leaf 6 (idx); free-slot construction pushes scratch first, then `Mem(3),Mem(2)` (compile.rs:616–625; Mem0/1 excluded by the occ seed), and `pop()` yields **`Mem(2)`** → page-in `Mexch{6, 2}` (664–670). So the asserted `Mexch{_, addr≥2}` (array_map.rs:507–515) is exactly the idx displacement, and no other emission can produce a `Mexch` with addr≥2 (paging targets only Mem homes; Mem homes ≥2 exist only via displacement) — no false positive. If the occ seed broke, displacement lands on `Mem(0)` → the shape assert fails AND the 80-input whole-domain oracle diverges (arr[0] clobbered). If the pop order flipped to scratch → shape assert fails. This is the F-6 mem-spill-onto-resident sub-case with teeth, unlike f1's original T5-F6b (I-1) — it did not repeat that mistake. One overstatement: the design's "fails if the F-4 bound regresses" is not exercised here (idx is Bit, always <2); F-4 reject coverage remains f1's T5-F4 (F2-I4, doc nit).

## (G) Encodings, counts, hidden failures — **CONFIRMED**

- `numeral_val(k) = R^k(L())` matches `numeral_ty` exactly (ty.rs:33–44) and `leaf_value_to_u8`'s Right-count (compile.rs:93–107); `decompose_output` is its inverse and cannot hit the bare-`Unit` panic (the `1+0` terminus forces a final `Left`). `bit_val`/`make_input` shapes match `enumerate_values`' constructions (prim table keys are exact-Value lookups — verified same constructors).
- `2·(d−1)=6` MexchReg at d=4: 3 copies × 2 ✓ (T1 asserts the exact count, ROM-wide).
- T2 domain 64 = 4×2×2×2×2×1 ✓; T-F2-7 domain 80 ✓.
- `#[ignore]` inventory: T3, T6, T7 only — T1/T2/T4/T5/T-F2-7 run by default. No default-tier assertion is hidden. All referenced APIs exist with matching signatures (`system_ring`/`bus_quiescent` ring_run.rs:369/481, `encode_system`/`decode_system` types.rs:369/381, `rom_program_energy_resident` pub, `Iso::times/seq/iter`, `Value::left/right/pair`).
- Park hygiene: dead-zone fetch decodes to `Addi r0,#0` — a true no-op — so unmarshal after the full budget is clean.

---

## Findings

### F2-I1. MAJOR (merge-blocking, trivial fix) — the park-tax histogram, this milestone's M-E deliverable, is asserted only inside `#[ignore]`d `t6_park_tax_histogram`; the default suite pins neither `B=30` nor `T(k)=18+4k`.
**Vacuity:** array_map.rs:343 marks T6 `#[ignore]`, but the design's §7 reserves `#[ignore]` for the real-CPU/closure tiers (T3/T7); T6 appears in the acceptance list unqualified. A regression in the unroll spine, `body_len`, or the skip semantics (the exact machinery `T(k)=18+4k` measures) leaves the default suite green — T1/T5 pin instruction shapes, nothing pins lengths or the trace. The expensive parts of T6 are only `lower_iso(step(...))` + spice pricing; `compile → assemble → rom_trace` is milliseconds.
**Required fix + test:** split T6: a **non-ignored** `t6a_trace_histogram` asserting `lowered.wcet()==30`, `rom_trace(...).steps_t == 18+4k` for k∈0..4, and the derived ratios `{0.4, 8/30, 4/30, 0}` strictly decreasing (no netlist, no CharDb); keep the joule-pricing half (`rom_program_energy_resident`, `step_j`, `total_j`) in the `#[ignore]`d `t6b`.

### F2-I2. MINOR — T6's `total_j == B·step_j` assert is an identity, not a check.
`rom_program_energy_resident` computes `total_j := steps_b × step_j` and `rom_trace` echoes `steps_b := budget` (energy.rs:358, 476); T6's `total_expected = b × pe.step_j` with `pe.steps_b == b` asserted from a deterministic re-compile is the same expression rebuilt (f1's I-5 shape). Not damaging — the genuine two-path energy check lives in the rerouted memexch T7 (API vs test-composed `balanced_energy×budget`), and T6's real teeth are the `steps_t` measurements — but the comment "exact" should not imply this line could fail. **Fix:** either drop it to a documented model-identity comment or assert against the manual `balanced_energy` path as T7 does (one extra call; T6 already has the net in scope).

### F2-I3. MINOR — T5 does not pin "succ is the LAST body instruction" (the F2-6 point) or `body_len=4`.
T5 asserts `instrs[pos2+1] == Xori{ra,1}` but nothing prevents extra instructions between succ and the closing `Swapbr`; a lowering change that appends body instructions would pass T5 (and, per F2-I1, today nothing else in the default tier would catch the resulting `T(k)` change). **Fix:** add `assert!(matches!(instrs[pos2+2], Instr::Swapbr{..}))` (closes the body at 4 instructions) and, belt-and-braces, `assert_ne!(rd1, ra1)`.

### F2-I4. MINOR (doc only) — T-F2-7's claim "fails if … the F-4 bound ever regresses" is overstated.
Its idx is a Bit (values {0,1} < n=2); no F-4 violation is representable through it. Occ-seed and pop-order regressions ARE caught (traced in (F)); F-4 reject coverage is f1's T5-F4. Amend the test comment / design line; no code change.

### Nits (no action required)
- `rom_program_energy_resident` lacks the F-9 `num_leaves(in)==num_leaves(out)` assert the oracles carry, and panics (via `marshal_resident`'s I-4 asserts) rather than `Err` on malformed `resident` — loud, consistent with the I-7 disposition; note if the API goes load-bearing beyond tests.
- "Serialized" for the `#[ignore]` tier remains a runner-discipline comment (no mechanism), same as memexch.rs — pre-existing convention, not a new defect.

## Confirmed CLOSED (with evidence)
- **Design §8(1)** — body compiles to exactly the T5 slice: traced end-to-end in (A), `[MexchReg{4,5}, Xori{4,1}, MexchReg{4,5}, Xori{5,1}]`, shared `ra`, g/succ as `Xori`.
- **§8(2)** — `rom_program_energy_resident` runs the resident compile+marshal for real (energy.rs:457, 465) and f1's T7 is rerouted through it, strengthened not weakened ((E)).
- **§8(3)** — T2 includes k=3 (asserted, reachable); T-F2-7 has a real `Mexch{6,2}` displacement + 80-input oracle ((F)).
- **§8(4)** — T3 exercises idx=1, k≥1 on the real `system_step` with visible memory effects; no I-2 recurrence ((B)).
- **F2-1/F2-5/F2-6 fold-ins** — g² pinned (T4 k=3, recomputed correct); `park_tax(0)=0.4` asserted (not "~max"); succ-inside-copy shape asserted (modulo F2-I3's LAST-ness gap).

## Verdict roll-up
**SHIP-WITH-FIXES.**
- **Merge-blocking:** F2-I1 (un-ignore the trace half of the histogram — the milestone's deliverable must be pinned by the default suite; trivial test split, no src change).
- **Follow-up:** F2-I2 (T6 identity → real cross-path or honest comment), F2-I3 (T5 closing-`Swapbr` + `rd≠ra` asserts), F2-I4 (F-4 claim wording).

The compiled program is the designed program; every pinned trajectory recomputes correctly; the one semantically novel regime (g², k>n) and the one structurally novel regime (d=5 displaced idx) both have genuine default-tier whole-domain oracles. The single significant defect is the familiar class — a true measurement parked behind `#[ignore]` where nothing runs it — and it costs one test split to close.
