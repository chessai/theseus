# M-D.b2.f2 — DESIGN review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2.f2 (reversible in-memory array map, `iter` ∘ `memexch`) — DESIGN.
- **Design under review:** `docs/scratch/mdb2-f2-design.md` @ `5bf11c9`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the design.
- **Verdict:** **SHIP-WITH-FIXES** — no blockers (composition sound, idx-stability traced at
  instruction level incl. paged + displaced regimes); 3 MAJOR (F2-1 d=3 makes k>n untested,
  F2-2 rom_program_energy rejects resident ROMs, F2-3 the reduce finding is false) + 3 MINOR
  (F2-4/5/6) + 1 required follow-on test (F2-7 d=5 displacement).
- **Disposition:** all folded into the design (d=4; rom_program_energy_resident; corrected
  reduce finding; reworded mechanism/park-tax/T5; d=5 test added). Committed verbatim per L21.

---

# Adversarial DESIGN review — M-D.b2.f2 (reversible in-memory array map, `iter` ∘ `memexch`), `docs/scratch/mdb2-f2-design.md` @ `5bf11c9`

- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the design. Reasoned from code; cargo not run (per instructions; the probe already established the single-memexch composition).
- **Reviewed against:** `docs/scratch/mdb2-design.md` (f1), `docs/reviews/mdb2-design-review.md` (F-1..F-10), `docs/reviews/mdb2-impl-review.md` (I-1..I-8), `crates/theseus-tisc/src/compile.rs`, `crates/theseus-tisc/src/energy.rs`, `crates/theseus-eval/src/interp.rs`, `crates/theseus-core/src/typing.rs`.

## VERDICT: **SHIP-WITH-FIXES** — the core composition is sound (I could not construct a corrupting input for the program as drawn: the #1 risk, idx stability across the memexch pair, is closed by the canonical-layout bracket discipline, traced below at instruction level). But the design contains three MAJOR claims that are false or unbuildable as written — the `k>n` oracle claim is vacuous at the design's own example `d=3`, the energy deliverable names an API that rejects resident programs, and the "reduce is impossible without a new primitive" finding is wrong and would enshrine a false lesson — plus three MINOR honesty/wording defects. All are design-text and test-plan amendments; none require new emission machinery.

---

## (A) State enumeration — what each stage assumes, and what defeats it

| Stage | Assumption | Verdict / what defeats it |
|---|---|---|
| marshal-in (`marshal_resident`, compile.rs:2535–2571) | counter thermometer tags at leaves `0..d-1` → Reg; data/idx → next Regs; arr trailing 2 leaves → `Mem(0),Mem(1)`; everything else zero | HOLDS: `home_of` (361–375) is sole layout; f2's carrier (d=4: 4+1+1+2=8 leaves, head 6 ≤ 14) never reaches the else-branch; I-4 bounds asserts present (2540–2554) |
| `Ctx::new_resident` + occ seed (404–410) | occ marks `Mem(0),Mem(1)` live so no spill lands there | HOLDS: seed at 408; `allocate_aux` free-slot scan consults `occ.is_free(Mem(a))` (621–625) |
| iter aux allocation (`allocate_aux`, 1921) | rb/temp are genuinely free (zero) registers; counter leaves excluded from displacement | HOLDS for d≤4 (free regs remain); for d=5,6 it **displaces idx (and data) to `Mem(2)/Mem(3)`** — note free_slots pop order prefers *mem* over scratch (616–625: scratch pushed first, mem last, `pop()` takes mem). Traced sound (below), but untested — F2-7 |
| body lowered **once** (1926–1927) against `dctx.split(n_counter)` | split preserves absolute homes (413–423); body sub-ctx = `[Reg(data), Reg(idx), Mem(0), Mem(1)]` | HOLDS. NB: the design's "guard runs on each unrolled copy" is mechanically false — F2-4 |
| memexch recognizer ×2 (1001–1061) | F-3 verify (1006–1035, CPU modulus at 1010) + F-2 guard (1037–1048) pass; emit `MexchReg{rd,ra}`, `rd≠ra` (1051–1060) | HOLDS: sub-ctx homes[2]=Mem(0), homes[3]=Mem(1); data/idx distinct Reg homes ⇒ `rd≠ra` |
| `g⊗id`, `succ⊗id` | `Times` splits by `num_leaves(type_of(f))` (1749–1758); notb hits the `xori_K` arm (989–996) → `Xori{reg,1}`; `Id` emits nothing (1740) | HOLDS; neither touches idx's home between the memexch pair |
| unroll copies (1949–1957) | each copy is a byte-identical duplicate guarded on counter tag reg i; canonical layout restored at every instruction boundary at runtime | HOLDS: all brackets (`with_registers` 506–511, `ensure_reg`, skip-block CSWAP pairs 792–813) are LIFO self-inverse |
| run (fast sim 2893–2928 / real CPU 2863–2889) | budget `B = wcet() = rom.len()` (no `loop_terms` for iter — tight, as claimed); `MexchReg` = swap `reg[rd] ↔ mem[reg[ra]%4]` (2794–2797); P2: `B+S_max ≤ 256` (d=4: 30+12=42 ✓) | HOLDS |
| unmarshal-out | aux page-out (1959) restored any displaced homes before reading | HOLDS |
| eval side (interp.rs:190–204) | `iter` applies body exactly `k` times, counter preserved | HOLDS; per-iteration effect matches (traced in (D)) |

## (B) Attack-surface disposition (design §8, item by item)

**§8.1 — Two memexch per copy, same idx (THE #1 RISK): CLOSED, with instruction-level evidence.**
Concrete f2 (d=4): body instrs are exactly `[MexchReg{4,5}, Xori{4,1}, MexchReg{4,5}, Xori{5,1}]` (data=Reg4, idx=Reg5). Between the pair, the only instruction is `g`'s `Xori` on the *data* register — idx's register is never written. The two `MexchReg` are byte-identical; `succ`'s `Xori{5,1}` runs strictly *after* the second memexch. I then attacked the paged/displaced variants:
- *Paging (data/idx in scratch, junk-padded head):* `with_registers` spares pop from the high end (478–502); the Xchg squat swaps a possibly-live spare's byte into the operand's scratch slot and back (page-out is the exact reverse, 506–511) — self-restoring, and the emitted body between brackets touches only `rd`, `ra`, `mem[idx%4]` with `idx∈{0,1}`, disjoint from every squat location.
- *Displacement (d=5/6: idx and/or data displaced to `Mem(2)/Mem(3)`)*: worst case d=6, aux `rb=7, temp=6`, and memexch's two paged operands take spares 7 (=rb) and 6 (=temp). On the executing path `rb=0` and `temp=body_len` during the body (emit_skip_block 767–784): page-in `Mexch{7,3}` squats rb's 0 in `mem[3]`, `Mexch{6,2}` squats `body_len` in `mem[2]`; `MexchReg` addresses `mem[idx%4]` with `idx∈{0,1}` — **cannot reach the squat words** — and LIFO page-out restores rb/temp before the closing `Swapbr/Cswap/Subi` need them. No perturbation of idx is constructible. The disjointness argument is load-bearing: it rests on F-4 (`idx < n = resident ≤` first free mem slot). It holds, but only F2-7's test pins it.

**§8.2 — idx register vs resident words: CLOSED.** `home_of` puts head leaves in Reg/Scratch only (head ≤ 14 enforced, 1663–1670); the only path to a Mem home for idx is `allocate_aux` displacement, whose free-slot scan excludes `Mem(0),Mem(1)` via the occ seed (408, 621–625). F-6 survives iter's rb/temp grab — but the displaced-idx path (d≥5) has zero test coverage today (F2-7).

**§8.3 — Home guard on every copy: CLOSED in effect, FALSE as described (F2-4).** The body is lowered **once** (1926–1927) and the d−1 copies are literal duplicates (1949–1957); the guard runs once, not per copy. Safety across copies rests on (i) every lowering returning with canonical layout (structural: all arms return `ctx.clone()` or restored merges), checked only by `debug_assert_eq!` at 1724–1729 and 1938–1942, and (ii) runtime self-restoration of brackets. The conclusion (arr stays at `[Mem(0),Mem(1)]`, `succ` mutates idx's *value* never its *home*) is right; the mechanism claim in §3/§6 is wrong and must be restated, because a future change that makes per-copy lowering real (e.g. copy-specialized bodies) would silently lose the "probe confirms it once ⇒ holds for all copies" property.

**§8.4 — k>n semantics: BROKEN at the design's own example (F2-1).** With `d=3`, `k ∈ {0,1,2}` and `n=2` ⇒ `k > n` is **unreachable**; §2's "the oracle covers all k" is vacuously false for the g² case. Eval and CPU do agree by construction for k>n (both apply the body k times; unroll executes exactly the k tag-guarded copies, thermometer marshalling [1×k, 0×rest]), but the claim is untested unless `d ≥ 4`.

**§8.5 — Park-tax honesty: mechanism REAL, magnitude OVERSTATED (F2-5).** `rom_trace` (energy.rs:338–361) measures T as first PC-into-dead-zone; a skipped copy's `Swapbr` jumps `body_len` (Swapbr exec 2798–2800, `pc += br+1`), so T(k) = 6(d−1) + 4k + const: **strictly increasing, histogram not flat** — the forward-skip → early-park mechanism is exactly fib's (me-design §5b, energy.rs ProgramEnergy). But the skip-block spine (Addi/Cswap/Swapbr/Swapbr/Cswap/Subi = 6 of every 10 per-copy instructions, emit_skip_block 767–813) always executes, so for d=4: B=30, park_tax(0)=12/30=**40%**, not "~max (all skipped)". The energy story does not collapse to f1's (f1 was 0%), but §4's sweep claim must be quantified honestly.

**§8.6 — Counter/idx aliasing: CLOSED.** Positional leaves are distinct and `home_of` is injective on head leaves (`Reg(leaf)` for leaf<8, `Scratch(leaf−6)` for 8≤leaf<14, 361–375); counter occupies leaves 0..d−1, data/idx the next two. No hand-assigned layout exists on the iter path (unlike Trace). T2's whole-domain oracle would catch any overlap.

## (C) Findings

### F2-1. MAJOR — §2's "oracle covers all k / k>n is oracle-checked" is false at the design's example d=3: k>n is unreachable (k_max = d−1 = 2 = n).
**Break:** the g² regime — the one semantically novel case f2 adds over "map-once" — is never executed by any test if d=3 is chosen. A future bug in the unroll that double-executes a copy (e.g. a tag-guard off-by-one) would be invisible on the k≤n domain for g=notb only in the wrong direction — the claim is simply untested (L20 class).
**Fix:** set `d = 4` (counter 4 leaves; carrier 8 leaves, head 6 ≤ 14; free regs 6,7 = exactly rb/temp, no displacement; B=30, B+S_max=42 < 256; domain 4×16=64 inputs). This simultaneously gives the 4-point histogram §4 wants.
**Required test:** T2 whole-domain at d=4 (includes k=3>n); plus a pinned-value assertion: k=3, idx=0, g=notb ⇒ `arr_out = (arr0, ¬arr1)` (g² = id on the doubly-hit word), data/counter unchanged, idx_out = idx0⊕1.

### F2-2. MAJOR — the §4 energy deliverable is unbuildable as written: `rom_program_energy` hardwires the non-resident compile and marshal, and will cleanly REJECT the arrayMap ROM.
**Evidence:** `rom_program_energy` calls `compile_with_metadata(iso)` and `marshal(input, &in_ty)` (energy.rs:408, 416) — resident=0 — so the memexch home guard sees arr leaves at Reg homes and errors `ResidentMisplaced` (compile.rs:1037–1048). No `*_resident` variant exists in energy.rs (grep: none). f1's T7 already dodged this by hand-composing `balanced_energy` + `rom_trace` (impl review I-5) — f2's design re-names the API that still cannot run its program.
**Fix:** add `rom_program_energy_resident(iso, input, resident, …)` (or a resident parameter) using `compile_with_metadata_resident` + `marshal_resident`, and resolve I-5 in the same stroke (T6 cross-checks `total_j` from the API against `steps_b × step_j`).
**Required test:** T6 calls the resident-aware entry and asserts `Ok`, `park_tax(d−1)=0`, strict monotonicity of T(k)/park_tax(k), `total_j == B·step_j` exact — plus the *specific* expected values T(k) = 18+4k, B=30 for d=4 (pins the spine/body split, which is what makes the histogram honest).

### F2-3. MAJOR — the §5 "genuine finding to record" (reduce impossible without a new primitive, "akin to the TP-5 lookup-vs-swap gap") is FALSE as stated; recording it would enshrine a wrong lesson.
**Break:** the borrow-swap idiom f2 itself uses IS a non-destructive indexed read. Carrier `(acc, data, idx, arr)` (5 leaves; acc leading keeps arr trailing and memexch's sub-ctx contiguous — `Times(id_acc, memexch)` splits to homes `[Reg1,Reg2,Mem0,Mem1]`, guard passes): BODY = `(id⊗memexch) ; (SwapProd;xorInto;SwapProd on (acc,data) ⊗ id) ; (id⊗memexch) ; succ_idx` computes `acc ⊕= arr[i]` with arr, data, idx-advance exactly as in f2 — a parity/XOR reduce (dot-product mod 2 with a resident key is a controlled-op away), all from *existing* recognized prims (xorInto compile.rs:856–863, SwapProd→Exch 1761–1777). What memexch genuinely cannot give you is an **arithmetic** sum — but the blocker is that no single-leaf type represents a value > 1 (the F-1 thermometer fact; `1+1=2` is unrepresentable so `addInto` on Bit leaves isn't domain-closed), i.e. element/accumulator *width*, not swap-vs-read.
**Fix:** amend §5: defer the reduce as scope (fine), but record the accurate finding — "XOR-reduce expressible today via borrow-swap; arithmetic reduce blocked by single-leaf value width (F-1), not by memexch."
**Required test:** none for f2 (scope-OUT); if the lesson is recorded, the parity-reduce sketch above is the f3 candidate and must ship with its own oracle then.

### F2-4. MINOR — §3/§6's "the guard runs on every unrolled copy" misdescribes the mechanism: the body is lowered ONCE (compile.rs:1926–1927) and duplicated (1949–1957); cross-copy safety is canonical-layout restoration + debug-only asserts (1724–1729, 1938–1942).
**Break:** none today (the property is structural and the asserts run under `cargo test` debug builds), but the design text would mislead the next person who makes copies non-identical.
**Fix:** reword §3/§6 to state the real invariant ("guard runs once on the shared lowering; copies are duplicates; canonical layout at every instruction boundary carries it to all copies").
**Required test:** T5 as amended (F2-6) already pins the duplicated-body shape.

### F2-5. MINOR — §4's "park_tax sweeps from ~max (k=0, all skipped)" overstates by ~2.5×: only the 4 body instructions per copy are skippable; the 6-instruction skip spine always executes. Actual sweep for d=4: 40% → 26.7% → 13.3% → 0.
**Break:** an implementer writing T6 to assert "park_tax(0) ≈ 1" would fail and might "fix" the assertion downward without understanding; the M-E artifact's headline must not claim near-total idling.
**Fix:** state the model in §4: T(k) = 6(d−1) + body_len·k, park_tax(0) = body_len·(d−1)/B = 40% at d=4. **Required test:** the exact-value assertions in F2-2's T6.

### F2-6. MINOR — T5's "succ (a NOT on idx_reg) sits between copies" is wrong: succ is the LAST instruction inside each copy's body.
**Fix + test:** T5 asserts the per-copy body slice is exactly `[MexchReg{rd,ra}, Xori{rd,1}, MexchReg{rd,ra}, Xori{ra,1}]` with the two `MexchReg` sharing `ra` (and count = 2(d−1) across the ROM, tightening T1's "≥ 2").

### F2-7. MINOR (required follow-on test) — the displaced-idx regime (d≥5) is reachable, traced sound, and completely untested; it is the only regime where the §8.1/§8.2 disjointness argument does real work.
**Evidence:** at d=5 the register file is full past Reg6; `allocate_aux` displaces idx to **`Mem(2)`** (mem slots are popped before scratch — free_slots construction 616–625 — a surprise worth documenting), and body memexch then pages idx via `Mexch{spare,2}` with spare = rb or temp (spares pop high, 478–502). Soundness rests on: occ seed keeps displacement off `Mem(0,1)` (408, 621); `MexchReg` addresses < n = 2 ≤ first free slot (F-4); LIFO brackets. This is precisely the I-1 lesson ("invariant real, nothing would notice if it broke").
**Required test:** compile arrayMap at d=5, assert the aux page-in contains a displacement `Mexch{_, addr≥2}`, and run the whole-domain fast oracle (5×16=80 inputs). Fails if the occ seed, the pop order, or the F-4 bound ever regresses.

## (D) Reversibility judgment (§2) — SOUND
Per iteration from `(D, i, arr)`: memexch → `(arr[i], i, arr[i←D])` (verified table, 1006–1035); g → `(g(arr[i]), i, arr[i←D])`; memexch → `(D, i, arr[i←g(arr[i])])` — data restored *within* the iteration, so it is invariant across the whole run (the "borrowed scratch" claim is exact); succ flips idx. BODY is a composition of bijections; `iter` preserves the counter on both sides (eval interp.rs:190–204 returns the counter unchanged and applies body exactly k times; the unroll executes exactly the k thermometer-guarded copies, tags untouched by the body). Final idx = idx0 ⊕ (k mod 2) on both sides. The "map" reading for k ≤ n is correct (each word hit ≤ once, both starting idx values). **No run_on_cpu ≠ eval input constructible** — the residual risk is coverage (F2-1, F2-7), not semantics.

## (E) Scope judgment (§5) — right cut, two amendments
n=2/Bit-index/no-thermometer-pack is the correct continuation of f1 route (i); `g⊗id` and mid-carrier `succ` both compile through existing arms (evidence in (A)); nothing IN scope is unbuildable **except the T6 energy entry point (F2-2)**. Choose **d=4**, which satisfies F2-1, the histogram, and stays displacement-free; add the d=5 displacement test (F2-7) as the one deliberate step past the comfortable regime. The reduce deferral stands, but the recorded reason must be corrected (F2-3).

## Verdict roll-up
**SHIP-WITH-FIXES** (design amendments before delegation):
1. **F2-1:** d=4; k>n pinned test.
2. **F2-2:** `rom_program_energy_resident` (+ I-5 cross-check) named in the design as new API surface.
3. **F2-3:** rewrite the reduce finding (parity-reduce expressible; width is the real blocker).
4. **F2-4/F2-5/F2-6:** reword mechanism, park-tax quantification, and T5; exact-value T6 assertions.
5. **F2-7:** d=5 displacement oracle test joins the acceptance list.

No blocker: the emission composition under `iter` is sound as probed and as traced here, including the paged and displaced regimes. The findings are all of the "claim ≠ code / claim ≠ test" class — the exact class (L15, L20, I-1) this repo has learned to close before shipping, not after.
