# M-D.b2.f1 — IMPLEMENTATION review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2.f1 (compiler-side MEXCHR emission) — IMPLEMENTATION.
- **Branch reviewed:** `maestro/01KXVSZ8XW75EMTG4Y252CSWDD` (58f427f), base `3d5ed9d`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change.
- **Verdict:** **SHIP-WITH-FIXES.** No emitted-`MexchReg`-diverges-from-eval input was
  constructible (emission path sound). 4 merge-blocking findings (I-1..I-4), all of the
  "invariant is real but nothing would notice if it broke" class (L15 / M-D.d F-1); 4
  follow-ups (I-5..I-8).
- **Disposition:** blockers I-1..I-4 (+ the separately-found pre-existing `fib_cpu` stale
  test, a scoped-gate-rot from M-D.b) sent back as a fix task; follow-ups I-5..I-8 folded.
  Committed verbatim per L21.

---

# Adversarial IMPL review — M-D.b2.f1 (compiler-side MEXCHR emission), branch `maestro/01KXVSZ8XW75EMTG4Y252CSWDD` (58f427f, base 3d5ed9d)

- **Reviewed against:** `docs/scratch/mdb2-design.md` (design of record) + `docs/reviews/mdb2-design-review.md` (F-1..F-10).
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change. Reasoned from the code; cargo not run.
- **Files:** `crates/theseus-tisc/src/compile.rs` (+331), `crates/theseus-tisc/tests/memexch.rs` (+445).

## VERDICT: **SHIP-WITH-FIXES** — the emission path itself is sound (I could not construct an input that makes an *emitted* `MexchReg` diverge from eval), but four of the design's mandated test/API obligations are unmet, two of them exactly the "green suite, zero teeth" class (L15/L20) this project has shipped before. Merge-blocking: I-1, I-2, I-3, I-4. Follow-up: I-5..I-8.

---

## (A) Prompt items — what I verified, with the attack that failed

**A. F-3 semantic verify (CLOSED, with one caveat → I-6).** The verify runs strictly *before* the home guard and emit (compile.rs:1014–1045 → 1047 → 1060), over `enumerate_values(&def.input_ty)` — the whole domain — checking each forward entry byte-for-byte with the CPU's modulus `idx = a % MEM_WORDS` (compile.rs:1019) and rejecting missing entries, `idx >= n_arr`, and byte mismatch. Crucially the verify is at the *same byte level the CPU executes*: it demands exactly "swap `bytes[0]` ↔ `bytes[2+idx]`, `bytes[1]` fixed, all else fixed", which — given the F-2 guard pinning leaf `2+j` at `Mem(j)` and `with_registers` pinning leaves 0/1 into `rd`/`ra` — is *definitionally* what `MexchReg` does (compile.rs:2774–2777). So any table that passes cannot diverge from the emitted instruction; the recognizer's rule, not the generator's, gates emission.

Can generator `a % n` vs recognizer/CPU `a % MEM_WORDS` differ? Not for any *representable* input: `value_to_leaf_bytes` writes only tag bytes 0/1 and zero padding (compile.rs:141–176), so the addr byte is always ≤ 1; for `n ≥ 2`, `a%n == a%4`. I did find one generator↔recognizer disagreement: `elem_ty = Sum(Zero, One)` (one inhabitant, byte **1**), `n = 1` — generator computes `idx = 1%1 = 0` and returns `Ok`, recognizer computes `idx = 1%4 = 1 ≥ n_arr=1` and **rejects**. Safe direction (reject, not corruption), but it falsifies the generator comment "`always == a since idx_count <= n and a < idx_count`" (compile.rs:1240) — `a` is a tag byte, not a value index, and is *not* bounded by `idx_count`. See I-6.

**B. F-2 home guard consults the right context (CLOSED; T5-F2a has teeth).** `lower_dispatch` for `Times(f,g)` does `ctx.split(n_left)` (compile.rs:1740–1744) and `Ctx::split` slices `homes` preserving *absolute* locations (compile.rs:424–434); the `Prim` arm passes that sub-context to `recognize_prim` (compile.rs:1783–1786). The guard checks `ctx.homes[2+j] == Mem(j)` for all `j` (compile.rs:1047–1057). I traced T5-F2a concretely: `Times(memexch, id_Bit)`, 5 leaves, `resident=2` → homes `[Reg0,Reg1,Reg2,Mem0,Mem1]`; memexch's sub-ctx = `[Reg0,Reg1,Reg2,Mem0]`; guard sees `homes[2]=Reg(2)` → `ResidentMisplaced`. **Without** the guard: verify passes (honest table), `with_registers` emits `MexchReg{0,1}`, and at `a=0` the CPU swaps `d ↔ mem[0] = arr[1]` while eval swaps `d ↔ arr[0]` (which sits in Reg2) — real corruption; compile would succeed and the test's `matches!(Err(ResidentMisplaced))` would fail. The reject test genuinely fails without the fix. T5-F2b's positive case is also correctly aligned (sub-leaf `2+j` = global `3+j` = `Mem(j)`), whole-domain oracle.

**C. F-5 single-source (VIOLATED → I-3).** `home_of` (compile.rs:361) is used by `Ctx::new_resident` (414), `marshal_resident` (2549), `unmarshal_resident` (2572) — good. But the original `Ctx::new` (394–407), `marshal` (2502), `unmarshal` (2528) all **keep their own inline copies of the layout rule**. `home_of(i,n,0)` does equal the old rule for all `i` today (head = n, so branch 2–4 reproduce `i<8→Reg(i)`, `i<14→Scratch(i−6)`, `else Mem(i−14)` exactly) — but there is **no test pinning that equivalence**, and design §8(1) makes "home_of is the *sole* placement function" an explicit impl-review gate. Blast radius of a future drift is contained (each entry-point family is internally consistent), but it is precisely the drift hole F-5 ordered closed by construction.

**D. `home_of` else-branch collision (REAL, pub-API-reachable → I-4).** For `resident > 0` and `head > 14`, the else branch aliases: e.g. `n=17, resident=2` → head=15; leaf **14** (head) → `Mem(14−14)=Mem(0)` and leaf **15** (resident) → `Mem(15−15)=Mem(0)`. `marshal_resident` then writes both bytes to `mem[0]` (last wins) and `unmarshal_resident` reads `mem[0]` for both — **silent value corruption, no panic**. Unreachable through `compile_with_metadata_resident`/the oracles (the `head > 14` reject at compile.rs:1649–1655 fires first), but `marshal_resident`/`unmarshal_resident` are `pub` with **no bounds check of their own** and are already called directly by tests (T4, T7). This violates the design's reject-don't-corrupt boundary for a public function. Also: `resident > n_leaves` reaches `n_leaves - resident` underflow in `home_of` → panic (loud, lesser issue, I-7).

**E. Vacuous tests (TWO CONFIRMED → I-1; plus I-5).**
- **T5-F6b is vacuous for its stated purpose** (memexch.rs:282–333). `Times(big_id_8, memexch)`: `Iso::Id` lowers to zero instructions; memexch's data/addr sit in `Scratch(2)/Scratch(3)` and `with_registers` pages them with **`Xchg`, never `Mexch`** (compile.rs:504–512). No `Mexch` exists in the ROM, so the `addr >= 2` assertion body **never executes**. The design's T5-F6b ("a program forcing one mem displacement") is not implemented.
- **T5-F6a never reaches the occ machinery** (memexch.rs:258–279). 19-leaf `Iso::Id`, resident=4 → head=15 → it hits the `head > 14` bounds reject in `compile_with_metadata_resident`; `allocate_aux` and `occ` are never touched (an `Id` emits no spills anyway). It tests the bounds reject (worth keeping) but not what its name claims.
- **Net effect:** delete `occ: OccSet::from_locations(&homes)` from `Ctx::new_resident` (replace with an empty set) and **the entire suite still passes** — the F-6 obligation the design flagged as "T5-F6a fails without it" (§8(3)) has zero teeth. The code *is* correct (seeding present at compile.rs:418; `allocate_aux` free-slot scan consults `ctx.occ.is_free(Mem(a))`, compile.rs:611–615), but this is exactly the L15 pattern: correct helper, no test that the *sequence* preserves the invariant.

**F. Adjoint/Sym F-8 (CLOSED).** `Sym(f)` lowers via `adjoint(f)` (compile.rs:1773–1780); `PrimDef::adjoint` swaps names and tables; memexch sets `backward = forward.clone()` and `backward_name = "memexch"` (compile.rs:1259–1268), so the adjoint is the identical def and the F-3 verify runs on the adjoint's forward table (= original backward = same involution). T5-F8 asserts `MexchReg` presence **and** a whole-domain oracle (memexch.rs:373–394) — meaningful, includes `a = 1`.

**G. Other machinery checked.** `run_on_cpu_fast_resident` is line-for-line the `run_on_cpu_fast` loop (fetch/decode/`pc += br+1`, budget = `wcet()`) — correct, though duplicated. `enumerate_values` is left-factor-major (theseus-core/prim.rs:42–63) — this is what breaks the T3/T4 sampling (I-2). `with_registers` spares may hold live data, but the Xchg/Mexch brackets are self-restoring transient squats and never touch `Mem(0..resident)` (page-ins touch only the operand's own home; `MexchReg` touches only `mem[a]`, `a < n_arr`, verified). `allocate_aux` can only displace *Reg-home* leaves, so arr words are never displaced, and occ-seeding excludes `Mem(0..resident)` from zero-slots. `Ctx::merge`/`occ` copies consistent. Aliasing `rd == ra` is impossible from distinct homes; the defensive check + `asm.rs` `MexchAddrAliasesData` guard remain belt-and-braces. The memexch arm cannot be shadowed by the 1/2/3-leaf arms for `n_in = 4`. All external APIs the `#[ignore]` tests reference exist with matching signatures (`system_ring`, `bus_quiescent(&StateVals)`, `encode/decode_system`, `system_step`, `step`, `rom_trace`, `balanced_energy`, `rung0_measured`, `lower_iso`; theseus-netlist/-spice are `[dependencies]` of theseus-tisc, so the test crate builds).

---

## (B) Findings

### I-1. MAJOR (merge-blocking) — the F-6/occ obligation has zero test teeth: T5-F6b's displacement assertion never executes and T5-F6a never reaches `allocate_aux`.
**Break/vacuity:** as detailed in (E). Removing the occ-seeding from `Ctx::new_resident` leaves the whole suite green, yet without it `allocate_aux`'s free-slot scan (compile.rs:611–615, slots pushed `rev()` then `pop()`) would hand out **`Mem(0)` first** as a zero-slot — a displacement `Mexch{_, 0}` silently clobbering `arr[0]`.
**Required test:** a branching (controlled-op) program with `resident = 2`, regs and scratch fully occupied (head = 14), whose lowering forces `allocate_aux` to take a **Mem** zero-slot. Assert (a) the emitted displacement `Mexch.addr ≥ 2` and (b) whole-domain (or a≥1-inclusive) oracle. This test fails without the occ-seeding (displacement lands on `Mem(0)`, oracle diverges) — the design's stated bar. Keep the current T5-F6a renamed as the bounds-reject test it actually is.

### I-2. MAJOR (merge-blocking) — the authoritative (real-CPU) tier never executes a nonzero address: design §8(4) "T2/T3 include a = n−1" is violated for T3, and T4 runs on the all-zeros state.
**Break/vacuity:** `enumerate_values` is left-major, so `full_domain().take(4)` (T3, memexch.rs:92) is exactly `data=0, addr=0, arr ∈ Bit²` — the address register is 0 in every real-`system_step` run; the *data-dependent* half of data-dependent addressing is only ever checked on the fast sim (which the code itself documents as "NOT the authoritative CPU model", compile.rs:2795–2803). T4's single sample (memexch.rs:118) is the all-zeros value, for which `MexchReg` is a state no-op — the closure check cannot see a wrong exchange. Same `take(4)` pattern weakens T5-F6b/T5-F7 sampling (addr=0 only), though those have whole-domain/other coverage concerns of their own.
**Required test change:** pick samples that include `addr = 1` with `d ≠ arr[1]` (e.g. `first 2 ⧺ last 2` of the enumeration, or filter on the addr component) in T3 and T4; do the same for the T5-F6b/F7 sampled oracles. Fails today? No — it fails only when a real addressing bug exists; the point is it's the L19 authority and currently blind to the feature under test.

### I-3. MAJOR (merge-blocking, trivial fix) — F-5 only half-realized: legacy `Ctx::new`/`marshal`/`unmarshal` remain a parallel second implementation of the layout, unpinned.
**Break:** design §8(1) ("home_of is the SOLE placement function — no re-implemented rule in marshal/unmarshal/Ctx") is the impl-review gate and it is not met; `home_of(i,n,0)` equals the inline rule today by inspection only.
**Required fix:** re-implement `Ctx::new(n)` as `Ctx::new_resident(n, 0)`, `marshal(v,ty)` as `marshal_resident(v,ty,0)`, `unmarshal(s,ty)` as `unmarshal_resident(s,ty,0)` (behavior-identical, makes drift unrepresentable) — or, at minimum, a property test asserting `∀ i < n: home_of(i,n,0)` equals the location `marshal` writes and `Ctx::new` assigns. Prefer the refactor; that is what the design ordered.

### I-4. MAJOR (merge-blocking) — `marshal_resident`/`unmarshal_resident` silently alias head and resident leaves through `home_of`'s else-branch when `head > 14`.
**Break:** `marshal_resident(v, ty_17_leaves, 2)` writes head leaf 14 and resident leaf 15 both to `mem[0]` — silent data loss in a `pub` function, no error, no panic (construction in (D) above). The compile path rejects first, but the marshalling API is public, already used directly by T4/T7, and will be used by every future resident harness.
**Required fix + test:** make the else-branch unreachable-by-contract — either `home_of` panics there when `resident > 0`, or `marshal_resident`/`unmarshal_resident` assert `n − resident ≤ 14` (and `resident ≤ MEM_WORDS`, `resident ≤ n`). Test: `marshal_resident` on a 17-leaf type with `resident=2` must panic/error, not round-trip-corrupt (today `unmarshal_resident(marshal_resident(v)) ≠ v` for a value distinguishing leaves 14/15 — that inequality is the failing-without-fix witness).

### I-5. MINOR — T7's headline assertion is a tautology.
`total_j = steps_b as f64 * step_j` and `expected_total = steps_b as f64 * step_j` are the same expression; `rel_err` is identically 0 (memexch.rs:432–443). The test's real content is `step_j > 0` and `steps_b == steps_t`. Also uses `balanced_energy` + `rom_trace` by hand instead of the design-named `rom_program_energy`. **Fix:** compute `total_j` via `rom_program_energy` and compare against `steps_b * step_j` — then the identity is a cross-check between two code paths, not of an expression with itself.

### I-6. MINOR — generator's F-4 reasoning is wrong as stated, and `memexch_prim` accepts multi-leaf/degenerate `elem_ty`, producing prims whose tables don't implement the documented semantics.
**Break:** (a) `Sum(Zero,One)` counterexample from (A): comment "`a < idx_count`" is false; for `n=1` the generator returns `Ok` but the recognizer rejects its own honest output. (b) For a multi-leaf `elem_ty` (e.g. `Sum(Bit,Zero)`, 2 leaves), `bytes[1]` is the *data payload*, not the address — the generated table is semantically mislabeled (byte-level involution, but not `arr[a]`-semantics). None of this can reach a corrupting emission (the F-3 verify and home guard/`resident ≤ 4` wall it off — I verified the walls hold), but the pub generator violates its own doc comment. **Fix + test:** enforce `num_leaves(elem_ty) == 1` and `max representable addr byte < n` in `memexch_prim`; test that `memexch_prim(Sum(Zero,One), 1)` and a 2-leaf `elem_ty` are rejected. (T5-F4 as shipped does have teeth for the `idx_count > n` check via `(Bit, 1)` — confirmed it fails without the check — but the design's "3-value index type" case is unrepresentable through this API; note it as not-tested-because.)

### I-7. MINOR — `compile_with_metadata_resident(iso, resident)` with `resident > n_leaves` panics (usize underflow in `home_of` via `Ctx::new_resident`) instead of cleanly rejecting.
The `saturating_sub` at compile.rs:1648 masks the case past the head check, then `home_of`'s unsaturated `n_leaves - resident` (compile.rs:362) panics. Loud, not silent — but add `resident ≤ n` to the reject set + a one-line negative test.

### I-8. MINOR — T6 undersells its design mandate; `run_on_cpu_fast_resident` duplicates the sim loop.
T6 is roundtrip-only; the marshal↔Ctx half of the design's T6 is satisfied *by construction* (both call `home_of`) — acceptable, but say so in the test comment rather than in the name. The fast-sim loop duplication (compile.rs:2873–2905 vs 2809–2841) is a drift-in-waiting; factor the loop or accept knowingly.

---

## (C) Confirmed CLOSED (with evidence)

- **F-3** semantic verify: whole-domain, byte-level, CPU modulus, *before* guard and emit; forged-table T5-F3 fails without it (without the verify the forged def compiles clean — I traced the divergent entry `d=0,a=0,arr=[1,0]`). compile.rs:1014–1045.
- **F-2** home guard: correct sub-context via `Ctx::split` absolute homes; T5-F2a fails without the guard and the guarded-against corruption is real (swap with `arr[1]`/clobber class). compile.rs:1047–1057, 1740–1744.
- **F-4** emission-side: closed by the verify's `% MEM_WORDS` + `idx < n_arr` reject; representable addr bytes are 0/1 by the tag encoding. (Generator-side wording: I-6.)
- **F-6** code-side: occ seeding present (compile.rs:418) and `allocate_aux`/`with_registers`/`swap_homes` cannot touch `Mem(0..resident)` given it — *tests* are the gap (I-1).
- **F-7** paging: T5-F7 is non-vacuous (asserts both `MexchReg` and `Xchg` present + sampled oracle); the spare-squat bracket is self-restoring and mem-disjoint.
- **F-8** Sym: identical adjoint def, verified, whole-domain oracle including `a=1`.
- **F-9** API: `compile_with_metadata_resident` exists; leaf-count assert present in both oracles (compile.rs:2851–2860, 2881–2889).
- **F-1/route (i)**: n=2, Bit index, one `MexchReg`; T1 asserts on the instr vector; T2 is whole-domain (16 inputs) including `a = n−1` on the fast sim.

## (D) Verdict roll-up

**SHIP-WITH-FIXES.**
- **Merge-blocking:** I-1 (real displacement test with teeth for occ-seeding — the design's own §8(3) gate), I-2 (T3/T4 + sampled tests must include `addr = 1`; §8(4) gate), I-3 (make `home_of` the sole layout function — trivial refactor), I-4 (bounds-guard `marshal_resident`/`unmarshal_resident` + aliasing regression test).
- **Follow-up (non-blocking):** I-5 (T7 tautology → `rom_program_energy` cross-check), I-6 (generator input validation + comment fix), I-7 (`resident > n` clean reject), I-8 (T6 comment, sim-loop dedup).

No emitted-`MexchReg`-diverges-from-eval input was constructible: the byte-level semantic verify with the CPU's own modulus, gated behind the home-layout guard and the register paging, makes the emission faithful to any table it accepts. The blockers here are all of the "the invariant is real but nothing would notice if it broke" class — which, in this repo's history (L15, M-D.d F-1), is the class that bites.
