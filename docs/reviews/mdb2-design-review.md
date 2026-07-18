# M-D.b2 — DESIGN review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2 (compiler-side MEXCHR emission) — DESIGN.
- **Design under review:** `docs/scratch/mdb2-design.md` @ `c76b6f3`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the design.
- **Verdict:** **DO-NOT-SHIP AS DRAWN** — 2 BLOCKERs (F-1, F-2), 3 MAJORs (F-3/4/5),
  1 confirmed-closed-with-obligation (F-6), 4 MINORs (F-7..F-10). Revision path small.
- **Disposition:** all findings folded into the design (route (i) n=2 chosen for F-1);
  finding→test index below. Committed verbatim per L21.

---

# Adversarial DESIGN review — M-D.b2 (compiler-side MEXCHR emission), `docs/scratch/mdb2-design.md` @ `c76b6f3`

## VERDICT: **DO-NOT-SHIP AS DRAWN** — two blockers, one of which makes f1 *unbuildable as specified*; the revision path is small and is spelled out per finding.

The two-increment cut (f1 single exchange, f2 iter) is the right shape, the resident-marshalling idea is sound in principle, and the reuse of the M-D.b closure harness is legitimate. But §4c's emission is written against a machine-value model this compiler does not have (F-1), and the recognizer as drawn repeats, move for move, the exact occ/home-blindness that produced M-D.d's BLOCKER F-1 (`Times(Trace,g)` clobber, docs/reviews/mdd-impl-review.md) — this time with memory words instead of registers (F-2). Both are silent-corruption paths, not reject paths.

---

## (A) State/assumption enumeration (what each stage assumes, and what defeats it)

| Stage | Assumption in the design | What defeats it |
|---|---|---|
| marshal-in (`marshal_resident`) | trailing `resident` leaves ↦ `mem[0..resident]`, head positional | head/Ctx drift (F-5); `resident` ≠ prim's `n` (F-2) |
| home layout (`Ctx::new_resident`) | homes agree with marshal; occ marks resident words occupied | occ not seeded ⇒ allocate_aux zero-slots land on live resident words (F-6, must be made explicit) |
| lower → `recognize_prim` name arm | prim's leaf 0/1 are two head leaves in registers; leaf 1 holds the *numeric* index; arr leaves sit at `Mem(0..n)` in order | **no single-leaf type holds a value > 1** (F-1); **nothing checks arr homes** (F-2); name forgery / wrong-modulus table (F-3) |
| emit `MexchReg{rd,ra}` | `reg[ra] % MEM_WORDS` = eval's `a mod n`, target is a resident word | `n < MEM_WORDS` with representable `a ≥ n` (F-4); arr displaced off `Mem(0..n)` (F-2) |
| run (`run_rom_on_cpu`) | budget/park machinery unchanged | nothing new — straight-line, `B = rom.len()`; confirmed no interaction with P1–P3 guards (compile.rs:2408–2438) |
| unmarshal-out | same `resident`, same positional rule, out_ty leaf count = in_ty | drift class (F-5); in/out leaf-count mismatch for non-endo isos (F-9-adjacent, MINOR) |
| eval side | `Iso::Prim` table lookup; resident-ness invisible to eval | table built with a modulus the CPU doesn't use (F-3/F-4) |

---

## (B) Findings

### F-1. BLOCKER — §4c's emission is unimplementable for `n = MEM_WORDS = 4`: no core type marshals a numeric index into one register. The design's own T2 (§7, "`n = MEM_WORDS = 4`") contradicts its own emission sketch.

**Evidence.** Every marshalled leaf byte is a sum **tag ∈ {0,1}**: `value_to_leaf_bytes` (compile.rs:134–168) emits only tag bytes 0/1 and zero-padding; `One` emits nothing; products concatenate. A type is single-leaf only when `num_leaves(Sum(a,b)) = 1 + max(|a|,|b|) = 1`, i.e. `a,b ∈ {Zero, One}` (compile.rs:65–80) — at most **2 inhabitants**. A 4-value index type (`Sum(One,Sum(One,Sum(One,One)))`) is **3 leaves** in thermometer code: value k marshals to `[1×k, 0×(3−k)]` across three registers. This is not a corner: it is the same unary encoding `lower_data_dependent_iter` documents ("the i-th tag register of the unary-encoded Nat", compile.rs:1679–1681).

**The break.** As drawn ("addr leaf = 1 of the prim's own 2 head leaves", §4c), `reg[ra]` at MexchReg time can only ever be 0 or 1. With `n = 4`, addresses 2 and 3 are unreachable by the hardware while the eval table freely maps them — `run_on_cpu_resident ≠ eval` on the first input with `a ≥ 2`. T2 as specified cannot even be constructed: there is no `Idx_4` that is "leaf 1".

**Required revision (pick one, say which in the design):**
- **(i) minimal f1:** `Idx = Bit`, `n = resident = 2`. Coherence holds because every representable `a < 2 ≤ MEM_WORDS` (both moduli are the identity). One `MexchReg`, no packing. Note this *falsifies* §4b's "with n = MEM_WORDS … they coincide by construction" — replace that sentence with the real condition (see F-4).
- **(ii) full n=4:** keep the 3-leaf thermometer `Idx_4` and emit a reversible packing bracket around the exchange — the thermometer property gives `k = Σ tag bytes`, so: `ADD t0,t1; ADD t0,t2; MEXCHR data,t0; SUB t0,t2; SUB t0,t1` (5 instrs, all operands pairwise distinct by canonical layout). The eval table indexes by the value's Right-depth. This must be *in the design*, not discovered by the implementer.

**Test that ships with the fix:** T1+T2 as amended — whole-domain fast oracle at the chosen `n`, **including the maximal address** (`a = n−1`), plus the real-CPU `#[ignore]` tier (T3). If (ii): also an input with `a = 3` asserting `mem[3]` is the word exchanged (pins the packing arithmetic, not just end-to-end equality).

### F-2. BLOCKER — the recognizer never checks that the arr leaves are actually home at `Mem(0..n)`; any composition that shifts them silently corrupts. This is M-D.d impl-review F-1 (occ/home-blind Trace) reincarnated for memory words.

**Evidence.** §4c pages only `data`/`addr` and says arr "stays in mem, addressed implicitly" — but *implicit* means **absolute** `mem[a % MEM_WORDS]`, while the arr leaves' homes are whatever the positional rule gave their *global* positions. `Ctx::split` (compile.rs:382–392) hands `recognize_prim` a home slice with absolute locations; nothing in the design's arm consults it beyond leaves 0/1.

**Concrete corrupting input.** `Times(memexch, id_Bit)` with `n = resident = 4` (or the n=2 analogue): global carrier = `data, addr(…), arr[0..4], g` — 7+ leaves; `resident = 4` puts the *global trailing four* in mem, i.e. `arr[1..4] → Mem(0..2)` **shifted by one** and **`g`'s live byte → Mem(3)**, while `arr[0]` sits in a register. The emitted `MexchReg` then: for `a = 0` exchanges with `mem[0]` = arr[1] (eval says arr[0]); for `a = 3` **clobbers `g`'s data**. Compiles clean, runs clean, park checks pass (P1/P3 look at BR/s0/s1 only, compile.rs:2433) — wrong answer. Exactly the "invisible corruption" class this project has already shipped once (M-D.d F-1) and swore off.

**Required revision:** the recognizer arm MUST verify, before emitting, that the prim's arr leaves have homes exactly `[Mem(0), Mem(1), …, Mem(n−1)]` **in order** (order matters — element j ↔ `mem[j]` is what the eval table assumes), and reject otherwise (`Deferred`/new `ResidentMisplaced`). With the guard, misplacement is a loud reject; without it, the side-parameter convention is unsound (this is the §8.6 answer: the convention is salvageable *only* with this guard).

**Tests that ship with the fix:** (a) negative — `Times(memexch, id_Bit)` under the misaligning `resident`: assert the specific reject, and assert (without the guard the test fails by) the fast oracle diverging; (b) positive — `Times(id_Bit, memexch)` where arr *is* globally trailing: compiles, whole-domain oracle passes. Both belong in T5.

### F-3. MAJOR — name-keyed recognition with zero semantic verification is a forgery hole, and the design's justification ("the table is too large to pattern-match structurally") is factually wrong.

**Evidence.** Every existing arm *verifies the table* against the claimed semantics over the enumerated domain before emitting (`check_pattern_2reg` compile.rs:975–996, `check_pattern_2leaf` 1033–1058, `check_pattern_3reg`). Verification cost is O(|table|) — the same table `admit()` (theseus-core/prim.rs:110+) already walks in full. For f1's domain (≤ 128 entries) this is trivial. A `PrimDef` with `forward_name == "memexch"` but any other bijective table (hand-built, or a future generator bug — e.g. indexing `(a+1) mod n`, or a multi-leaf `elem_ty`) would lower to `MexchReg` and silently compute the wrong function in every user program; the compiler's own tests would pass because *they* use the honest generator.

**Required revision:** add `check_pattern_memexch(def, n, resident)`: for every domain value, decompose leaf bytes as `(d, a_leaves, arr)`, compute the index (Right-depth / thermometer sum), and check the output equals `(arr[idx], a, arr[idx ← d])` with **idx computed by the CPU's rule** (`% MEM_WORDS` after the F-4 constraint). Mismatch → `UnrecognizedPrim`. This check *subsumes* the name key (keep the name as a fast pre-filter if you like) and structurally enforces `num_leaves(elem_ty) == 1` for free (a multi-leaf element can't match a one-word exchange).

**Test:** a forged `PrimDef` named "memexch" whose table swaps at `(a+1) mod n` → must NOT compile to `MexchReg` (expect `UnrecognizedPrim`). Fails without the fix via oracle divergence.

### F-4. MAJOR — the modulus-coherence constraint is understated in §4a/§6: only `n > MEM_WORDS` is rejected, but the corrupting regime is `n < MEM_WORDS` with a representable address `≥ n`.

**Evidence/break.** Table indexes `a mod n`; CPU indexes `a % MEM_WORDS` (asm.rs:128–131, fast-sim compile.rs:2513–2516). With `n = 2`, `resident = 2`, and any index carrier admitting `a = 2`: eval touches `arr[0]` = `mem[0]`; CPU touches `mem[2]` — a *free* word (or, under f2, an allocate_aux displacement slot). Silent divergence, and §6 as written admits this combination (`resident = 2 ≤ MEM_WORDS` ✓, `n ≤ MEM_WORDS` ✓).

**Required revision:** state and enforce, in the generator (and re-checked by F-3's verifier): **every representable index value must satisfy `a < n`** (then `a mod n = a = a % MEM_WORDS` since `n ≤ MEM_WORDS`), **or `n == MEM_WORDS`**. Under F-1(i) this is `|Idx| = n = 2`; under F-1(ii), `n = 4 = MEM_WORDS`. Also §8.5 (involution for `a ∉ [0,n)`): with this constraint such `a` is *unrepresentable*, and the table is an involution pointwise (single-location swap, addr fixed) — confirmed sound; backward = forward; no Bennett witness needed. No separate finding.

**Test:** `memexch_prim(Bit, 2)` invoked with a 3-value index type → generator returns Err (or the F-3 verifier rejects); negative test asserts the error, not corruption.

### F-5. MAJOR — marshal/Ctx home-layout drift (the L15 class, §8.3): the design specifies `marshal_resident` and `Ctx::new_resident` as *two separate implementations* of the same layout function.

Today's `marshal` (compile.rs:2291–2313), `unmarshal` (2318–2333), and `Ctx::new` (363–379) already triplicate the positional rule; the design adds a `resident` twist to all three independently. T6 (round-trip) only catches marshal↔unmarshal disagreement; a **marshal↔Ctx** drift (e.g. one maps trailing leaves ascending, the other descending) produces a compiler whose gate tests pass wherever the drifted leaf is untouched — silent home drift.

**Required revision:** one shared `home_of(leaf, n_leaves, resident) -> Location` used by `marshal_resident`, `unmarshal_resident`, AND `Ctx::new_resident` — make drift unrepresentable, don't test it in. Plus the property test: for all `i`, the byte `marshal_resident` writes for leaf `i` lands at exactly `Ctx::new_resident(n, r).homes[i]`. (For §8.3's aliasing half: with `head ≤ 14` and `resident ≤ MEM_WORDS` enforced, head homes are Reg/Scratch only and can never alias `Mem(0..resident)` — confirmed, *provided the rejects exist*; they're load-bearing, keep T5's negative tests for both bounds.)

### F-6. CONFIRMED-CLOSED (with one explicit obligation) — resident/spill collision (§8.2): the existing occupancy machinery already prevents silent collision; the reject is reachable and is `TooManyLeaves`.

**Evidence.** The only paths that write compile-time-constant mem addresses are: (a) positional homes for leaves ≥ 14 — unreachable for head leaves under the `head ≤ 14` reject; (b) `allocate_aux` zero-source displacement, which selects mem slots via `ctx.occ.is_free(Location::Mem(a))` (compile.rs:590–594) and cleanly errors when slots run out (609–614); (c) `swap_homes`/`with_registers` brackets, which are self-restoring squat patterns (487–540, 455–480). Since `OccSet::from_locations(&homes)` (377) marks every home occupied, resident words at `Mem(0..r)` are never selected as zero-slots — **provided `Ctx::new_resident` seeds `occ` from its homes exactly as `Ctx::new` does.** That proviso is currently implicit in the design; make it explicit (it is the entire mechanism §6's "MUST detect and reject" relies on — there is no separate detector to build).

**Tests:** (a) `resident = MEM_WORDS` + a branching program that exhausts scratch → assert `TooManyLeaves`, not corruption; (b) `resident = 2` + a program forcing one mem displacement → whole-domain oracle passes and (assert on the instr vector) the displacement `Mexch` addr is ≥ 2. MINOR note: the error surfaced is `TooManyLeaves`, which mislabels the cause; acceptable, but say so in the design.

### F-7. MINOR — `with_registers` paging (§8.4) is confirmed safe, but f1's T2 never exercises it; add a paging-path test.

The scratch bracket (`Xchg` in/out, compile.rs:455–462) is a sound transient squat and cannot touch mem; the mem bracket (464–471) squats the spare's byte in a *free* word, which under F-4's constraint is disjoint from every address `MexchReg` can touch (`a < resident ≤` first free word). Aliasing `rd == ra` is impossible from canonical layout (distinct leaves, distinct homes; spares exclude operand regs, two paged operands get distinct spares 447–471) — the design's defensive check + the assembler guard (asm.rs:229–233) are belt-and-braces, fine. But in f1's flat carrier, data/addr are `Reg(0)/Reg(1)` and the bracket never fires. **Test:** a junk-padded carrier (≥ 8 head leaves) putting data or addr in scratch → oracle over a sampled domain, asserting the ROM contains the `Xchg` bracket around `MexchReg`.

### F-8. MINOR — adjoint naming: `adjoint(Prim)` swaps names (theseus-core adjoint.rs:107, `PrimDef::adjoint` prim.rs:81–91), so `Sym(memexch)` presents `forward_name = backward_name`. Since the table is an involution, set `backward_name = "memexch"` too; otherwise `Sym(memexch)` hits `UnrecognizedPrim` — a safe but needless reject (and under F-3's semantic verification this becomes moot for correctness; keep the name symmetric anyway). Test: oracle on `Sym(memexch)` compiles to the same single `MexchReg`.

### F-9. MINOR — the design names `run_on_cpu_resident` and `Ctx::new_resident` but no resident-aware *compile* entry. T1 asserts on the instr vector, so the public surface needs `compile_resident`/`compile_with_metadata_resident` (today's `compile_with_metadata` hardwires `Ctx::new(n)`, compile.rs:1434–1445). Also state the `resident > 0 ⇒ num_leaves(in_ty) == num_leaves(out_ty)` assumption for the oracle wrapper (trivially true for memexch; assert it rather than inherit it silently).

### F-10. MINOR — §6 bullet 1 ("constant address → keep emitting immediate Mexch") is aspirational: no compiler path constant-folds a program input, and f1's memexch always takes the register route. Fine to keep the *boundary statement*, but per L20 don't ship a claimed behavior with no test — mark it N/A-for-f1 explicitly or drop it.

---

## (C) Attack-surface disposition (§8, item by item)

1. **eval/CPU modulus** — NOT closed as drawn: broken twice over, by F-1 (index can't reach a register at all for n=4) and F-4 (`n < MEM_WORDS` combos admitted by §6). Closed by F-1's revision + F-4's constraint + F-3's verifier.
2. **Resident/spill collision** — closed by existing occ machinery (F-6), contingent on the explicit occ-seeding obligation; reject is reachable (`TooManyLeaves`).
3. **Marshalling non-bijection / home drift** — no aliasing given the two rejects; drift risk real, closed by single-source `home_of` + property test (F-5).
4. **with_registers paging** — sound (F-7); coverage gap only.
5. **Involution for `a ∉ [0,n)`** — sound after F-4 (such `a` unrepresentable; table pointwise involution).
6. **Side-parameter soundness** — unsound as drawn (F-2's corruption); sound *with* the recognizer home-layout guard. A `Type::Mem` would have caught F-2 by construction, which is a genuine point in its favor for the follow-on — but the guarded side-parameter is an acceptable f1 cut.

## (D) f1/f2 scope judgment

The cut itself is right (one prim, one instruction, oracle + closure reuse; f2 gets the loop and the energy story). But f1 as specified is **not buildable**: T2's `n = MEM_WORDS = 4` collides with the single-leaf index assumption (F-1), and the design must choose (i) n=2 or (ii) the packing bracket *before* delegation — this is a design decision, not an implementer's discovery. The oracle genuinely needs no `Type::Mem` (§4a's argument is correct: resident-ness is placement, eval compares `Value`s), so no smuggled dependency there. §4e's honesty about f1's null park-tax is appreciated and correct (`B = T` for straight-line).

## (E) Required-test roll-up (each finding → reproducing test, L20)

- **F-1:** amended T1/T2/T3 incl. maximal address; packing-arithmetic pin if route (ii).
- **F-2:** `Times(memexch, id)` reject + `Times(id, memexch)` positive oracle.
- **F-3:** forged "memexch" table → `UnrecognizedPrim`.
- **F-4:** over-wide index type → generator/verifier Err.
- **F-5:** marshal-vs-Ctx home property test + T6.
- **F-6:** scratch-exhaustion reject; displacement-addr ≥ resident assertion.
- **F-7:** scratch-paged emission oracle.
- **F-8:** `Sym(memexch)` oracle.
- **F-9/F-10:** API surface + N/A annotation (no test; *not-tested-because*: no behavior).

**Bottom line:** DO-NOT-SHIP as drawn. Revise §4b/§4c/§6/§7 per F-1 (choose the index representation), F-2 (home-layout guard is mandatory, it is the soundness of the whole side-parameter convention), F-3/F-4 (verify the table, state the real coherence condition), F-5 (single `home_of`). With those five amendments this is a clean, small, well-scoped task; F-6/F-7 confirmations mean the allocator needs no new machinery.
