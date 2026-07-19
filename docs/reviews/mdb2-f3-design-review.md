# M-D.b2.f3 — DESIGN review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2.f3 (reversible XOR/parity reduce, `iter` ∘ `memexch` +
  borrow-swap accumulator) — DESIGN.
- **Design under review:** `docs/scratch/mdb2-f3-design.md` @ `cfa6fff`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the design.
- **Verdict:** **SHIP-WITH-FIXES** — no blockers, no majors (all load-bearing claims recomputed
  at instruction level, landing on the probe's exact 24/56 counts). 5 MINOR: F3-1 garbled T5 +
  Xor-direction trap, F3-2 phantom xor aliasing guard, F3-3 missing idx0=1 semantic pin
  (oracle-blind class), F3-4 under-pinned d=4 shape, F3-5 over-optimistic f3.5 forecast.
- **Disposition:** all folded into the design. Committed verbatim per L21.

---

# Adversarial DESIGN review — M-D.b2.f3 (reversible XOR/parity reduce, `iter` ∘ `memexch` + borrow-swap accumulator), `docs/scratch/mdb2-f3-design.md` @ `cfa6fff`

- **Milestone / phase:** M-D.b2.f3 — DESIGN.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the design. Reasoned from code; cargo not run (per instructions; the probe established composition).
- **Reviewed against:** `docs/scratch/mdb2-design.md` (f1), `docs/scratch/mdb2-f2-design.md` (f2), `docs/reviews/mdb2-{design,impl,f2-design,f2-impl}-review.md` (F-1..F-10, I-1..I-8, F2-1..F2-7, F2-I1..I4), `crates/theseus-tisc/src/compile.rs`, `crates/theseus-tisc/src/energy.rs`, `crates/theseus-eval/src/interp.rs`.

## VERDICT: **SHIP-WITH-FIXES** — no blockers, no majors. I could not construct a corrupting input. The three load-bearing claims — reduceStep is exactly `(acc,data)↦(acc⊕data, data)`, the Assoc regroup is a zero-instruction ctx-identity, and the displaced-idx d=4 body is squat-disjoint — all CONFIRM at instruction level (recomputed independently below; my d=3 and d=4 instruction counts land on the probe's 24 and 56 exactly, which is strong mutual corroboration). Five MINOR findings: one garbled acceptance-test spec with a real implementer trap in it (the physical `Xor` writes the *data-home* register, not acc's), one false guard claim (`xor` has no aliasing reject), one missing semantic pin (T4 never starts at `idx0=1` — the one regime where a faithfully-compiled wrong program is invisible to `run==eval`), one under-pinned d=4 shape, and one over-optimistic scope forecast (the f3.5 dot-product will trip `has_inner_control` as sketched).

---

## (A) State enumeration — what each stage assumes, and what defeats it

| Stage | Assumption | Verdict / what defeats it |
|---|---|---|
| marshal-in (`marshal_resident`, compile.rs:2535–2571) | counter tags → Reg0..d−1; acc,data,idx → next Regs; arr trailing 2 → `Mem(0),Mem(1)`; rest zeroed | HOLDS: `home_of` (361–375) sole layout; d=3 head 6, d=4 head 7 ≤ 14; I-4 bounds asserts (2540–2554) |
| `Ctx::new_resident` occ seed (404–410) | `Mem(0),Mem(1)` marked live | HOLDS (408); `allocate_aux` free-slot scan consults occ (616–625) |
| iter aux (`allocate_aux`, 1921) | d=3: Reg6,7 free → rb=6,temp=7, no displacement. d=4: free=[Reg7], displaces the **last** displaceable = idx (leaf 6, 654) to the popped slot = **Mem(2)** (mem popped before scratch, 616–625, 655) → rb=7, temp=6 | HOLDS — traced; matches the probe's "idx displaced to Mem(2)". acc/data are never displaced at d≤4 (needed=1, last-first order) |
| body lowered ONCE (1926–1927) vs `dctx.split(n_counter)` | split preserves absolute homes (413–423); sub-ctx d=3 `[Reg3,Reg4,Reg5,Mem0,Mem1]`, d=4 `[Reg4,Reg5,Mem2,Mem0,Mem1]` | HOLDS; copies are duplicates (1949–1957), layout invariant `debug_assert_eq!` (1938–1942, plus 1724–1729 per lower()) |
| memexch ×2 (recognizer 1001–1061) | F-3 verify (1006–1035) + F-2 guard (1037–1048) pass on the sub-ctx after `Times(Id_acc, ·)` split; guard checks only leaves 2..2+n → arr@Mem0/1, indifferent to idx's home | HOLDS both regimes: d=3 emit `MexchReg{4,5}`; d=4 `with_registers` pages mem-homed idx via spare (pop = Reg7) → `[Mexch{7,2}, MexchReg{5,7}, Mexch{7,2}]` (495–502, 506–511) |
| Assoc regroup | `AssocLProd/AssocRProd` emit **nothing**, ctx unchanged | HOLDS: compile.rs:1780–1786 returns `(vec![], ctx.clone())` — genuine leaf-identity (flattened leaf order of `a×(b×c)` and `(a×b)×c` is identical) |
| reduceStep = `SwapProd;xor;SwapProd` | lowers to `[Exch, Xor, Exch]`, no idiom mis-capture | HOLDS: `match_controlled_idiom` (1284–1328) requires `Distrib` as the 2nd Seq element (1292–1296); `Seq(SwapProd, Seq(Prim(xor), SwapProd))` fails at 1295 and falls through to plain Seq/SwapProd/Prim lowering. `SwapProd(Bit,Bit)` → one `Exch` (1761–1777 via swap_homes 522–527); xorInto → `Xor{rd:regs[1], rs:regs[0]}` (856–863) |
| succ | `SwapSum(One,One)` → `Xori{idx,1}` (1825–1836); at d=4 paged `[Mexch{7,2}, Xori{7,1}, Mexch{7,2}]` | HOLDS; mutates idx's value, never its home |
| unroll copies | spine = exactly 6 (`Addi,Cswap,Swapbr,…,Swapbr,Cswap,Subi`, emit_skip_block 735–816, skip_when_set=false); ctrl = counter tag Reg i, never paged (d≤4 → Reg0..3); no `Xor` in the spine | HOLDS → T1's "exactly 2 Xor" is clean |
| run (fast sim 2829–2858 / real 2678–2748) | `MexchReg` = `reg[rd] ↔ mem[reg[ra]%4]` (2794–2797), `Xor{rd,rs}` = `rd ^= rs` (2769–2771); budget = wcet = rom.len() (no loop_terms); P2: d=3 24+12=36, d=4 56+36=92 < 256 | HOLDS |
| unmarshal-out | aux_po (1959) restores displaced idx to Reg6 before read | HOLDS (LIFO; runs on k=0 too) |
| eval side (interp.rs:190–205) | `iter` applies body exactly k times, counter preserved; memexch/xor by verified table | HOLDS; trajectory matches (D) |

**Recomputed instruction counts (independent of the probe):** d=3 body = `[MexchReg{4,5}, Exch{3,4}, Xor{4,3}, Exch{3,4}, MexchReg{4,5}, Xori{5,1}]` = 6; B = 2×(6+6) = **24**, 4 MexchReg, 2 Xor. d=4 body = 3+3+3+3 = 12; B = 2 aux + 3×(6+12) = **56**, 6 MexchReg, 3 Xor. Both match the probe exactly.

## (B) §8 attack-surface disposition, item by item

**§8.1 — data preservation through reduceStep (THE #1 RISK): CONFIRMED CLOSED, recomputed both semantically and physically.**
Semantic: `SwapProd`: `(acc,data)↦(data,acc)`; `xor = (a,b)↦(a,a^b)` on `(data,acc)` → `(data, data⊕acc)`; `SwapProd` → `(acc⊕data, data)`. Second slot = **data unchanged** — the `(acc⊕data, acc)` corruption is not producible from this composition. Physical (d=3, acc=Reg3, data=Reg4): `Exch{3,4}` → R3=data,R4=acc; `Xor{4,3}` → R4=acc⊕data, R3=data untouched; `Exch{3,4}` → R3=acc⊕data, R4=data. So the second memexch swaps the *original* `data=arr[idx]`-borrowed value back: `arr[idx]` restored, `data` restored. I also attacked the recognizer path: could a *mis-built* xor table (`(a,b)↦(a⊕b,b)`) silently emit something? No — it matches none of the 2-reg patterns (checked add/sub/xorInto/swap/rotl/rotr/3-cycle pointwise over the Bit² domain) → `UnrecognizedPrim`, a clean reject. The one residual corruption channel is a *wrong program faithfully compiled* (e.g. dropping the SwapProd bracket gives `data ⊕= acc`, a valid bijection on which `run==eval` stays green) — closed only by T4's preservation pins; see F3-3.

**§8.2 — Assoc regroup a leaf-identity: CONFIRMED CLOSED.** compile.rs:1780–1786: `AssocLProd | AssocRProd | UniteProd | UnitiProd → Ok((vec![], ctx.clone()))`. Zero instructions, homes and occ byte-identical, so the second memexch sees the same sub-ctx as the first and the F-2 guard's conclusion carries. The regroup only changes the `num_leaves` split point for `Times(reduceStep, id)` — split(2) → `[Reg3,Reg4]`/`[Reg5,Mem0,Mem1]` (d=3) — which is exactly what makes reduceStep contiguous. The "free regroup" claim is literally true.

**§8.3 — displaced idx (d=4): CONFIRMED CLOSED, traced.** idx home = Mem(2) for the whole unroll. Each idx-touching op brackets it: `with_registers` has one mem-op, spares = `[0,1,2,3,4,6,7]` minus used → pop = **Reg7 = rb**. On the executing path rb=0 during the body (skip_when_set=false: `Addi rb; Cswap` moved body_len into… rb only on the *skip* path; executing path leaves rb=0, temp=12). `Mexch{7,2}` squats rb's 0 into mem[2], idx into Reg7; `MexchReg{5,7}` addresses `mem[idx%4]` with idx∈{0,1} → **cannot reach mem[2] or mem[3]**, and never touches Mem0/1 except as the intended resident words; LIFO `Mexch{7,2}` restores both before the closing `Swapbr` needs rb. reduceStep (`Exch{4,5},Xor{5,4},Exch{4,5}`) touches neither Reg7 nor mem[2]. Between instructions idx sits canonically in mem[2]; succ's bracket updates it in place. On the skipped path the body (including all brackets) is jumped wholesale — mem[2] untouched. Squats occur *only at operand homes* (mem_ops are the operands being paged, 495–501), so a squat at Mem(0)/Mem(1) is structurally impossible — arr words are never `with_registers` operands. No perturbation constructible. (Same shape f2's T-F2-7 proved at d=5; T-d4 gives it teeth here.)

**§8.4 — XOR-twice cancellation (d=4, k=3, idx0=0): CONFIRMED, recomputed.** idx trajectory 0,1,0 (succ inside each copy): iter0 `acc⊕=arr0`, iter1 `⊕=arr1`, iter2 `⊕=arr0` → `acc_out = acc0⊕arr1`, `idx_out=1`, data/arr/counter preserved. Eval: interp.rs:195–201 applies BODY exactly k=3 times — identical algebra. CPU: tags `[1,1,1]` → all 3 duplicate copies execute; each copy's `MexchReg` reads the *live* idx (advanced by the previous copy's `Xori`). Both sides compute the same fold; the d=4 domain (4×2×2×2×4 = 128, k=3 included) covers it. Agreement is by construction *plus* T-d4's oracle; the pinned value in T4 is the right semantic witness.

**§8.5 — acc aliasing: CONFIRMED CLOSED.** `home_of` is injective on head leaves (Reg(leaf) for leaf<8, 361–375); acc = leaf d → Reg3/Reg4, disjoint from counter Reg0..d−1, idx, and (via the head/resident split + occ seed) from Mem(0..2). At d=4 `allocate_aux` needs 1 displacement and takes the *last* displaceable (654) = idx — acc stays registered. The only writes to acc's location are reduceStep's bracketed `Exch/Xor/Exch`. Even if a future pop-order change displaced acc instead, the lowering would page it correctly (with_registers handles Mem-homed operands); no silent path.

**§8.6 — park-tax honesty (d=3): CONFIRMED — B=24, T(k)=12+6k, {50%, 25%, 0} all recomputed.** body_len is really 6 (memexch 1 + reduceStep 3 + memexch 1 + succ 1; Assocs 0; no paging at d=3), spine really 6 (735–816; ctrl_pi empty — tags Reg-homed), aux_pi/po empty (Reg6,7 free) → B = 2×12 = 24 = wcet (no loop_terms for iter). rom_trace's steps_t = first PC into `[rom_len,256)` (energy.rs:322–350): skipped copy executes 6 steps (`Swapbr` with rb=6 → pc jumps over the 6 body slots), executed copy 12 → T(k) = 12+6k exactly; T(2)=24=B consistent with rom_trace's no-break default (energy.rs:340). park = 12/24, 6/24, 0 = exactly {0.5, 0.25, 0} (dyadic — exact in f64). The design's "worker MEASURES and pins; a difference IS the finding" is the right posture.

## (C) Findings

### F3-1. MINOR (design-text, must fix before delegation) — §7 T5 is garbled and hides a real implementer trap: the physical `Xor` writes the **data-home** register, not acc's.
The design of record literally contains `"between them sits [Exch, Xor{acc,idx… no}, Exch]…"` — broken spec text in the one test that pins the reduceStep shape. The trap: because `Exch{acc,data}` *precedes* the Xor, the emitted instruction is `Xor{rd = data_home_reg, rs = acc_home_reg}` (`Xor{4,3}` at d=3) — the accumulate lands in acc's home only after the second `Exch`. An implementer who "obviously" pins `Xor{rd=acc_reg}` gets a red test and may fix the assertion in the wrong direction (the L15/L20 class).
**Fix:** rewrite T5 to pin the exact 6-slice `[MexchReg{rd,ra}, Exch{a_r,d_r}, Xor{rd:d_r, rs:a_r}, Exch{a_r,d_r}, MexchReg{rd,ra}, Xori{ra,1}]` with `a_r`=acc home, `d_r`=data home=`rd`, the two MexchReg byte-identical sharing `ra`, plus (F2-I3 lesson) `assert!(matches!(next, Instr::Swapbr{..}))` after the `Xori` (closes body_len=6) and `assert_ne!(rd, ra)`.
**Test:** T5 as rewritten.

### F3-2. MINOR (false claim, doc) — §6's "`xor`/`Exch` aliasing rejects apply" is half-false: the xorInto arm has **no** aliasing reject.
compile.rs:856–863 emits `Xor{rd:regs[1], rs:regs[0]}` with no `regs[0]==regs[1]` check (contrast Exch, 866–877, and MEXCHR, 1054–1058). `Xor{rd,rd}` would zero a live register — but it is unreachable here: distinct leaves have distinct homes (`home_of` injective; split/merge preserve), and `with_registers` assigns distinct regs to distinct homes. So: no break constructible, but the design cites a guard that does not exist.
**Fix:** amend §6 to "Exch/MEXCHR aliasing rejects apply; xor aliasing is structurally unreachable (injective homes)". No code change required.
**Test:** none (not-tested-because: unreachable; noting per L20 discipline).

### F3-3. MINOR — T4 never starts from `idx0=1`, and that is precisely the regime where a faithfully-compiled wrong program is invisible to every oracle in the plan.
`run_on_cpu == eval` (T2/T-d4) cannot catch a *program-construction* bug — both sides execute the same wrong bijection. The §8.1 residual (dropped SwapProd bracket) and succ-ordering errors are caught by T4's idx0=0 pins, but an initial-idx-ignoring error class (address driven by iteration count rather than idx0+j — e.g. a rebuilt succ/marshalling that zeroes idx's contribution) produces *identical* trajectories at idx0=0 and diverges only at idx0=1 (k=1: `acc⊕arr1`, not `acc⊕arr0`). T3 has `idx=1` on the real CPU but asserts against eval, which shares the wrong program.
**Fix + test:** add one T4 case: d=3, `idx0=1, k=1` ⇒ `acc_out = acc0⊕arr1`, `idx_out=0`, data/arr/counter preserved (and keep T4's preservation asserts over all 8 `(acc,data,arr)` combos — they are the load-bearing teeth per §8.1).

### F3-4. MINOR — T-d4 under-pins the displaced regime the probe already measured: nothing asserts B=56, 6 MexchReg, or body_len=12.
As drawn, T-d4 asserts "an `Mexch{_,addr≥2}` present" + the 128-input oracle. A paging regression that *adds* instructions (or a pop-order change that reroutes the bracket but stays correct) passes silently, and the d=4 regime has no length pin anywhere (the §4 histogram is d=3-only). The probe's numbers are in hand; pinning them is free and is exactly the "measured, not assumed" discipline §4 claims.
**Fix + test:** extend T-d4 with `lowered.instrs.len()==56`, `wcet()==56`, MexchReg count == 6, Xor count == 3 (values re-derived in (A); if the worker measures otherwise, that is a finding to surface, not to hardcode over).

### F3-5. MINOR (scope-forecast honesty) — §5's f3.5 teaser ("dot-product… a controlled-op variant (Toffoli-into-acc); a clean f3.5") will NOT be clean as sketched: it trips `has_inner_control`.
`acc ⊕= key ∧ arr[i]` needs a 2-controlled NOT. The controlled idiom's single-instruction recognitions are CNOT and CSWAP only (compile.rs:1367–1388); every other controlled body lowers via a skip-block (1395–1432), and a skip-block inside the iter body is rejected at compile.rs:1931 ("nested skip-block control in iter body — deferred") — the same guard §6 correctly credits for reject-don't-corrupt. There is no CCNOT recognizer and no Cswap-trick lowering for Toffoli today. f3.5 as drawn is a *reject*, not a build, unless a single-instruction Toffoli path (or nested-control support) lands first.
**Fix:** amend the §5 OUT bullet to name the dependency ("needs a CCNOT recognizer or nested skip-block support — not merely 'once XOR-reduce lands'"). No f3 test impact.

## (D) Reversibility judgment (§2) — SOUND
Per iteration from `(C, D, i, arr)`: memexch → `(C, arr[i], i, arr[i←D])` (table verified per-input at lowering, 1006–1035); reduceStep → `(C⊕arr[i], arr[i], i, arr[i←D])`; memexch (self-inverse on the same address — idx untouched between the pair, traced in (B) §8.3) → `(C⊕arr[i], D, i, arr)` — **data and arr restored within the iteration**, so both are invariants of the whole run; succ flips idx. BODY is a composition of involutions (memexch, Exch/SwapProd, notb) and the bijective xor → a bijection; `iter` of a bijection is a bijection with the counter preserved on both sides (interp.rs:190–205; tags untouched by the body — the body sub-ctx excludes Reg0..d−1). Closed form both sides: `acc_out = acc0 ⊕ ⊕_{j<k} arr[(idx0+j) mod 2]`, `idx_out = idx0⊕(k mod 2)` — for both idx0 values and all k including k>n (XOR-twice cancels, (B) §8.4). Inverse = XOR the touched elements back, as claimed. **No `run_on_cpu ≠ eval` input constructible**; the residual risk is the intended-semantics class (F3-3), not divergence.

## (E) Scope / d-choice judgment (§2/§5) — RIGHT CUT
d=3-primary (no displacement, exact 3-point histogram, 96-input whole-domain oracle) + d=4-secondary (k>n **and** displaced idx in one regime, 128 inputs) is the minimal pair covering every regime f3 adds; d=3 alone would repeat the F2-1 mistake (k>n unreachable since k_max=2=n) and the design correctly does not. The "arithmetic sum blocked by value-width, not memexch" claim is **correct**: a leaf is one sum-tag byte ∈ {0,1} (the F-1 thermometer fact; `num_leaves(Sum)=1+max`), so `1+1=2` is unrepresentable in any single-leaf accumulator and `addInto` isn't domain-closed on Bit — the blocker is M-D.c widening, exactly as stated (and F2-3 ordered recorded). Everything IN scope compiles through existing arms (evidence in (A)); the only unbuildable-as-described item is the *OUT*-scope f3.5 teaser (F3-5). No new emission machinery needed — confirmed: the probe's 24/56 counts fall out of the existing lowering with zero deltas.

## Verdict roll-up
**SHIP-WITH-FIXES** (all design-text/test-plan amendments; no code changes):
1. **F3-1:** rewrite T5 with the exact 6-slice incl. `Xor{rd=data_reg, rs=acc_reg}` direction + closing-Swapbr + `rd≠ra` asserts.
2. **F3-2:** correct §6's aliasing-guard claim (xor has none; unreachable).
3. **F3-3:** add the `idx0=1, k=1` semantic pin to T4 (the only oracle-blind error class).
4. **F3-4:** pin the d=4 shape in T-d4 (56 instrs / wcet 56 / 6 MexchReg / 3 Xor).
5. **F3-5:** re-scope the f3.5 dot-product forecast (CCNOT recognizer or nested control is a prerequisite).

No blocker: reduceStep's data-preservation, the free Assoc regroup, the displaced-idx stability, the XOR-twice cancellation, acc non-aliasing, and the 24/12+6k/{50,25,0} histogram all confirm with independent recomputation that lands on the probe's exact numbers. The findings are the familiar claim≠spec class (garbled T5, phantom xor guard, missing idx0=1 pin) — the class this repo closes before shipping, not after.
