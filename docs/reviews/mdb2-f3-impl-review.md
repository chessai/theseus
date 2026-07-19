# M-D.b2.f3 — IMPLEMENTATION review (adversarial, fresh-context)

- **Milestone / phase:** M-D.b2.f3 (reversible XOR/parity reduce) — IMPLEMENTATION (test-only).
- **Branch reviewed:** `maestro/01KXWD6AMRE2ERXCPFC7SBW353` (d4e35dd), base `3835875`.
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change.
- **Verdict:** **SHIP-WITH-FIXES — zero blocking.** Every pinned expected value recomputed
  correct (T4 trajectory table, B=24, T(k)=12+6k, park {50,25,0}, d=3 4/2, d=4 56/56/6/3); the
  F3-1 Xor-direction pinned correctly (`Xor{rd:data,rs:acc}`); no vacuity; `#[ignore]` = the
  designed OOM tier. 3 MINOR follow-ups (T5 derives a_r; a self-contradictory T3 comment; a T4
  mislabel) + 1 cosmetic.
- **Disposition:** the 3 minors fixed as advisor-finish (test-only). Committed verbatim per L21.

---

# Adversarial IMPLEMENTATION review — M-D.b2.f3 (reversible XOR/parity reduce), `crates/theseus-tisc/tests/xor_reduce.rs` @ `maestro/01KXWD6AMRE2ERXCPFC7SBW353` (`d4e35dd`)

- **Milestone / phase:** M-D.b2.f3 — IMPLEMENTATION (test-only).
- **Reviewer:** fresh-context Fable (claude-fable-5), did not author the change. Cargo not run (per instructions); every load-bearing value below is recomputed by hand from the design, the src (unchanged on this branch — verified: the diff vs merge-base is exactly one file, +753 lines), and the f1/f2 lineage.
- **Reviewed against:** `docs/scratch/mdb2-f3-design.md` (post-F3-1..F3-5), `docs/reviews/mdb2-f3-design-review.md`, `crates/theseus-tisc/src/compile.rs` (memexch table 1197–1276, xorInto recognizer 856–863, marshal/leaf-bytes 141–171 & 2535–2614, run oracles 2863–2928), `energy.rs` (rom_trace 321–360, rom_program_energy_resident 450–490), `asm.rs` (Instr 103–137), `theseus-core` (`numeral_ty` ty.rs:38–44, Assoc typing typing.rs:177–185), `theseus-eval` (iter interp.rs:190–205, numeral_value 288–300), and the f2 template `tests/array_map.rs`.

## VERDICT: **SHIP-WITH-FIXES — nothing blocking.** No wrong expected value anywhere (I recomputed every pinned constant independently: all of B=24, T(k)=12+6k, park {0.5, 0.25, 0}, the d=3 counts 4/2, the d=4 shape 56/56/6/3, and every T4 acc/idx trajectory are RIGHT). No vacuous default-tier test; `#[ignore]` is exactly the designed set {T3, T6b, T7}. The F3-1 Xor-direction trap was NOT stepped in: T5 asserts `Xor{rd: data_home, rs: acc_home}` (line 530), the correct direction. Three MINOR findings: one real (if compositionally-covered) under-constraint in T5's derivation of the acc register, and two wrong comments (one of which mislabels a design finding). All are follow-up-commit material; none certifies a wrong value.

---

## (A) The program built == the design — CONFIRMED, node by node

`xor_reduce_body()` (lines 138–176) builds exactly the design §2 body, in order:

- **Carrier:** `carrier_ty(d) = Nat_d × (acc × (data × (idx × (arr0 × (arr1 × 1)))))` (lines 44–62). `numeral_ty(d)` is the ADR-0035 numeral chain (ty.rs:38–44, `d` sum-leaves), so at d=3 the head is leaves 0–5 → Reg0..5 (counter 0–2, acc=Reg3, data=Reg4, idx=Reg5) and arr = trailing 2 → `Mem(0),Mem(1)` under `home_of` with resident=2. arr trailing ✓, head = d+3 ✓.
- **memexch leaf:** `memexch_prim(&bit(), 2)` (line 68) — its carrier per compile.rs:1227–1239 is `data × (idx × (arr0 × (arr1 × 1)))`, which is byte-for-byte the second factor of `body_ty()`. So `Times(Id(bit), mexch)` (line 143) is well-typed with acc leading ✓. The **same** `mexch` value is cloned for both occurrences (lines 143, 172) — the two memexches are identical by construction ✓.
- **Assocs:** `AssocLProd(b, b, idx_arr_ty)` maps `acc×(data×rest) → (acc×data)×rest` (typing.rs:177–180) and `AssocRProd` back (182–185) — the exact regroup, with `rest = idx×(arr0×(arr1×1))` ✓.
- **reduceStep** (lines 122–127): `SwapProd ; xor ; SwapProd` with `xor_prim = (a,b) ↦ (a, a^b)` — the table is built with the *compiler's own* `value_to_leaf_bytes_pub`/`value_from_leaf_bytes_pub` (lines 75–99), so encoding mismatch is impossible by construction. The forward table matches the xorInto recognizer pattern (compile.rs:856: `|a,b| (a, a^b)`) and cannot be mis-captured by the earlier addInto/subFrom checks (they differ from xor at (a,b)=(1,1) resp. (1,0)). The backward table (lines 88–98) maps every `(a,c) ↦ (a, a^c)` — the correct inverse, since xor-into is an involution. `Seq(SwapProd, Seq(Prim, SwapProd))` cannot trip `match_controlled_idiom` (needs `Distrib` in second position, compile.rs:1292–1296) ✓.
- **succ:** `SwapSum(One,One)` on the idx leaf, wrapped `Id_acc ⊗ (Id_data ⊗ (succ ⊗ Id_arr))` (lines 165–169) — the right leaf, `arr_ty` correctly `arr0×(arr1×1)` ✓.
- **Composition order** (lines 171–180): left-nested `Seq` of exactly `mexch ; assocL ; (reduceStep⊗id) ; assocR ; mexch ; succ` ✓. `xor_reduce_iso(d) = Iso::iter(numeral_ty(d), body)` ✓.

No mis-order, no wrong leaf, no type mismatch. The program IS the design's program.

## (B) T4 expected values — ALL RIGHT (independent recomputation)

Per-iteration effect (recomputed from the memexch table compile.rs:1246–1263 and the reduceStep algebra): memexch → `data' = arr[idx]`, `arr[idx] ← data`; SwapProd;xor;SwapProd → `(acc⊕data', data')` with slot 2 preserved; memexch (same address — idx untouched in between) → data and arr[idx] both restored; succ → idx ⊕= 1. Net per iteration: `acc ⊕= arr[idx_current]`, idx flips. Trajectories:

| case | touched | expected acc_out | expected idx_out | test asserts (line) | verdict |
|---|---|---|---|---|---|
| d=3, idx0=0, k=1 | arr[0] | `acc⊕arr0` | 1 | `acc ^ arr0`, idx 1 (≈365–370) | **RIGHT** |
| d=3, idx0=0, k=2 | arr[0],arr[1] | `acc⊕arr0⊕arr1` | 0 | `acc ^ arr0 ^ arr1`, idx 0 (≈379–386) | **RIGHT** |
| d=3, idx0=1, k=1 (F3-3) | arr[1] | `acc⊕arr1` | 0 | `acc ^ arr1`, idx 0 (405–411) | **RIGHT** |
| d=4, idx0=0, k=3 | arr[0],arr[1],arr[0] | `acc⊕arr1` (arr0 cancels) | 1 | `acc ^ arr1`, idx 1 (430–440) | **RIGHT** |

Every case additionally asserts counter `k_out==k`, `data_out==data`, `a0_out==arr0`, `a1_out==arr1` — the load-bearing preservation teeth — over **all 16** `(acc,data,arr0,arr1)` combos per case (a superset of the design's "all 8"). The value encodings are correct against the compiler: `numeral_val` builds `inrᵏ(inl ())` (lines 191–197), exactly what `numeral_value` counts (interp.rs:288–300) and what marshals to the thermometer tag bytes `[1;k]++[0;d−k]` (value_to_leaf_bytes: Left→0, Right→1, compile.rs:150–162); `bit_val` Left=0/Right=1 matches the tag byte. `decompose_output` inverts faithfully. T4 asserts against **eval** of the built Iso — which is the correct oracle for the wrong-program-faithfully-compiled class (T2/T-d4 separately close eval==CPU over the *entire* domain, so nothing escapes through the eval-only choice). No wrong constant anywhere in T4.

## (C) T5 — the F3-1 direction is pinned CORRECTLY; one under-constraint (finding 1)

- Line 530: `assert_eq!(instrs[pos1+2], Instr::Xor { rd: d_r, rs: a_r })` with `d_r = rd1` = the MexchReg **data** register. This is the right physical direction — the leading `Exch{acc,data}` swaps first, so the accumulate lands in data's home (at d=3 concretely `Xor{4,3}`); an author who had asserted `rd=acc` would be red. The test asserts the strong form (full-instruction `assert_eq!`, `Instr` derives `PartialEq` at asm.rs:103), not something weaker.
- Both MexchReg per copy byte-identical: `rd1==rd2`, `ra1==ra2` (≈482–493); `assert_ne!(rd1, ra1)` (494); `pos2 == pos1+4` (504) pins the 3-instr reduceStep between them; the two Exch are asserted equal (≈536); `Xori{rd: ra, imm: 1}` after the second MexchReg (≈540); closing `Swapbr` right after (549) pins body_len=6 — the F2-I3 lesson, applied. Exactly 4 MexchReg asserted, paired per copy in ROM order (copies are sequential in the unroll) ✓.
- **Finding 1 (MINOR):** `a_r` is *derived* from the first Exch's other operand (510–524) and never independently pinned to acc's home, nor asserted distinct from `ra` or the counter registers. T5-in-isolation would accept the pathological slice `[MexchReg, Exch{ra,d_r}, Xor{d_r,ra}, Exch{ra,d_r}, MexchReg, Xori{ra}]` (acc never touched, "xor idx into data"). This is compositionally closed — such a lowering breaks `acc ⊕= arr[idx]` and T2's 96-input whole-domain oracle plus T4 kill it — so it is not a vacuity of the suite, only of T5 alone. But the design-review F3-1 fix spec said `a_r = acc home`, and at d=3 the layout is deterministic (acc=3, data=4, idx=5). **Fix (follow-up):** add `assert_ne!(a_r, ra)` and, ideally, the absolute pin `assert_eq!((a_r, d_r, ra), (3, 4, 5))` for the d=3 ROM.

## (D) T6a — a real measurement, default tier — CONFIRMED

Not `#[ignore]` ✓. `b = lowered.wcet()` pinned `== 24` (570) — recomputed: 2 copies × (6-instr spine + 6-instr body) = 24 = rom len, no aux at d=3 (Reg6,7 free). `trace.steps_t` comes from `rom_trace` (energy.rs:338–360: first PC into the dead zone — a measurement of the executed ROM, independent of the park formula) and is asserted `== 12+6k` (583–588); recomputed: skipped copy = 6 spine steps, executed copy = 12 → T(0)=12, T(1)=18, T(2)=24. `steps_b == b` asserted. Park recomputed from `(b − steps_t)/b`: {0.5, 0.25, 0} — dyadic, the `1e-10` tolerances are safe — plus the strict-decrease loop. Teeth: a spine/body_len/wcet regression moves `b` or `steps_t` and fails the exact pins. T(2)=24 hits `rom_trace`'s "never entered dead zone" default, but the default equals the true value here and any slowdown would first break the `b==24` pin (wcet is computed from the ROM). Honest and non-tautological.

**T6b** (`#[ignore]` ✓): `total_j == B·step_j` is checked against energy.rs:474, where `total_j` is *literally computed* as `steps_b as f64 * step_j` — the test's comment "model identity (not a falsifiable check)" (≈636) is exactly honest, and the falsifiable content is carried by `steps_b==b`, `steps_t==12+6k` (an independent re-trace inside `rom_program_energy_resident`, energy.rs:467) and `step_j > 0`. Correct per design T6 split.

## (E) T-d4 — has teeth — CONFIRMED

All four shape pins present: `instrs.len()==56` (704), `wcet()==56` (710), MexchReg `==6` (721), Xor `==3` (731) — recomputed: aux pi/po 2 + 3×(6 spine + 12 paged body) = 56; 2 MexchReg + 1 Xor per copy × 3. Displacement pin `Mexch{_, addr≥2}` present (≈690–698) — the idx page brackets emit `Mexch{7,2}`, and resident arr words are never `with_registers` operands so no false positive from addr 0/1. Whole-domain oracle over all 128 (asserted `domain.len()==128`, 738) with explicit `has_k3` presence check (741). An occ-seed/pop-order regression either changes the instruction count (fails 704/710), removes the displacement (fails 698), or corrupts paging semantics (fails the 128-input oracle incl. k=3). Real teeth.

## (F) Vacuity / `#[ignore]` audit — CLEAN

- `#[ignore]` on exactly **t3** (326), **t6b** (610), **t7** (647) — precisely the design's OOM tier. All six default-tier tests (t1, t2, t4, t5, t6a, t_d4) run.
- No unreachable assertion body anywhere: every loop domain is non-empty by construction or asserted (96/128 pinned before iteration; `0..=1` ranges; `k in 0..d`); no early returns; T2's presence flags `has_idx1`/`has_k2` are asserted (309–310) before the oracle loop, and the oracle loop asserts inside for every input.
- T1 counts are exact-equality on the whole ROM (4 MexchReg / 2 Xor — the spine contains neither, and `Xori` is a distinct variant from `Xor`, asm.rs:137 vs the Xor variant, so no count pollution).
- All imports/APIs verified against src: `run_on_cpu_resident(iso, input, resident, eval_fn)` (compile.rs:2863), `run_on_cpu_fast_resident` (2893), `marshal_resident` (2535), `rom_trace(rom, init, budget)` (energy.rs:338), `rom_program_energy_resident(iso, input, resident, db, t_ramp, step_net)` (450), `system_ring`/`bus_quiescent` (ring_run.rs:369/481), `encode_system`/`decode_system` (types.rs:369/381), `step`/`system_step` (step.rs:237/259), `lower_iso`, `rung0_measured` (chardb.rs:159), `value_{to,from}_leaf_bytes_pub` (2607/2612), `Xori{rd,imm}`/`Mexch{rd,addr}`/`MexchReg{rd,ra}` field names (asm.rs:127–137) — and the T6a/T6b/T7 bodies are structurally the proven-green f2 template with f3 values substituted.
- T7's sample `make_input(2,1,0,1,1,0)` satisfies the designed `idx=1, k≥1, acc≠0` regime ✓.

## Findings

1. **MINOR — T5 derives `a_r` instead of pinning it** (lines 510–524). T5 alone would pass an acc-never-touched slice with `a_r==ra`; closed only compositionally by T2/T4. Fix (follow-up): `assert_ne!(a_r, ra)` + the absolute d=3 pin `(a_r,d_r,ra)==(3,4,5)`.
2. **MINOR (comment-only, but a wrong claim in a test) — T3 sample-3 comment is self-contradictory** (line 334): "`data=0≠arr[1]=0 OK`" — 0 equals 0; that sample violates the `data≠arr[idx]` regime it claims. Coverage is intact (sample 1, line 332, satisfies all four design constraints: k=1, idx=1, acc=1, data=0≠arr[1]=1), so no assertion is weakened. Fix: correct the comment (or change sample 3 to `arr=[1,1]`).
3. **MINOR (traceability) — T4's d=4 XOR-twice block is labeled "F3-4"** (lines 419, 435). F3-4 is the *T-d4 shape pin* finding; the k=3 semantic pin was part of the design's T4 spec (§7/§8.4) all along. A future reader auditing finding→test coverage will mis-map. Fix: relabel to "design T4 / §8.4 (XOR-twice-cancels)".
4. **COSMETIC — `xor_prim`'s backward loop** (lines 88–98) is a verbatim duplicate of the forward loop; correct (involution), but `let backward = forward.clone()` with an F-8-style comment (as `memexch_prim` does at compile.rs:1266) would state the intent.

## Verdict roll-up

**SHIP-WITH-FIXES — zero blocking; findings 1–3 as one follow-up commit, finding 4 optional.** The things this review was sent to hunt did not materialize: every pinned expected value recomputes correctly (the full T4 trajectory table, B=24, T(k)=12+6k, park {50,25,0}, d=3 4/2, d=4 56/56/6/3); the F3-1 trap was pinned in the correct direction (`Xor{rd:data, rs:acc}`, line 530); no default-tier assertion is vacuous or unreachable; the `#[ignore]` set is exactly the designed OOM tier; and the encodings (numeral, bit, leaf-bytes) are self-consistent with the compiler by construction. The suite as merged would certify the right thing.
