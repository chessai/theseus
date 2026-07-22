# BVEQ slice-3b impl review — selection-by-equiv symbolic-prim recognition (adversarial, fresh-context)

**Reviewer:** fresh-context Fable 5, did not write the change. Worktree `/tmp/s3b-verify` @ `1f95d5a` (clean before and after; all teeth corruptions reverted). Diff reviewed in full: `75ff066..1f95d5a`, 4 files, +586/−12 (prim.rs +55, bveq.rs +64, compile.rs +148, symbolic_prim.rs +331). All cargo runs foreground inside `nix develop`; energy.rs never run (memory hazard respected).

*(Advisor's faithful capture, L21. The A-1 test and E-1 doc the review requires were added in `b805c74` on top of `1f95d5a`, teeth re-verified by the advisor: `selection_non_identity_register_mapping` passes at HEAD (symbolic_prim 24/24) and fails under remap-disable with `Deferred("no arm-library candidate matches …")`.)*

## VERDICT: SHIP-WITH-FIXES

The mechanism is sound and the teeth are real: neutering the equiv gate flips **5** tests to failing, so no selection test passes vacuously. The TCB (`equiv`/`VarMap`/`expr_to_bdd`/`symbolic_exec`) is untouched, I1 is preserved by branch structure and green bounded batteries, and the Flat(8)³ payoff genuinely runs at full width with no enumeration. One MAJOR test-gap must be fixed before merge (a ready-made test is provided below, teeth-proven in both directions); two MINOR items should be tracked.

## A. Soundness of selection — SOUND, with one MAJOR test gap

**Mechanism verified correct:**
- **Shared variable ordering:** `bveq::equiv` (bveq.rs:728) constructs ONE fresh `VarMap::new(NUM_REGS)` internally and converts *both* transfers under it — callers cannot desynchronize orderings. `expr_to_bdd` interprets `Var{operand}` as a **register** index, which is exactly the space `remap_expr_operands` maps into; `symbolic_exec`'s transfer is natively in that space. Consistent.
- **Type-derived operand_regs (N5):** `selection_by_equiv` derives `operand_regs` from `with_registers(ctx, &leaves, …)` leaf homes — from the TYPE/ctx, never from the candidate body. The same `operand_regs` feeds both the shim and `equiv`'s Flat(w) constraint.
- **Full-frame comparison:** `spec_to_reg_transfer` starts from `RegTransfer::identity()` (Var(r,b) everywhere), overwrites operand registers' bits 0..w with remapped spec exprs and w..BITS with `Const(false)`. `equiv` then checks (i) all bits of non-operand registers (identity must be preserved — a candidate clobbering a bystander register fails), (ii) low bits of operand registers under the Flat(w) constraint, (iii) width-escape on upper bits of operand registers on **both** sides (the MAJOR-1/N1 branch, bveq.rs:806-832) — so e.g. byte-`Add` for a w<8 XOR-like spec is rejected via its satisfiable carry. A wrong body therefore cannot pass.
- **with_registers composition:** the selected body is proven for exactly the register assignment `with_registers` hands the closure, and scratch/mem-home operands get the page-in/body/page-out bracket around that same body. Correct by construction.
- **Fail-closed edges:** `n_in != n_out` → `Deferred`; `symbolic_exec` errors (aliased regs etc.) → candidate skipped; no candidate → `Deferred`. Arities ∉ {1,2,3} → empty library → `Deferred`.

**Finding A-1 (MAJOR — missing teeth on the soundness core):** Every shipped 3b test uses homes `Reg(0),Reg(1),Reg(2)`, i.e. the **identity** operand→register mapping. I disabled the remap entirely (`remap_expr_operands` returning `Var{operand: *operand}` unmodified) and the shipped suite stayed **23/23 green** — the suite is completely blind to a remapping bug in the shim, the exact attack-A vector. I then wrote a probe test with scrambled homes `Reg(4),Reg(2),Reg(7)`: at HEAD it passes (selection returns `Ccnot{rd:4, rs:2, rt:7}` — the shipped code is **correct**), and under the corruption it fails (`Deferred: no arm-library candidate matches … (12 candidates tried)`). Note the observed failure mode under this corruption is over-rejection, and I verified by analysis that an operand-*order* corruption (reversed `operand_regs`) WOULD be caught by the shipped exact-body assertions — so this is a coverage hole, not a live unsoundness. Per the repo bar (L20: soundness-critical logic ships with a test that fails without it), **add the non-identity-mapping test before merge** — the exact test I ran:

```rust
#[test]
fn selection_non_identity_register_mapping() {
    use theseus_tisc::asm::Instr;
    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));
    let def = PrimDef {
        forward_name: "maskacc8_scrambled".into(),
        backward_name: "maskacc8_scrambled_inv".into(),
        input_ty: ty.clone(), output_ty: ty,
        forward: BTreeMap::new(), backward: BTreeMap::new(),
        symbolic: Some(spec),
    };
    // Scrambled homes: leaf0->r4, leaf1->r2, leaf2->r7 (NON-identity).
    let ctx = theseus_tisc::compile::Ctx {
        homes: vec![
            theseus_tisc::compile::Location::Reg(4),
            theseus_tisc::compile::Location::Reg(2),
            theseus_tisc::compile::Location::Reg(7),
        ],
        occ: theseus_tisc::compile::OccSet::from_locations(&[
            theseus_tisc::compile::Location::Reg(4),
            theseus_tisc::compile::Location::Reg(2),
            theseus_tisc::compile::Location::Reg(7),
        ]),
    };
    let body = theseus_tisc::compile::recognize_prim(&def, &ctx)
        .expect("scrambled-home masked-accum must still be recognized");
    assert_eq!(body.len(), 1, "expected single instr, got {body:?}");
    assert!(matches!(body[0], Instr::Ccnot { rd: 4, rs: 2, rt: 7 }),
        "expected Ccnot{{rd:4, rs:2, rt:7}}, got {:?}", body[0]);
}
```
Teeth proven both directions: passes at HEAD; FAILS under remap-disable AND under the neutered equiv gate. (A follow-up with a `Scratch` home would additionally exercise the page-in bracket on the symbolic path — LOW, optional.)

## B. The payoff is real — PASS

- `payoff_flat8_masked_accum_recognized_as_ccnot` builds `masked_accum_spec(8)` over `Flat(8)³` with **empty** forward/backward tables (`BTreeMap::new()` — the 2^24 table never exists), runs `admit_symbolic`, then `recognize_prim` → asserts exactly `[Ccnot{rd:0,rs:1,rt:2}]` in <2s, then a bounded 8-input oracle (`exec_instr` vs `theseus_eval::eval` via spec) covering corners (0/255/128/mixed). Full width-8, not shrunk.
- No enumeration on the path: `recognize_prim`'s first statement (compile.rs:1010) is the arithmetic `cardinality_u64` precheck; incomplete-table symbolic prims branch to `selection_by_equiv` BEFORE the first `enumerate_values` at compile.rs:1028. `selection_by_equiv`/`generate_arm_candidates`/the shim contain zero `enumerate_values` calls (grep verified; remaining call sites 1028–1729 are the table path and unrelated helpers). Empirically the whole 23-test symbolic_prim binary, including two Flat(8)³ recognitions, finishes in **0.12s** — materializing 16.7M boxed `Value` trees cannot hide in that.
- The R3 partial-table+spec case (1 table entry, Flat(8)³) also completes <2s with no enumeration (see E).

## C. Non-vacuity — PASS, TEETH PROVEN

- `non_vacuity_wrong_spec_rejected`: an OR-instead-of-AND masked-accum spec at Flat(8)³ (computed by no arm) → `recognize_prim` errs, bounded (<5s asserted, ~ms actual). Present and green at HEAD.
- **TEETH (neutered verifier):** I replaced the `bveq::equiv(…).is_ok()` gate with `if true` (first candidate wins unconditionally). Result: **5 tests FLIPPED to FAILED** — `non_vacuity_wrong_spec_rejected`, `payoff_flat8_masked_accum_recognized_as_ccnot`, `recognize_prim_selects_ccnot_for_symbolic_only`, `r3_partial_table_spec_recognize_prim_fast`, and my probe (19 passed / 5 failed). The first 3-op candidate is `Cswap{0,1,2}`, so every exact-body assertion and the wrong-spec rejection genuinely depend on `equiv`. No selection test is vacuous. Corruption reverted.
- **TEETH (remap disabled):** shipped suite 23/23 green (the A-1 gap); probe FAILED. Corruption reverted.

## D. I1 — table-prim recognition unchanged — PASS

- (a) Inspection: compile.rs has exactly **two** hunks — the guard-body swap at 1004-1017 and the appended `selection_by_equiv`/`generate_arm_candidates`. The symbolic branch fires only when `def.symbolic.is_some()` AND the table is incomplete; `symbolic.is_none()` prims (every existing table prim) take a byte-identical path — not one line of the table-matching code (1028–1353) changed. bveq.rs's single hunk appends the shim; `equiv`, `VarMap`, `expr_to_bdd`, `symbolic_exec` diffs are empty. Full-table+spec prims still take the table path (table wins), preserving 3a's I1 certification.
- (b) Bounded batteries all green: `bveq_recognize` 33/33, `decoder` 19/19 (8 ignored, 175s), `composite` 9/9 (2 ignored, 95s), `bveq` 28/28 (incl. the netlist reconciliation, 138s), `bveq_netlist` 4/4 (3 ignored). theseus-core 54/54, theseus-eval 126/126. energy.rs not run per the memory hazard; decoder/composite + untouched-branch inspection stand in for golden confirmation.

## E. Residuals R1/R2/R3 — folded, with one MINOR asymmetry

- **R1 — FOLDED:** `validate_expr_var_indices` (prim.rs) walks every `BvExpr` node in both directions, rejecting `operand >= count_leaves` and `bit >= width`; wired into `validate_against` for forward (vs input leaves) and backward (vs output leaves). Tests `r1_operand_index_out_of_range_rejected` / `r1_bit_index_out_of_range_rejected` assert rejection **and** the message content; both are genuine reproducers — pre-3b `validate_against` had no walk, so both fail on the parent commit by construction.
- **R2 — FOLDED:** `r2_f5_sum_type_decompose_agreement` covers the previously-untested Sum zero-padding arm: whole 32-value `Sum(Flat(4),Flat(4))` domain, core `value_to_leaf_bytes` vs tisc `value_to_leaf_bytes_pub` byte-equal. (I also confirmed core `count_leaves` and tisc `num_leaves` agree on Sum = 1+max — no leaf-count skew feeding the shim.)
- **R3 — FOLDED, semantics legitimately updated, one MINOR flag:** `r3_partial_table_spec_recognize_prim_fast` (Flat(8)³, 1-entry table + spec) completes <2s via selection — the enumeration remains unreachable (the F2 arithmetic precheck is still the first statement; the admit-side `f2_partial_table_with_spec_rejected_fast` teeth test is intact). **MINOR (E-1):** `recognize_prim` now *accepts* a shape `core::admit` *rejects* (`PartialTableWithSpec`), and eval is per-value table-wins-else-spec (interp.rs:270-285) — so for an **un-admitted** prim whose partial table contradicts its spec, eval and emitted code would diverge at the tabled points. Unreachable through the admit-first pipeline (and R3's one entry is spec-consistent), but the asymmetry deserves either a doc comment on `selection_by_equiv` stating the admit-first contract, or tightening selection to empty-table-only symbolic prims. Not merge-blocking.
- 3a residual 4 (`cardinality_u64` debug_assert) — untouched, still LOW/acceptable.

## F. TCB — PASS

- `equiv`, `VarMap`, `expr_to_bdd`, `symbolic_exec` byte-untouched (hunk-level verification: bveq.rs's only hunk is the appended `spec_to_reg_transfer`/`remap_expr_operands`).
- biodivine stays out of theseus-core: `grep biodivine|bveq crates/theseus-core/Cargo.toml` → empty. The shim lives in tisc; core gained only the pure R1 expr walk.
- `clippy --workspace --all-targets -- -D warnings` clean.

## Additional observations (LOW, non-blocking)

- **First-pass-wins ordering:** for a 1-op identity-like spec, `Addi{imm:0}` would win before `Xori{imm:0}` — both are *correct* code (soundness unaffected), but selection order is part of emitted-code determinism; fine as-is since the library order is fixed.
- The 2-op comment "symbolic_exec rejects rd==rs, so skip" is moot (the two regs are always distinct) — cosmetic.
- Duplicate `operand_regs` entries (an invalid ctx) would silently overwrite in the shim; that invariant predates this change and holds for every `with_registers` caller.

## Worktree & tallies

Both corruptions (T1 equiv-gate neuter in compile.rs, T2 remap-disable in bveq.rs) and the probe test were **reverted**; `git status --short` is empty at `1f95d5a` and the post-revert re-run is green. Exact tallies observed: clippy clean; theseus-core **54/0**; theseus-eval **126/0**; symbolic_prim **23/0** (0.12s, incl. both Flat(8)³ recognitions); bveq **28/0** (137.7s); bveq_recognize **33/0**; bveq_netlist **4/0** (3 ignored); decoder **19/0** (8 ignored); composite **9/0** (2 ignored). Flip runs: neutered equiv → symbolic_prim **19 passed / 5 FAILED**; remap disabled → shipped 23 passed / probe FAILED. energy.rs deliberately not run.

**Required fix before merge:** add the non-identity operand→register mapping test (A-1, provided above, teeth-proven). Recommended: the E-1 doc-or-tighten on partial-table+spec selection.

---

## Advisor disposition (post-review)

- **A-1 (MAJOR): DONE** — `selection_non_identity_register_mapping` added verbatim (`b805c74`); advisor re-verified teeth: passes at HEAD (symbolic_prim 24/24), fails under remap-disable with `Deferred("prim 'maskacc8_scrambled': no arm-library candidate matches the symbolic spec (12 candidates tried)")`.
- **E-1 (MINOR): DONE** — doc comment on `selection_by_equiv` states the admit-first contract (`core::admit` rejects `SpecTableMismatch`, so a recognized symbolic prim's table is always spec-consistent and eval's table-wins path cannot diverge). Chose doc-the-contract over tighten-to-empty-table because R3's fast-reject case legitimately carries a 1-entry table.
- **LOW observations:** left as-is (first-pass-wins determinism is intended; cosmetic comment / duplicate-operand invariant predate this change).
