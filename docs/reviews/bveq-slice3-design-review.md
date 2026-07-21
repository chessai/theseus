# BVEQ slice-3 design review — symbolic prim specs (adversarial, fresh-context)

**Reviewer:** fresh-context Fable 5 (did not write the design). Design: `docs/scratch/bveq-slice3-symbolic-prim-specs-design.md` r1 @ `f6406f8`. Spike: `docs/scratch/slice3-spike-review.rs.ref` (both tests green; promote per L20, do not merge as-is).

*(Advisor's faithful capture for the durable record, L21.)*

## VERDICT: RE-SLICE (mechanism SOUND + spike-PROVEN; but a 3-crate blast radius + 4 under-specified gaps make the full slice unsafe as ONE delegation)

Every existing consumer of a table-absent prim FAILS CLOSED today; the additive `Option<symbolic>` framing keeps table prims byte-identical; and the two critical claims are spike-proven (Q3 whole-domain eval agreement at Flat(4); Q5 full-Flat(8) 3-op recognition via `equiv` in **610µs**, non-vacuous). But the design omits 4 load-bearing decisions (recognizer SELECTION, admit LAYERING, both-present PRECEDENCE, 2 enumeration-HANG guards). Split into **3a** (representation + eval + admit) → **3b** (recognition).

## Q1 — BLAST RADIUS (the gating deliverable)
136 `PrimDef`/`.forward`/`.backward` refs across 10 crates. NO serde/ron on PrimDef; NO energy touchpoint. Change-needed sites:
- **`core/prim.rs` PrimDef struct** — add `symbolic: Option<PrimSpec>` (the change site).
- **`prim.rs` adjoint()** — swaps fwd/bwd; forces the spec to be a **PAIR** (adjoint is total+infallible).
- **`prim.rs` admit()** — **HANG RISK**: `enumerate_values(input_ty)` runs first → Flat(8)³ = 2^24 thermometer Values (tens of GB) BEFORE the clean rejection. Must spec-presence-check BEFORE any enumeration; new explicit `AdmitError` variant.
- **`theseus-eval/interp.rs:269` eval of Iso::Prim** — the load-bearing change (eval-via-spec). theseus-eval deps core only; `bits_of` (netlist) has NO Flat arm → decompose/recompose + expr eval must be **core-native**.
- **`compile.rs recognize_prim`** — **HANG RISK**: k-leaf arms call `enumerate_values(input_ty)` (2^24) before pattern checks; and the design never says how a symbolic prim's emission is **SELECTED**. Must reject symbolic-only prims explicitly BEFORE enumeration.
- **`compile.rs bveq_verify_emission`** — **SILENT-VACUITY**: Step 3 pointwise `for (in,out) in &def.forward`; on an empty table the loop never runs and returns Ok having verified NOTHING (the f3.5 vacuous-oracle class). Hard-reject empty table here.
- **`bveq.rs BvExpr`** — needs `PartialEq,Eq` if embedded in PrimDef (which derives Eq); move to core (Q2).
- **72 literal `PrimDef {` across 27 files** — mechanical `symbolic: None` ripple (compiler-enforced exhaustiveness = zero silent-corruption; but the maestro file_allowlist MUST include all 27 or scope_violation).
Everything else fails closed unchanged (netlist `synthesize_prim`/`prim_matches` reject Flat before reading the table; `synthesize_gate` GateError; etc.).

## Q2 — LAYERING (resolved): move `BvExpr`+`eval_expr` to theseus-core (re-export from bveq; add PartialEq/Eq). BvExpr has zero BDD dep; BDD machinery (expr_to_bdd/VarMap/equiv) stays in tisc. `PrimSpec` (core) = per-output-leaf per-bit BvExpr over (leaf,bit) vars + width, **forward AND backward** (Q4). Admit layering: `core::admit` stays table-only + early-rejects table-absent (no bdd dep in core); `bveq::admit_symbolic(spec)` in tisc does BDD mutual-inverse. Bit convention = LSB-first leaf byte (== value_to_leaf_bytes/VarMap); pin with a cross-crate agreement test.

## Q3 — EVAL GROUND TRUTH (PROVEN): built the Flat(4) masked-accum table (4096 rows, admit Ok) + the bit spec; eval-via-spec (thermometer→byte→bits→eval_expr→recompose) EXACTLY matches the table over the whole domain. **Missing rule to pin:** both-present precedence — **table WINS** in eval (zero behavior change = I1); admit of a both-present prim verifies spec==table over the table domain (divergence rejected at admit, never downstream).

## Q4 — ADMIT (resolved): the spec is a `(forward_spec, backward_spec)` PAIR (adjoint() total+infallible needs a constructive inverse; injectivity-only BDD check gives adjoint nothing to swap). `bveq::admit_symbolic`: backward∘forward=id AND forward∘backward=id per output bit via BDD composition (µs at 24 vars). Backward *table* dies for symbolic prims; backward *spec* mandatory. (Masked-accum is self-inverse → trivial first pair.)

## Q5 — WALL-BREAK (PROVEN): spec RegTransfer (identity except rd.b ^= rs.b & rt.b, 8 bits) vs `symbolic_exec([Ccnot{rd,rs,rt}])` under `equiv(_,_,8,&[rd,rs,rt])` → **Ok in 610µs at full Flat(8), 24 vars**; the 2^24 table never built. Corrupting the spec (OR for AND) → counterexample (non-vacuous). **GAP:** verification presumes a candidate body, but selection today is table pattern-matching. Fix = **SELECTION-BY-EQUIV**: for a symbolic prim, iterate the finite arm library (Add/Sub/Xor/Exch/Cswap/Ccnot/… bodies over with_registers assignments), symbolic_exec each, equiv vs the spec transfer; first pass wins (~15 candidates × ~1ms). Must be written into the design before delegation.

## THE RE-SLICE (delegation plan)
**Slice 3a — representation + eval + admit** (core + eval + tisc guards; NO new recognition):
1. Move BvExpr+eval_expr to core (re-export; +PartialEq/Eq). 2. PrimSpec pair in core; `PrimDef.symbolic`; 72-site `symbolic: None` ripple (27 files in allowlist). 3. adjoint() swaps specs + involution test. 4. core::admit spec-presence early-return (new AdmitError) BEFORE enumerate_values; both-present ⇒ verify spec==table over table domain. 5. `bveq::admit_symbolic` BDD mutual-inverse. 6. eval-via-spec (core-native decompose/recompose) + I2 whole-domain agreement test at Flat(4) (promote Q3 spike). 7. Fail-closed guards + tests: bveq_verify_emission hard-rejects empty table; recognize_prim rejects symbolic-only BEFORE enumerate_values; netlist still rejects.
Exit: a Flat(8)³ prim is representable/evaluable/admissible/adjointable, and every recognizer/lowering path rejects it EXPLICITLY and INSTANTLY.

**Slice 3b — symbolic recognition** (tisc only): selection-by-equiv over the arm library; spec→RegTransfer shim (shared VarMap, type-derived operand_regs — N5); promote Q5 spike incl. the corrupted-spec non-vacuity control; payoff = compile Iso::Prim(Flat(8)³ masked-accum) → [Ccnot] + ring-run vs eval-via-spec on a BOUNDED sample (never 2^24).

## DELEGATION HAZARDS (for the 3a TaskSpec)
- file_allowlist must include all 27 `PrimDef{` files or the mechanical ripple → scope_violation (near-workspace-wide).
- Forbid touching `bveq::equiv`/`VarMap` internals (TCB, r2 MAJOR-2/3) — call, don't adapt.
- Bound every test domain (gate-no-timeout: a 2^24 whole-domain test once ran 3h).
- The two hang-guards (admit, recognize_prim) ship with tests constructing a WIDE symbolic prim and asserting FAST explicit rejection (the failure mode is a hang — use a bounded-time pattern).
- The vacuity guard in bveq_verify_emission needs a teeth test: empty-table prim reaching it must ERROR, not pass.
