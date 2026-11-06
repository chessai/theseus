# ADR-0049: Lane-parallel `BvExpr` evaluation

**Status:** Accepted -- **implemented** (`theseus-core::bvexpr`,
`theseus-core::prim::admit`, `theseus-tisc::bveq`,
`theseus-tisc::compile::bveq_verify_emission`).

## Context

`eval_expr` walks a `BvExpr` tree for one operand vector and one output bit.
Several checks sweep a whole finite domain with it: `admit`'s spec==table check
(ADR-0013), the pointwise step of `bveq_verify_emission` (ADR-0042), and the
full-domain reconciliation test (`tests/bveq.rs` T6, 2 x 2^24 inputs). Each
re-walks the same small trees once per input.

## Decision

1. **Bit-sliced evaluation.** `eval_expr_lanes(expr, vars) -> Lane` evaluates
   one tree for 256 inputs at once. `Lane = [u64; 4]` holds one boolean per
   input; `vars[op][bit]` is the lane mask of that operand bit. Nodes become
   word ops (`Mux` is `(s & t) | (!s & e)`); one walk per 256 inputs, no heap
   allocation. `eval_transfer_lanes` lifts it to a `RegTransfer`;
   `pack_lane`/`unpack_lane` (and `pack_regs`/`unpack_regs`) transpose
   between bytes and planes.
2. **Concrete, not generic.** A generic is compiled at the *calling* crate's
   opt-level, and in dev builds theseus-tisc is opt-level 0 while theseus-core
   is 2. `Lane` is a fixed `[u64; 4]` so the evaluator always runs as
   theseus-core code. Plain `[u64; 4]` loops; LLVM vectorizes them.
3. **Scalar stays the reference.** `eval_expr` / `eval_transfer` are kept for
   single inputs (eval-via-spec, counterexample checks) and as the oracle the
   lane path is property-tested against (random trees, partial chunks, every
   lane word).
4. **Verdicts are unchanged.** Sweeps take chunks in table (BTreeMap) order and
   report the lowest failing lane, and `admit` still checks forward before
   backward, so the `AdmitError` variant and offending `Value` (and the
   `bveq_verify_emission` message) are the scalar ones. `admit` also compares
   the spec output with the table value without decoding it into a `Value`
   (`leaf_bytes_match`): after bit-slicing, decoding (a `Flat(8)` leaf is up to
   256 boxes) was the check's whole cost. Undecodable outputs fall back to the
   decoder, so failures are identical too.

## Not done

- **Truth-table memoization** of subtrees over <= 6 variables (one `u64`
  table each): skipped. `BvExpr` is a `Box` tree, so equal subtrees are not
  shared in memory; memoizing needs a support-set pass and a structural cache,
  while the lane walk already amortizes each node over 256 inputs.
- `admit`'s mutual-inverse checks (BTreeMap lookups over deep `Value`s) now
  dominate `admit`; they do not evaluate `BvExpr`s and are out of scope.

## Consequences

The T6 sweep's symbolic side drops from ~2^25 tree walks to ~2^17; the
remaining time is the scalar `exec_instr` oracle it is compared against.
`admit`'s spec==table check runs about 3x faster on `Flat(8)^2`. Any new
whole-domain sweep over `BvExpr`s should use the lane path.
