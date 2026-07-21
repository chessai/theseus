# BVEQ — symbolic (BDD) equivalence checking for the prim recognizer

**Status:** design **r2** (post design-review). Author: advisor. Target: the compiler's trust core
(`recognize_prim`), so this carries the CLAUDE.md non-negotiable bar — fresh-context adversarial
design-review AND impl-review, reproducing tests, before any merge.

---

## r2 revision — folding the adversarial design-review (`docs/reviews/bveq-design-review.md`)

Verdict was **SHIP-WITH-FIXES**: the BDD approach is correct and the perf win is **POC-proven**
(measured — see bottom), but the reconciliation was mis-costed, the Slice-2 headline was unreachable as
sliced, and two invariants were incomplete. The r1 body below stands EXCEPT where this r2 overrides it.

**POC-proven (measured, biodivine-lib-bdd 0.6.3, release):** Ccnot 3-operand at **full `Flat(8)`**
(2^24 = 16.7M domain) proven equivalent by BDD in **11 µs** (56 nodes) — a proof the current
thermometer-`Value`+`BTreeMap` representation **cannot construct at any cost** (projected ≈750 s CPU +
O(100 GB) heap). 2-op `Flat(8)`: BDD 40 µs vs the real Value/BTreeMap check path 1.98 s (~50 000×). Q5
stress: a 40-var two-carry-chain expr = 122 µs / 571 nodes; ordering sensitivity graceful (~10×, not
exponential — no multiply in the ISA). **The decisive, unfakeable win is completeness WITHOUT a table**,
not raw speed (raw-u8 enumeration of 16.7M is only 9.6 ms — but the *table it needs* is unbuildable).

**BLOCKER-1 — reconciliation was arithmetically mis-costed.** The per-instruction domain is **256^k,
not k×256**, and the netlist leg (`system_step` via `theseus_eval::eval`) costs **5.56 s per input** →
full-domain pointwise is 101 h (2-op) / ~3 yr (3-op): infeasible ([[maestro-gate-no-timeout]] all over
again). §4/G2 is REPLACED by three legs of differing completeness:
- (L-a) `symbolic ≡ exec_instr` at the **full** per-instruction domain — cheap (ms). **Slice 1.**
- (L-b) `exec_instr ≡ netlist` by **stratified sampling** over operand values **and register selectors**
  rd/rs/rt (the decoder's register-select mux trees are per-index hardware — a fixed (0,1,2) tests one
  path only), bounded to minutes; L19 real-CPU stays the merge authority. **Slice 1.**
- (L-c) the **full-width netlist proof** = a **symbolic netlist walk**: evaluate the decoder's reversible
  gate netlist (not/cnot/toffoli over bit wires, `step.rs`/`decoder.rs`) with BDDs on the wires, operand
  registers symbolic, all else constant — ≤24-var BDDs, well within budget. Promoted from open-question
  Q3 to its **own slice (Slice 2b)** — it is the ONLY path to a machine-complete full-width instruction
  proof.

**BLOCKER-2 — corrected slicing (attack D confirmed).** Verifying Ccnot at full `Flat(8)` needs a
"function B" that the `forward` table cannot provide (unbuildable at 3-op `Flat(8)`). §5 slicing is
REPLACED by:
- **Slice 1** — the `bveq` trust core (BvExpr + BDD `equiv` + `instr_semantics` + `symbolic_exec`) +
  reconciliation legs L-a (full) & L-b (sampled) + the perf POC. **Purely additive, no `recognize_prim`
  change** (confirmed self-contained: needs only `exec_instr` + `run_rom_on_cpu`, both public).
- **Slice 2** — rewire `recognize_prim` arms to `bveq::equiv` at **currently reachable widths** (2-op
  `Flat(8)`; 3-op ≤ `Flat(4)` tables); keep enumeration as a cross-check (Q1); honest claim = "recognizer
  transcription divergence retired" (NOT "machine-verified at full width").
- **Slice 2b** (NEW) — the symbolic netlist walk (L-c): `netlist(Ccnot) ≡ instr_semantics(Ccnot)` at full
  `Flat(8)³`. Composed with Slice 2, the first machine-complete full-width instruction proof — the real
  wall-break.
- **Slice 3** — symbolic `PrimDef` specs (only then is a `Flat(8)³` prim constructible/recognizable, and
  I5-backward becomes load-bearing). Deferring this is correct.

**MAJOR-1 — I4 needs its OUTPUT half (POC-demonstrated break).** On `Flat(w<8)` inputs, byte-`Add`'s low
w output bits are BDD-identical to the mod-2^w `addInto` spec, but output bit w (the real carry) is NOT
identically zero → a low-w-only `equiv` mis-recognizes byte-`Add` as `Flat(4)` `addInto`, driving a
register out of canonical range (silent corruption — the escape `admit()`/`BackwardNotTotal` caught in
the value-width work; symbolic specs have no `admit()`, so the checker must carry it). **Fix:** `equiv`
requires (a) low-w bits ≡ spec **AND** (b) bits w..7 ≡ constant-false. Ship a **byte-Add-at-`Flat(4)`
MUST-REJECT** regression test (L20).

**MAJOR-2 — full-frame equivalence.** `equiv` must compare the **whole register-file transfer** (identity
for untouched regs) plus **non-register quiescence** (BR/scratch/mem/IR untouched, PC uniform), not just
the pattern's operands — else a net side effect on an out-of-pattern register, or a `Swapbr/Xchg/Mexch/
MexchReg` moving state off the register file, compares equal and passes. **Fix:** `symbolic_exec` returns
the full-file transfer and **rejects** (Deferred) any instruction outside the register-pure subset
{Add,Sub,Xor,Exch,Rotl,Rotr,Cswap,Ccnot,Addi,Subi,Xori,Rotli,Rotri} in Slices 1–2 (memexch keeps its
bespoke verify). The sequence-composition argument is sound by induction ONLY given per-instruction
quiescence — so L-b must compare the full post-state, not registers alone.

**MAJOR-3 — the shared var-mapping is itself TCB.** biodivine `Bdd` equality is structural over node
arrays; if the two sides map `(operand,bit) → BddVariable` via different code, two UNEQUAL functions can
produce identical BDDs (the concrete "encoder bug makes unequal equal"). **Fix:** ONE pinned mapping
object used by both encoders + a negative test that deliberately mis-maps one side and asserts `equiv`
breaks. Also: `equiv` returns `Result`, **never panics** across the recognizer boundary (stated
invariant).

**MINORs adopted:** (M1) `symbolic_exec` keyed on concrete register indices so aliasing (`Xor rd==rs`
zeroes) is modeled/rejected + an aliased-sequence test. (M2) perf wording made honest (win =
completeness-without-a-table). (M3) "backward untested" overstated — `admit()` at
`theseus-check/elaborate.rs:755` inverse-checks elaborated prims; I5 is Slice-3 scope, not Slice-1 creep.
(M4) Z3 differential cross-check kept but **feature-gated, test-only, never in the runtime/nix-default
closure**. (NIT) variable-rotate is a barrel mux (not a wire permutation) — wording; keep `Or`+`Mux` in
`BvExpr` (Q2). **Dep:** biodivine-lib-bdd 0.6.3 = MIT, pure Rust, no `unsafe`, no transitive C deps,
maintained — acceptable in the trust path given version-pin + the enumeration fallback (a dep failure
degrades to today's behavior, not corruption).

**Open questions resolved by the review:** Q1 augment (keep enumeration as cross-check ≥1 release); Q2
keep Or+Mux, no Mul (enum-enforced); Q3 → Slice 2b symbolic netlist walk; Q4 `sat_witness` suffices
(decode via the shared mapping); Q5 yes, 2 orders of headroom; R6 holds with the interning caveat (keep
`bveq` pure over `BvExpr`/`&[Instr]`; keep `spec_to_bv` for *patterns* in compile.rs, not in bveq).

---

## 1. Motivation — the recognizer's two walls + the model-divergence risk

`recognize_prim` (compile.rs ~L863) is the bridge from a reversible primitive (a bijection given by its
truth table) to a TISC-1 instruction sequence. Today it is a **linear scan of hand-written pattern arms**,
each verifying the prim by **whole-domain enumeration**:

- `check_pattern_2reg` / `_3reg` / `_2leaf` / `_kleaf` iterate every value `v` in the enumerated domain
  and compare `f(bytes(v))` (a modeled op) against `prim.forward[v]`. Complete, but O(2^(w·k)).
- `PrimDef` (theseus-core/src/prim.rs:85) stores `forward: BTreeMap<Value,Value>` **and**
  `backward: BTreeMap<Value,Value>` — the spec **is** an explicit table.

**Wall 1 — enumeration (the check).** 2-operand `Flat(8)` = 65 536 (fine); 3-operand = 256³ = 16.7M
(infeasible). Today's byte-parallel `Ccnot` is therefore *not* machine-verified at full width — its 8-bit
3-operand behavior rests on a structural argument + a strided real-CPU pass + one discriminating case
(`elems=[2],key=2`), per the value-width ladder notes. Verify-at-`Flat(2/4)`-ship-`Flat(8)` is a
*human* reasoning step at the ship width, not a machine-complete proof.

**Wall 2 — representation (the spec).** The `forward` table is itself 2^(w·k) to *construct*. A 3-operand
`Flat(8)` prim (16.7M entries) can't even be built. So wide prims are unreachable regardless of the check.

**Model-divergence risk.** Every instruction's semantics is transcribed **independently at least three
times**: the netlist decoder (`step.rs:259` `system_step` / `types.rs:273` `decode` — the real authority),
the fast-sim `exec_instr` (compile.rs:2949), and the recognizer's own inline models (the `|a,b|` lambdas
and pinned byte-maps like `mde_walk_bytes`). The recognizer trusts *its* transcription; only the L19
real-CPU oracle catches drift — per-merge, and (for wide ops) only on *sampled* inputs. A divergence that
the strided sample misses = an instruction that passes its own check but computes something else on real
silicon = silent corruption of every program using it.

**Backward untested.** Every `check_pattern_*` reads `forward` only; `backward` is *trusted* to be the
true inverse (constructed via `admit()` / involution). A prim whose backward map is inconsistent slips
past the recognizer.

## 2. Goal

Replace whole-domain enumeration with a **canonical, complete, sound, fast, pure-Rust symbolic
equivalence decision**, so that:

- (G1) instruction↔spec equivalence is proven at **full `Flat(8)` width for any operand count** (breaks
  Wall 1 — Ccnot and wider verified completely, no strided hand-waving);
- (G2) instruction semantics has **one symbolic source of truth**, machine-reconciled against the netlist
  decoder and `exec_instr` (retires model-divergence);
- (G3) `reject-don't-corrupt` is *strengthened*, never weakened: no symbolic spec, or `equiv` returns
  "unequal", ⇒ `Deferred`/`UnrecognizedPrim`, **never emit**. Enumeration remains a sound fallback.
- (G4, opportunistic) `backward == inverse` becomes checkable symbolically.

Non-goal for the first slices: eliminating the `forward` **table** representation (Wall 2, prim
*construction*). That is a separate, invasive theseus-core change (Slice 3), deferred until a real wide
prim demands it.

## 3. Approach — canonical BDD equivalence (`biodivine-lib-bdd`, pure Rust, MIT)

Decision from the backend study (`docs/reviews/…` will capture it; summary here):

- **Primary backend: `biodivine-lib-bdd` v0.6.x (MIT, maintained, pure Rust, no C deps).** Build a
  reduced-ordered BDD **per output bit** for each side; two functions are equal **iff the ROBDDs are
  structurally identical** (canonical). No miter, no CNF, no solver loop — equality is `==`; a
  counterexample is `a.xor(b).sat_witness()` (exact differing input, for error messages).
- **Variable ordering: interleaved by bit position** (all operands' bit 0, then all bit 1, …). This keeps
  ADD/SUB ripple-carry **linear** and rotates/cswap trivial. The ISA has **no multiply**, so we stay
  inside the BDD-friendly regime — the one known blow-up (Bryant's multiplier bound) is unreachable.
- Both sides built in **one shared `BddVariableSet`** (identical order) or the `==` is meaningless.
- **Why BDD over SAT here:** at ≤~40 input bits, straight-line, no multiply, the canonical route is
  *simpler to trust* — the TCB is `biodivine`'s textbook reduce+apply plus **our** encoder, versus
  (our bit-blaster)+(CNF)+(CDCL) for SAT. Pure-Rust SAT (`rustsat`+`rustsat-batsat`, MIT) is the
  swap-in secondary if an op ever escapes polynomial BDD size (it won't, absent multiply).
- **Pure Rust confirmed:** no mature pure-Rust *SMT* exists (oxiz too young for a trust core). Z3/cvc5
  are gold-standard but FFI/subprocess — used **test-only** (see §4), so the runtime stays pure Rust per
  the standing preference.

## 4. The symbolic bitvector model — one source of truth for `Instr` semantics

Introduce a small bit-vector expression layer (new `theseus-tisc` module, `bveq`):

- `enum BvExpr { Var{operand, bit}, Const(bool), Not(_), Xor(_,_), And(_,_), Or(_,_),
  Add(w-vec,w-vec), Sub(..), Rotl(vec,amt), Rotr(..), Mux(sel_bit, then, else) }` — a per-bit boolean
  DAG over `w`-bit operand lanes (`w ≤ 8`).
- `fn instr_semantics(&Instr) -> RegTransfer<BvExpr>` — the **single** definition of what each `Instr`
  does at the bit level (Add = ripple carry, Rotl = barrel network, Cswap = bit-0 mux, …).
- `fn symbolic_exec(&[Instr]) -> RegTransfer<BvExpr>` — symbolically execute a straight-line emitted
  sequence to a per-register `BvExpr`, then lower to per-output-bit BDDs.
- `fn spec_to_bv(pattern|prim) -> RegTransfer<BvExpr>` — lower the *intended* function. For the structural
  arms the intent is already a Rust closure (`|a,b| (a, a^b)`); express it once in `BvExpr`.
- `fn equiv(a: RegTransfer, b: RegTransfer) -> Result<(), Counterexample>` — per-output-bit BDD `==`.

**The reconciliation test (G2 — retires model-divergence).** A property test proves, over the **whole
`Flat(8)` per-instruction input domain** (per-instruction, so 2–3 operands × 256 = cheap), that for every
`Instr`: `symbolic_exec([instr]) ≡ exec_instr(instr) ≡ decoder(instr)`, where `decoder(instr)` runs the
real `system_step` on a single-instruction ROM. Three transcriptions, one machine-checked identity.

**Differential insurance (test-only).** A `#[cfg(feature = "z3-crosscheck")]` test drives Z3 via
`easy-smt` over a corpus of `(instruction-sequence, spec)` pairs and asserts Z3's verdict matches
`bveq::equiv`'s. Not a runtime dependency; cheap insurance for a component whose bug corrupts everything.

## 5. Slicing (one task per delegation)

- **Slice 1 — the `bveq` trust core (spec'd concretely; delegate first).** The `BvExpr` layer, the
  `biodivine` BDD encoder with pinned interleaved ordering, `equiv()` + counterexample, `instr_semantics`
  (the single model), `symbolic_exec`, and the **tri-model reconciliation test** (symbolic ≡ exec_instr ≡
  netlist over the whole per-instruction `Flat(8)` domain) + the Z3 differential test (feature-gated).
  **Purely additive — no `recognize_prim` control-flow change yet.** Establishes and *proves* the trusted
  core in isolation. This is the highest-value, lowest-blast-radius first step, and it directly answers
  the correctness worry that motivated this milestone.
- **Slice 2 — rewire the recognizer.** Replace the structural arms' enumeration with `bveq::equiv`
  (emitted-sequence BDD vs pattern BDD); **extend the 3-operand `Ccnot` arm to a verified-at-full-
  `Flat(8)` proof** (the Wall-1 break, demonstrated). Keep enumeration as the fallback where no symbolic
  spec exists; `reject-don't-corrupt` unchanged. Depends on Slice 1.
- **Slice 3 — symbolic prim specs (deferred).** Let a `PrimDef` optionally carry a `BvExpr` spec so wide
  prims never build a table (breaks Wall 2 for *construction*). Invasive (theseus-core + every prim
  builder); defer until a real wide prim (a crypto/S-box circuit) needs it. Ties directly back to the
  TP-5 lookup-gap finding.

## 6. Invariants & review targets

- **I1 Soundness:** `equiv()==Ok ⇒ equal on all inputs.** Rests on (a) biodivine canonicity [external,
  mature] and (b) **our** `BvExpr→BDD` and `Instr→BvExpr` encoders — the new TCB, defended by §4's
  reconciliation + Z3 differential tests. The review must probe (b) hardest.
- **I2 reject-don't-corrupt:** any failure path (no spec, unequal, encoder error) ⇒ reject, never emit.
- **I3 Ordering blow-up guard:** the interleaved ordering is pinned; a test bounds adder BDD size; no
  multiply is admitted into `BvExpr`.
- **I4 Faithful `w`:** `Flat(w)` for `w<8` must model exactly `w` live bits (upper bits absent, not
  zero-assumed) — the width-honesty that bit the `Add`-Flat(8)-only work.
- **I5 Backward (opportunistic):** verify `backward` is the symbolic inverse of `forward`.
- **Open questions for the design-review:** (Q1) replace vs augment enumeration in Slice 2 — do we delete
  the enumerated arms or keep them as a cross-check for a release or two? (Q2) exact `BvExpr` op scope
  (do we need `Or`, or is `Xor/And/Not` sufficient for the current ISA?). (Q3) the cleanest way to extract
  a per-instruction boolean function from the netlist decoder for reconciliation (single-instruction ROM +
  eval over all input states, vs a symbolic netlist walk). (Q4) counterexample ergonomics — is
  `sat_witness` enough for good `UnrecognizedPrim` messages? (Q5) does biodivine comfortably handle the
  ~24–40-bit var sets of our widest checks (the study says yes; confirm with a spike).

## 7. Authority / success criteria

- **Slice 1:** the tri-model reconciliation test green over the whole per-instruction `Flat(8)` domain +
  the Z3 differential cross-check green on the corpus. (This *is* the deliverable — a proven trust core.)
- **Slice 1 must also ship a PERF POC** that makes the win concrete, not asserted: a benchmark comparing
  `bveq::equiv` against the current whole-domain enumeration on (i) 2-operand `Flat(8)` (65 536 enumerated
  iterations vs one BDD build+compare) and (ii) the 3-operand case where enumeration is 256³ = 16.7M
  (measure it timing out / projected wall-clock) vs BDD in milliseconds. The POC is the empirical evidence
  that the asymptotic win is real at our widths — a gate condition for the design, alongside correctness.
  It also stress-checks Q5 (biodivine at ~24–40-bit var sets stays sub-second).
- **Slice 2:** all existing recognizer tests still green; `Ccnot` proven equivalent at full 3-operand
  `Flat(8)` **symbolically** (the wall-break, first-of-kind); the L19 real-CPU oracle unchanged as the
  ultimate authority (the BDD checker is a *faster, more complete* pre-silicon gate, not a replacement for
  running the real machine).

## 8. What this buys us

- **Completeness where we had sampling:** wide/3-operand ops proven at full width, not argued.
- **One instruction-semantics model,** machine-reconciled — the model-divergence class of silent
  corruption retired structurally, not by hoping the strided real-CPU sample hits the divergent input.
- **Milliseconds, not minutes** for the symbolic check (real-CPU stays the final authority but the *gate*
  gets a complete pre-check).
- **A path to wide-op recognition** (Slice 3) that the current table representation forecloses — the
  infrastructure the TP-5 S-box / crypto direction will eventually need.

## 9. Generalization — toward equality saturation (why this substrate is not throwaway)

Equality saturation (e-graphs; `egg`/`egglog`, both pure Rust) is a future *search/rewrite* layer;
BDD equivalence is a *decision procedure*. They compose — and this design deliberately builds the two
pieces an eqsat layer would reuse, so eqsat is a later addition, not a rework.

- **`BvExpr` is the e-graph's term language.** An e-graph rewrites over a term language (egg's `Language`);
  our symbolic bitvector AST *is* that language. A future e-graph is an e-graph of `BvExpr` terms with
  instruction-semantics as rewrite rules.
- **The BDD oracle becomes eqsat's soundness backstop.** Eqsat's correctness hinges on sound rewrite
  rules — the classic failure mode. The BDD equivalence checker is exactly the *validity oracle* that
  validates each rule (and/or the extracted result) — rule-synthesis-with-a-checker (à la Ruler). The
  trust core built here is reused verbatim.
- **Semantic e-class merging (strictly stronger than syntactic).** Two `BvExpr` terms with identical BDDs
  are semantically equal even when no syntactic rewrite connects them; the BDD-hash is a canonical
  e-class key that merges *observationally* — sound precisely because fixed-width BV is decidable and
  BDDs are canonical (it would not work over a richer theory).
- **The joule cost model is the natural extraction function.** theseus already prices ROMs in measured
  joules — exactly the cost eqsat extracts against. Eqsat over `BvExpr` with instruction-semantics as
  rules + joules as cost would extract the **minimum-energy** instruction sequence for a prim, replacing
  the hand-written pattern library with a cost-optimal synthesizer.

**Design consequence (review target R6):** keep `BvExpr` and the BDD engine **standalone and
recognizer-agnostic** (a `bveq` module with no `compile.rs` entanglement — pure functions over `BvExpr`
and `Instr`), so a future `egg`/`egglog` layer sits on top with zero rework. Do NOT build eqsat now
(heavier machinery, search without a completeness guarantee — premature until a real need such as
min-energy superoptimization or auto-discovered patterns). Build the substrate it will need.
