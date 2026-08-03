# Surface → compiled circuit

How to take a `.thse` program through every compiler stage to a
verified reversible cell netlist.  This page is the single reference
for the pipeline that is otherwise tribal knowledge scattered across
crate doc-comments and test files.

---

## 1. The pipeline, stage by stage

### 1a. Parse

**Crate:** `theseus-syntax`
**Entry:** `theseus_syntax::parse(src: &str) -> Result<Program, ParseError>`
(`crates/theseus-syntax/src/parser.rs`)

`Program` is:

```rust
pub struct Program {
    pub decls: Vec<Decl>,
    pub span: Span,
    pub depth_overrides: BTreeMap<usize, usize>,
}
```

Each `Decl` is a `Type`, `Iso`, or `Prim` declaration.
The `depth_overrides` map carries any `Name@depth` annotations the
programmer wrote (see §1c).

### 1b. Check -- coverage + linearity

**Crate:** `theseus-check`
**Entry:** `theseus_check::check(program: &Program) -> Vec<Diagnostic>`
(`crates/theseus-check/src/lib.rs`)

Runs four gated stages in order -- Names, Typing, Linearity, Coverage.
A non-empty stage short-circuits the rest.  An empty `Vec` means the
program is clean.

### 1c. Environment resolution

**Entry:** `theseus_check::env::resolve(program: &Program) -> Env`
(`crates/theseus-check/src/env.rs`)

Builds the `Env` (types, constructors, iso signatures, prims) from all
top-level declarations.  Infallible -- name collisions are recorded as
`IsoCollision`s (first definition wins).

Two knobs control μ-type bounded unrolling:

| Knob | Meaning |
|---|---|
| `DEFAULT_UNROLL_DEPTH` (= **4**) | Global default; `Nat` at D=4 has exactly 4 inhabitants (`Zero`, `Succ Zero`, ..., `Succ³ Zero`). |
| `Name@depth` override | Per-use-site annotation parsed into `Program::depth_overrides`; lets one iso expand a recursive type to a different depth than the default. |

`Env::lower(&self, ty: &SurfType) -> CoreType` converts surface types
to core types, unrolling μ-types to the configured depth.

### 1d. Elaboration -- surface → core

**Entry:** `theseus_check::elaborate::elaborate_program(program: &Program, env: &Env) -> BTreeMap<String, ElabResult>`
(`crates/theseus-check/src/elaborate.rs`)

where `type ElabResult = Result<Iso, Vec<Diagnostic>>`.

Each named iso in the program becomes either an `Ok(Iso)` (a
`theseus_core::Iso` -- the core combinator term) or an `Err`.
See §3 for the elaboration constraint that causes `Err`.

Internally, `elaborate` builds a clause table (a `BTreeMap<Value, Value>`)
from the surface patterns, then calls `synthesize` to decompose it
into core combinators.  `synthesize` dispatches to:

- `synthesize_sum` -- left/left + right/right decomposition, or full swap.
- `synthesize_prod` -- independent-factor decomposition; falls back to
  `synthesize_controlled` when the first component is a `Sum`.
- `synthesize_controlled` -- emits
  `SwapProd ; Distrib ; Plus(f_left, f_right) ; Factor ; SwapProd`.

### 1e. Lowering -- core → cells

**Crate:** `theseus-netlist`
**Entries:**

```rust
pub fn lower_iso(iso: &Iso) -> Result<PhasedCellNetlist, LowerError>
pub fn lower_iso_with_ancillas(iso: &Iso)
    -> Result<(PhasedCellNetlist, Vec<bool>), LowerError>
```

(`crates/theseus-netlist/src/lower.rs`)

These produce a dual-rail cell netlist of k-CSWAP and R1 cells.
`lower_iso_with_ancillas` additionally returns the ancilla initial
values (the `Vec<bool>` that must be prepended to data bits before
simulation).

`PhasedCellNetlist` (`crates/theseus-netlist/src/ir/cell.rs`) carries
`inputs`, `outputs`, `cells: Vec<Cell>`, `num_lines`, and per-output
depth annotations.

### 1f. Balancing and energy (brief pointer)

**Balancing:** `theseus_netlist::balance(net)` /
`theseus_netlist::balance_compact(net)`
(`crates/theseus-netlist/src/balance.rs`).
Inserts minimal K0 buffer stages so every cell's reconvergent inputs
are tick-exact.  `balance_compact` stores delays as integers instead of
materializing K0 cells -- necessary for the TISC-1 step's ~1.6 M-buffer
balanced netlist.

**Energy:** the `theseus-spice` crate
(`crates/theseus-spice/src/energy.rs`, `annotate.rs`, `chardb.rs`)
provides analytic energy estimates in joules via per-cell
characterization data from ngspice sweeps.

---

## 2. The CLI

Source: `crates/theseus-cli/src/main.rs`, `crates/theseus-cli/src/pipeline.rs`.

### Available verbs

| Verb | Usage | What it does |
|---|---|---|
| `check` | `theseus check <FILE>` | Parse + check; prints diagnostics. |
| `run` | `theseus run <FILE> <iso> <value>` | Parse + check + elaborate + eval the named iso on one value. |
| `repl` | `theseus repl <FILE>` | Interactive REPL over the elaborated program. |

### Pipeline internals (`pipeline.rs`)

```rust
pub fn diagnostics(src: &str) -> Result<Vec<Diagnostic>, ParseError>
pub fn elaborate(src: &str) -> Result<ElaboratedProgram, ElabError>
pub fn run_iso(iso: &Iso, value: Value, fuel: &mut u64) -> Result<Value, EvalError>
pub fn iso_sig(env: &Env, name: &str) -> Option<IsoSig>
```

`ElaboratedProgram` bundles the `Env`, the elaborated iso map, and the
parsed `Program`.

### Surface value syntax (`value.rs`)

Values on the command line follow this grammar
(`crates/theseus-cli/src/value.rs`):

- **Constructor application:** an uppercase name followed by zero or
  more atom arguments.  `Succ Zero`, `I`, `O`.
- **Tuple:** parenthesised, comma-separated.  `(O, I)`, `(Succ Zero, (I, I))`.
- **Numeric literals:** all-digit tokens are treated as `U<n>` type indices.

```rust
pub fn parse_surface_value(s: &str) -> SVal
pub fn to_core(sv: &SVal, ty: &Type, env: &Env) -> Value
pub fn render(v: &Value, ty: &Type, env: &Env) -> String
```

### Current gap -- no `lower` verb

The CLI pipeline stops at eval.  **There is no CLI verb that emits the
lowered cell netlist.**  To go from surface source to cells you must
call `elaborate` and then `theseus_netlist::lower_iso_with_ancillas`
yourself (see §6 for a proposed fix).

---

## 3. The elaboration constraint

This is the non-obvious gotcha that costs people time.

`synthesize` (and its helpers `synthesize_sum`, `synthesize_prod`,
`synthesize_controlled`) only handles clause tables that decompose
**structurally** -- independent products, controlled operations, and
sum-case dispatch.

A dense permutation truth table (e.g. add-mod-4 over a 4-bit product)
or a cyclic-successor iso **passes `check`** (coverage + linearity are
fine) but **fails to elaborate**: its entry in the map is `Err`.
`check` proves the iso is a valid bijection; `elaborate` asks a
stronger question -- can it be *decomposed* into the core combinator
vocabulary.

**Consequence:** multi-bit reversible arithmetic must be **composed**
from `cnot`/`toffoli`-style controlled primitives (as the TISC-1 `ADD`
instruction and the `fib_gen` ha4/fa5 adders are), not handed over as
a monolithic truth table.

For example, `addInto` (CNOT) elaborates fine because it decomposes as
a controlled-NOT on the second bit.  A hypothetical 4-entry
`addMod4 : U4 <-> U4` that directly lists all 4! permutations would
not.

---

## 4. Verification -- the differential oracle

The project's verification norm for compiled circuits is
**whole-finite-domain agreement**: lower the iso to cells, simulate
every possible input through the cell netlist, and compare against the
interpreter.

The canonical pattern (used in
`crates/theseus-netlist/tests/controlled_lowering.rs`,
`crates/theseus-netlist/tests/iter_lowering.rs`, and
`crates/theseus-netlist/tests/fib_lowering.rs`) is:

```
1. let (net, ancillas) = lower_iso_with_ancillas(&iso)?;
2. for v in theseus_core::enumerate_values(&in_ty) {
3.     let cell_result = eval_cells(&net, &ancillas, &v);   // untimed dual-rail sim
4.     let eval_result = theseus_eval::eval(&iso, v, &mut fuel)?;
5.     assert_eq!(cell_result, eval_result);
6. }
```

`theseus_core::enumerate_values(ty: &Type) -> Vec<Value>` produces
every inhabitant of a finite type.  Because all theseus types are
finite (μ-types are bounded-unrolled), this exhaustively covers the
domain.

This whole-finite-domain agreement **is** the definition of "the cells
compute the intended function."

Additional checks layered on in `iter_lowering.rs`:

- Cell vocabulary: every cell must be ≤ `CellKind::K3` or `CellKind::R1`.
- Balanced timed simulation (`assert_balanced_timed_sim_agrees`):
  `balance` the netlist, then verify agreement again through the
  tick-level simulator.

---

## 5. Worked example: Fibonacci mod 2

### The source program

File: `conformance/programs/0022-fib.thse`

```
type Bit = O | I
type Nat = Zero | Succ Nat

iso swap2 : Bit * Bit <-> Bit * Bit
| (a, b) <-> (b, a)

iso addInto : Bit * Bit <-> Bit * Bit
| (O, O) <-> (O, O)
| (O, I) <-> (O, I)
| (I, O) <-> (I, I)
| (I, I) <-> (I, O)

iso fibStep : Bit * Bit <-> Bit * Bit = swap2 ; addInto

iso fib : Nat * (Bit * Bit) <-> Nat * (Bit * Bit) = iter Nat fibStep
```

`fibStep` is `(a, b) ↦ (b, a ⊕ b)` -- swap then CNOT.
`fib` applies `fibStep` k times via `iter Nat`, computing
`(f_k mod 2, f_{k+1} mod 2)` from the initial pair.

### Running it

```
theseus run conformance/programs/0022-fib.thse fib "(Succ Zero, (I, I))"
```

Input: `k = 1`, initial pair `(1, 1)`.  One application of `fibStep`
gives `(1, 1 ⊕ 1) = (1, 0)`.  The Nat counter is threaded through
unchanged.  Expected output shape: `(Succ Zero, (I, O))`.

Other examples (D = 4, so `Nat` has inhabitants 0-3):

| Input | k | Initial pair | Output pair (f_k, f_{k+1}) mod 2 |
|---|---|---|---|
| `(Zero, (I, I))` | 0 | (1, 1) | (1, 1) -- identity |
| `(Succ Zero, (I, I))` | 1 | (1, 1) | (1, 0) |
| `(Succ Succ Zero, (I, I))` | 2 | (1, 1) | (0, 1) |
| `(Succ Succ Succ Zero, (I, I))` | 3 | (1, 1) | (1, 1) -- period 3 mod 2 |

### The verifier

File: `crates/theseus-netlist/tests/fib_lowering.rs`

**`surface_fib_lowers_and_agrees_with_eval`:** parses `0022-fib.thse`,
elaborates `fib`, calls `lower_iso_with_ancillas`, then runs the
differential oracle over all 16 inputs of `Nat × Bit × Bit` at D = 4
(4 × 2 × 2 = 16).  Every cell-simulation result matches `eval`.

**`compiled_fib_matches_reference_mod2`:** for each k ∈ {0, 1, 2, 3},
checks that `fib ⟨k, (1, 1)⟩ = ⟨k, (f_k mod 2, f_{k+1} mod 2)⟩`
where f_0 = f_1 = 1, verifying the mathematical identity.

Nat encoding in core values: `Zero = Left(Unit)`,
`Succ n = Right(n)`.

---

## 6. Recommended shortcut (proposal)

Today, going from surface source to a cell netlist requires manually
calling `elaborate` then `lower_iso_with_ancillas`.  Two additions
would close this gap:

**A one-call helper** in `crates/theseus-cli/src/pipeline.rs`:

```rust
pub fn surface_to_cells(src: &str, iso_name: &str)
    -> Result<(PhasedCellNetlist, Vec<bool>), /* error type */>
{
    let elab = elaborate(src)?;
    let iso = elab.isos.get(iso_name)
        .ok_or(/* not found */)?
        .as_ref().map_err(/* elab error */)?;
    Ok(theseus_netlist::lower_iso_with_ancillas(iso)?)
}
```

**A CLI verb** in `crates/theseus-cli/src/main.rs`:

```
theseus lower <FILE> <iso>
```

Print the `PhasedCellNetlist` (cell count, line count, ancilla count;
optionally a machine-readable JSON/textual dump).

Neither exists today -- this section is a recommendation, not a
description of implemented functionality.
