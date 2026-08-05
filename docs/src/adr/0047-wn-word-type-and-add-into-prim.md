# ADR-0047: W<n> machine-word type and the `add_into` word-add prim

**Status:** Accepted -- **implemented** (`theseus-check::env`, `theseus-core::prim`,
`theseus-check::elaborate`, `theseus-cli::value`). Adds the first *register-native*
surface word type and the first *builtin-resolved* prim. Complements ADR-0017
(surface syntax / `U<n>`) and ADR-0013 (prim admission).

## 1. Problem

ADR-0017 sketched arithmetic prims over the numeric type, e.g.
`prim add8 : (U8,U8) <-> (U8,U8)`. But two facts make that shape uncompilable to
a TISC ROM:

- **`U<n>` lowers to a bit-tree.** `U8 = U4 x U4 = ... = ` a balanced product of
  eight `1+1` bits (ADR-0017). Its core type has **eight leaves**, so a pair
  `U8 x U8` occupies **sixteen registers**. The compiler's word operations
  (`recognize_prim`, ADR-0042) match a *single-leaf-per-operand* shape; a
  sixteen-leaf pair blows past the register file and past the arm library.
- **A declared prim had no implementation.** `prim p : T <-> T = "key"` recorded
  only a `PrimSig { lhs, rhs, key }`; a *reference* to `p` in an iso body hit the
  `unsupported()` sentinel in `elaborate`. The key string was opaque -- no
  registry ever turned it into a `(forward, backward)` pair. So no declared prim
  could be evaluated or compiled at all.

The TISC datapath *does* have a reversible `Add` (`rd += rs`, byte-wide). What was
missing was (a) a surface type that lowers to the single-register word the `Add`
recognizer expects, and (b) a way for a declared prim's key to resolve to a
concrete definition.

## 2. Decision

### 2.1 `W<n>` -- the machine-word type

Add a builtin type family `W<n>` (`W1`, `W2`, `W4`, `W8`) that lowers to a single
[`Type::Flat(n)`] leaf -- one ISA register -- rather than a bit-tree. `n` is a
power of two in `1..=8`: one register is byte-wide, so a machine word caps at 8
bits. Wider arithmetic is the separate, gate-level `wide_add(k)` story (operand
isos over flattened wires, not an ISA byte-op) and is out of scope here.

`W<n>` is deliberately **distinct from `U<n>`**, not a redefinition:

| | `U<n>` (ADR-0017) | `W<n>` (this ADR) |
|---|---|---|
| Lowering | bit-tree `1+1` nested by products | one `Flat(n)` leaf |
| Widths | any power of two (U1, U8, U16, U32, ...) | 1..=8 only |
| Registers per value | `n` | 1 |
| Role | general numeric / literals | register-native machine word |

Keeping them separate avoids (i) silently changing `U<n>`'s value representation
(and the programs that depend on it), and (ii) the incoherence of `U8` being a
single register while `U16` stays a 16-leaf tree (`Flat` caps at 8). Values of
`W<n>` marshal to/from decimal literals in the CLI exactly like `U<n>`.

### 2.2 The builtin prim registry

`theseus_core::prim::builtin_prim(key, input_ty, output_ty) -> Option<PrimDef>`
resolves a prim key against its declared, *lowered* types. `elaborate`, when it
finds an `Expr::Var` naming a declared prim (not an iso), lowers the prim's
signature, calls `builtin_prim`, `admit`s the result (ADR-0013 mutual-inverse
check over the finite domain), and wraps it in `Iso::Prim`. An unknown key or a
type the builtin does not accept stays `unsupported()`.

### 2.3 `add_into`

The one builtin key today is `"theseus.arith.add_into"`:
`(a, b) |-> (a, (a + b) mod 2^w)` over a `W<w> x W<w>` pair. The addend `a` is
preserved (making it a bijection; the inverse subtracts). It is a **symbolic**
prim -- a bit-level ripple-carry `PrimSpec` (forward add / backward subtract),
*not* a materialized `2^w x 2^w` table: `Flat(w)` values are deep-thermometer
trees, so a table is quadratically heap-heavy (a debug build peaked ~45 GB before
this was made symbolic). The spec is `O(w)` tiny expressions; eval decodes each
input on demand and the compiler certifies + recognizes it symbolically (BDD). At
`w = 8` `recognize_prim`'s selection-by-equiv matches the spec against the arm
library and emits a single reversible `Add`; a byte-wide Fibonacci
`fibStep = swap2 ; add_into`, unrolled, compiles to a clean `EXCH ; ADD` ROM
(conformance `0027-fib8`, and `tests/add_into_prim.rs`).

## 3. Consequences

- A real, register-native word-add is now writable in surface theseus and
  compiles to the ISA datapath -- the U8 Fibonacci that prices in the README
  energy figure is the same program shape `tests/fib_cpu.rs` runs.
- The `builtin_prim` seam generalizes: more keys (sub, xor, rotate, ...) and more
  widths slot in without touching `elaborate`.
- **Limits / follow-ups.** `add_into`'s single-`Add` recognition is `w = 8`
  (byte-native); narrower widths eval correctly (the spec is exact mod `2^w`) and
  cleanly *defer* at compile (no byte-`Add` arm matches, so no miscompile) but do
  not reduce to one instruction. Wider (`W16+`) words need the multi-register
  carry story (`wide_add`), not this prim.

- **Known gaps (surfaced by adversarial review; pre-existing, not add_into-specific).**
  These are checker/UX limitations `add_into` *exposes* but does not introduce;
  each warrants its own focused pass rather than being bolted on here.
  - *Body-vs-signature is unchecked for `= expr` isos.* `iso f : W4*W4 <-> W4*W4 =
    add_into` (with `add_into : W8*W8`) type-checks `ok` because nothing compares
    the elaborated body's core type to `f`'s declared signature (the same hole lets
    `= swap2` at the wrong width pass). For a structural iso this only permutes; for
    `add_into` it can mint values outside the declared type (a `W8` sum at a `W4`
    site). Fix: a whole-iso `type_of(body) == lower(sig)` check (blast radius across
    all isos -> its own change).
  - *Builtin type names are reserved silently.* A user `type W8 = ...` (or `U8`,
    `W08`) is shadowed by the builtin with no diagnostic. No program in the corpus
    declares one; a reserved-name diagnostic is the fix.
  - *Rejected prim references are silent.* An unknown key or unsupported type
    elaborates to the `unsupported()` sentinel, so `check` reports `ok` on a program
    that cannot run. CLI value marshaling also `panic!`s (rather than erroring) on an
    out-of-range literal (e.g. `256` at `W8`) -- both shared with `U<n>`.
