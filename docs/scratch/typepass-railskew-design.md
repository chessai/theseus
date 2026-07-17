# Type-pass #5 — checked rail-arrival accessor (the coherent "rail-pair" refactor)

Status: DESIGN — pre-review. Canonical branch `chessai/theseus-v2`, tip `0297099`.

## The correction
`RailPair { line0: LineId, line1: LineId }` (`theseus-netlist/src/ir/cell.rs:50`) is a
STRUCTURAL pair of line IDs; it carries no arrival. The "skew" invariant lives in the
timing certificate (`theseus-tisc/src/ring_run.rs:1165`):
```rust
let a0 = arrivals[rp.line0.0 as usize];
let a1 = arrivals[rp.line1.0 as usize];
assert_eq!(a0, a1, "output rail pair skew");   // <-- the scariest untested assert
```
`arrivals` = `PhasedCellNetlist::line_arrivals()` (netlist-global). So skew is a DYNAMIC
property of a *specific balanced netlist*, not a static property of `RailPair`. Making a
"skewed RailPair unconstructible" is therefore incoherent. The coherent typed version is
a **checked accessor** that returns the invariant as a `Result`, moved INTO the netlist
crate (where the arrival data lives) and propagated by the certificate.

## The change (additive; REUSES existing skew machinery — Fable review fixes folded)
CRITICAL (review finding): theseus-netlist ALREADY expresses this invariant —
`PhasedCellNetlist::validate()` has a `pair_arrival` helper (cell.rs ~526-533) that
returns the EXISTING `CellNetError::PairSkew` (cell.rs ~273-274) when a pair's two rails
arrive on different ticks, and it already applies it to output ports (cell.rs ~543-546).
Do NOT add a new `RailSkew` enum (that would be a 3rd spelling of one invariant). Reuse
`CellNetError::PairSkew`.

1. **theseus-netlist** — add to `PhasedCellNetlist` (factoring the EXISTING
   `pair_arrival` logic used by `validate()`, do not re-derive skew semantics):
   ```rust
   /// Arrival tick of each OUTPUT port, verified both rails co-arrive.
   /// Err(CellNetError::PairSkew{..}) if any output pair's rails differ.
   pub fn checked_output_arrivals(&self) -> Result<Vec<u32>, CellNetError>;
   ```
   Impl: `let arrivals = self.line_arrivals()?;` then for each `self.outputs[i]`, use the
   SAME `pair_arrival`-style check (extract/share it with `validate()`), collecting the
   per-port arrival or returning the first `PairSkew`. Uses `line_arrivals()` (NOT
   `output_depths`). It must NOT call `self.validate()` (the negative test builds a net
   that fails `validate()`'s other checks — see step 3). It checks ALL output ports
   (strictly stronger than the old first-`n_data` check — intentional; document it).
2. **theseus-tisc** — `ring_run::two_body_timing_certificate`:
   - replace the inline `max_data_output_arrival` closure's `assert_eq!(a0,a1,…)` with a
     call to `net.checked_output_arrivals()?`, taking `max` over the first `n_data`
     entries. **Feed `bus_line_pads` (ring_run.rs ~1201) from the SAME checked vec**
     (`checked[bus_offset + i]`) — the bus range ⊂ the data range
     (`CPU_BUS_OFFSET = STATE_BITS`, `CPU_BODY_N_DATA = STATE_BITS + BUS_BITS`) — so there
     is ONE arrivals computation, not two that could drift.
   - signature becomes `-> Result<TwoBodyTiming, CellNetError>` (propagate). The fn keeps
     its other `.expect()`/`n_data` asserts (out of scope); only skew becomes typed.
   - Callers: `tests/ring_two_body_timing.rs` at **4 sites** (lines ~40, 70, 78, 155) →
     `.expect("no rail skew")`. `validate()` + the whole `validate_rejects_mutations`
     block operate on the built struct → unaffected. Confirm no `src/` caller exists.
3. **Negative test (the L20 reproducing test)** — hand-build a minimal `PhasedCellNetlist`
   whose one output pair mixes lines from TWO drivers so the rails arrive at different
   ticks (e.g. inputs `[{0,1}]`, one K0 `{0,1}→{2,3}`, output `[{line0:2,line1:0}]` →
   arrivals 1 vs 0). Assert `checked_output_arrivals()` returns
   `Err(CellNetError::PairSkew{..})`. Plus a positive test: a co-arriving net → `Ok` with
   the right per-port arrivals. Struct fields are `pub` (cell.rs:223-241), so the test may
   live in EITHER crate; put it in theseus-netlist (beside the accessor). NOTE the skewed
   net deliberately fails `validate()` (LineConsumers) — that is WHY the accessor must not
   call `validate()`.

## Honest reachability (corrected rationale)
The skew Err is **structurally UNREACHABLE on every crate-produced net**: `line_arrivals`
assigns both lines of a pair together (cell.rs ~394-397), and EVERY `RailPair`
construction site (`fresh_pair` lower.rs, `alloc_pair` balance.rs, bennett.rs, `pad_ring`
ring.rs, `swapped()`) allocates a pair WHOLE from one driver — no path mixes lines across
drivers. So both rails always co-arrive by construction (this is structural per-pair, NOT
a property of balancing — the certificate nets are UNBALANCED). Like `decode_*`, this is
**dead-but-tested**: the value is a formalized tripwire for a future optimization that
remixes lines across pairs (or IR corruption), which the negative test defends. This is
the honest justification for the change — not a live bug.

## Why this is safe
- ADDITIVE (one new method reusing existing `PairSkew`); no change to `RailPair`, the
  balancer, lowering, or sim. Only behavior change: `ring_run`'s skew panic → typed
  propagation, `Ok` on every real net (golden pin d_pre=2839/d_mem=35/d_post=152 preserved
  exactly, since `line_arrivals` is the unchanged source).
- Full cross-crate regression (theseus-netlist + theseus-tisc) is the behavior proof.

## Risks the review MUST probe
- **R1:** Is `checked_output_arrivals` semantically identical to the old inline logic
  (same arrivals source, same per-port max, same n_data slice)? A subtle divergence
  (e.g. using `output_depths` vs `line_arrivals`, or line0 vs line1) would silently
  change `d_pre/d_mem/d_post` → wrong certificate. Confirm it reproduces the EXACT
  values (golden pin: d_pre=2839 etc. must still hold).
- **R2:** Does making the certificate return `Result` ripple beyond the timing test?
  (Any other caller of `two_body_timing_certificate`?) Enumerate + confirm.
- **R3:** The negative test hand-builds a `PhasedCellNetlist` — is that constructible
  in a netlist unit test without going through lowering? Confirm the struct fields are
  reachable (`pub`) or a test builder exists; if not, the test must use `#[cfg(test)]`
  inside the netlist crate.
- **R4:** Is the skew genuinely unreachable on real nets (making Err dead-but-tested,
  like decode_*), or CAN a real lowering produce a skewed output pair? If the balancer
  guarantees output-pair co-arrival, say so (the Err is a fail-fast formalized as a
  type); if not, that's a real latent bug the accessor now surfaces.
