# A-5b Option A — aux-in-scratch for wide/deep loop carriers — scoping

**Status:** READ-ONLY design investigation (no `.rs` edited, heavy tier not run).
**Base:** `chessai/theseus-v2` @ `8206c60` (Option B = aux-REUSE via liveness is
MERGED; per-program widths B-2.0a/0b merged).
All `file:line` are against this commit's `crates/theseus-tisc/src/…`.

**One-line finding:** the datapath is *already* scratch-SWAPBR-capable on the real
ring path — the only source-level blocker is one over-conservative encoder guard
(`asm.rs:382-387`). Option A is a genuinely clean, small, bounded slice, BUT it is
**low value today**: Option B already gives arbitrary *aux* depth for free, so the
only shapes Option A newly unlocks are **wide carriers** (many live input leaves)
and **deep-behind-Plus** nesting that exhaust the 8-register file. Recommend
**building the encoder-relaxation + authority test now** (small, unblocks a real
capability and discharges roadmap open-question #1), and **deferring the
`lower_trace` aux-in-scratch mechanism** until a concrete program needs it.

---

## Q1 (the crux) — is the datapath already scratch-SWAPBR-capable?

**Yes. There is no datapath gap; the block is purely the source-level encoder guard.**

The SWAPBR datapath iso routes its `rd` through the *full* reg∪scratch A-0
crossbar, on BOTH the per-op path and the real shared-ring path:

- `swapbr_instr()` (`decoder.rs:1004-1007`) = `predicated(r_pred(0b1000),
  with_target(R_RD, swapbr_core()))`.
- `with_target(sel, core)` (`decoder.rs:663-666`) = `select_operand(sel,
  operand_field)` ∘ core ∘ `sym(...)` — the **16-way reg∪scratch crossbar**, not
  the low-8 `select_reg`. Comment at 1005: *"A-0 (BLOCKER-2 fix): route rd over
  reg∪scratch."*
- `select_operand` (`decoder.rs:372-395`) routes any legal scratch operand
  s2..s7 (idx 10..15) to the fixed r0-slot via the level-3 swaps (i∈2..8), and
  **structurally excludes** s0/s1 (idx 8/9) — the level-3 loop starts at `i=2`, so
  `base(0)↔base(8)` / `base(1)↔base(9)` never happen; the reserved operand-buffer
  (s0) + predicate word (s1) are untouched (`decoder.rs:359-371`).
- `swapbr_core()` (`decoder.rs:995-1002`) operates on `[reg_field(0), BR_FIELD,
  AUX_PRED_FIELD]` — i.e. on the r0-slot **after** `with_target` has routed the
  operand there. So a scratch rd is swapped ↔ BR exactly as a register rd is.

**Real-ring path confirmed:** `decode_execute()` → `decode_execute_shared()`
(`decoder.rs:1706-1712`) invokes `swapbr_instr()` as its **own separate block**
(the comment at 1704-1705 keeps SWAPBR out of the universal shared bracket because
"rs=0 aliases rd"). That separate block is the same full-crossbar `swapbr_instr()`
— so scratch-SWAPBR routes correctly on the real `system_step`/ring, not just in
the per-op replicated form.

**Come-from token semantics under a scratch home (no aliasing hazard).** The
come-from token never *lives* in BR: SWAPBR swaps the operand word ↔ BR **at the
branch point only**; BR is then consumed and zeroed by the end-of-step
`pc_add_br ; pc_inc`. The PC update reads BR alone — the token's home (reg or
scratch) is irrelevant to the PC arithmetic. A scratch come-from token round-trips
into BR and back into the scratch word identically to a register token.

**No BR-vs-scratch alias.** BR is `BR_FIELD` (a dedicated CpuState field, not part
of the reg∪scratch operand space); s2..s7 are `scratch_field(k)`. They are
disjoint fields, and `select_operand` provably never touches s0/s1. So there is no
datapath aliasing between the SWAPBR-rd scratch cell and BR or the reserved
scratch words.

**Caveat (the honest gap):** although the *iso* routes correctly, per the roadmap
review (`6b54d9d`) **no real-system_step test has ever executed a scratch-operand
instruction (idx 10..15) of ANY opcode.** The crossbar's scratch path is exercised
by construction/unit isos but not by an end-to-end ring run. So Q1's answer is
"datapath capable by construction, unproven on real authority" — the POC's job is
to close exactly that gap.

---

## Q2 — the encoder-relaxation spec

The blocker is a SWAPBR-specific guard **stricter than the general operand
validator**:

```rust
// asm.rs:382-387
Instr::Swapbr { rd } => {
    if (rd as usize) >= NUM_REGS {            // NUM_REGS = 8
        return Err(AsmError::SwapbrRegOutOfRange { rd });
    }
    r_word(0b1000, rd, 0, 0)
}
```

Every other reg∪scratch op (`Add/Xor/Cswap/Ccnot/Addi/…`) reaches
`check_operand` via `r_word`/`i_word`, which **accepts** s2..s7 (idx 10..15) and
rejects only s0/s1 (8/9, `OperandReservedScratch`) and ≥16 (`RegOutOfRange`)
(`asm.rs:285-294`). SWAPBR alone is gated to `rd < 8` — a conservative gate, not a
hardware limit (the 5-bit rd field in `r_word` fits idx 0..16).

**Relaxation:** drop the special guard and let `r_word` do the legality check:

```rust
Instr::Swapbr { rd } => r_word(0b1000, rd, 0, 0),
```

- **Legal scratch indices:** s2..s7 = operand idx **10..15** (matches the crossbar
  and every other op).
- **Still rejected:** s0/s1 (idx 8/9) via `check_operand`'s
  `OperandReservedScratch`; idx ≥16 via `RegOutOfRange`.
- **Encode/decode round-trip:** `r_word(0b1000, rd, 0, 0)` sets fmt+opcode+rd; the
  rs/rt fields stay 0 (SWAPBR reads BR, not rs/rt). `decode` masks the opcode to
  the datapath's tested bits (`asm.rs:408-427`) and reconstructs `Swapbr { rd }`
  from the rd field — a scratch rd round-trips exactly like a register rd (the
  decode path already accepts idx 0..16 in the rd field for all other R-ops).

**Test consequence (repin required):** the negative test
`negative_guards.rs:353-359` pins `encode(Swapbr { rd: 9 }) ==
Err(SwapbrRegOutOfRange { rd: 9 })`. After the relaxation, rd:9 is rejected by
`check_operand` as `OperandReservedScratch { which: "rd", val: 9 }` instead — the
test must be re-pinned to the new (still-rejecting) variant. Add positive coverage:
`encode(Swapbr { rd: 10 })` (s2) now `Ok`, and a decode round-trip
`decode(encode(Swapbr{rd:10})) == Some(Swapbr{rd:10})`. Also add a negative for
`rd: 8` (s0) and `rd: 16` (out of range).

This mirrors the legality rules A-0 established for every other operand — after
the change SWAPBR is uniform with the rest of the ISA.

---

## Q3 — the `lower_trace` aux-in-scratch mechanism

Today `lower_trace` draws the four aux `{rc, rb, temp, ctrl}`
**register-only** (`allocate_aux_frame`, `compile.rs:821-860`): candidates =
`free(occ) ∪ reusable_aux` over `0..NUM_REGS`; `< 4 available` ⇒
`Deferred` (graceful reject). The docstring (809-812) states the reason aux are
register-only: *"all four are live SWAPBR/CSWAP/ADDI operands across the whole loop
bracket, so they must be register-resident."* This is exactly the constraint
Option A relaxes for the SWAPBR pair.

The two register-exhaustion reject sites Option A would relieve:

1. **`allocate_aux_frame` `< 4 candidates`** (`compile.rs:842-849`) — the aux
   frame can't find 4 register slots.
2. **carrier guard `n_input + 1 > carrier_reg_budget`** where `carrier_reg_budget
   = NUM_REGS - 4 = 4` (`compile.rs:3488-3498`) — tag + live-input leaves can't fit
   in the 4 registers left after reserving the aux frame. (NB: this is a *carrier*
   bound, not an aux bound — Option A that only spills aux to scratch does NOT lift
   this; see Q5.)

**The mechanism (aux frame partly in scratch).** The four aux split by op-usage:

| aux | ops it appears in | scratch-legal TODAY? |
|---|---|---|
| `temp` | `ADDI/SUBI temp`, `CSWAP …,temp,…` (rs/rt) | **yes** — Addi/Subi/Cswap encode scratch operands already |
| `ctrl` | `XOR/XORI ctrl`, `CSWAP ctrl,…` (rd control) | **yes** |
| `rc` | `SWAPBR rc` (rd), `CSWAP ctrl,rc,temp` (rs) | SWAPBR blocks it → **needs Q2** |
| `rb` | `SWAPBR rb` (rd), `CSWAP ctrl,temp,rb` (rt) | SWAPBR blocks it → **needs Q2** |

So `temp`/`ctrl` can spill to scratch **today** (encoder already accepts them);
only `rc`/`rb` are gated by the SWAPBR guard. After Q2 all four may live in
s2..s7.

The `lower_trace` change is: when the register file can't host a full 4-register
frame, let `allocate_aux_frame` fall back to drawing from **free scratch slots
s2..s7** (via a `Location::Scratch` home, occ-aware, exactly like the existing
padding spill at `compile.rs:3586-3602`). Concretely the frame becomes a
`[Location; 4]` (reg or scratch per slot) rather than `[u8; 4]` register indices,
and the skeleton emitter (`compile.rs:3867-3934`) names each aux by its resolved
crossbar operand index (reg idx, or scratch idx+8).

**Same net-zero page-in/page-out bracket as the register case.** The invariant is
unchanged: each aux is **created-zero at loop entry and consumed-to-zero at exit**
(the Bennett discipline the frame already satisfies; net-zero verifier
`compile.rs:1955-1988`). A scratch home doesn't change this — the same ADDI/SUBI
mint/burn and CSWAP route/unroute run against the scratch word instead of a
register, and marshal already zeroes scratch (so a scratch aux starts at 0, like a
scratch padding leaf). The come-from token (`rc`/`rb`) is *transient in the aux
word* (minted from the compile-time `δu` immediate, swapped ↔ BR, consumed) — it is
NOT carried across the body in the aux (Option-B's §2.2 liveness), so a scratch
home is just a different resting place for a per-iteration ancilla.

**Invariant that must hold:** (i) aux created-zero / consumed-zero at every loop
boundary (net-zero, unchanged); (ii) no aux may land on s0/s1 (the crossbar +
encoder already forbid it — the scratch spill iterator must start at
`SCRATCH_SPILL_BASE = 2`, `compile.rs:416`); (iii) the come-from token round-trips
BR↔scratch correctly (Q1); (iv) the aux frame stays **disjoint** from carrier
leaves, `Times` siblings, and any outer-Trace aux (the occ-marking discipline
already threads this — scratch slots just join the occ pool).

**B-2.0b / scratch-count interaction (checked — no collision):**
- The shared-ALU reserved words are **s0 (operand buffer) + s1 (config one-hots)**
  = idx 8/9; legal aux-in-scratch is **s2..s7** = idx 10..15. The come-from/SWAPBR
  path provably never touches s0/s1 (`select_operand` level-3 excludes them,
  Q1) and the encoder rejects them. **No B-1 collision.**
- `derive_layout` (`compile.rs:3023-3034`) computes `max_named_reg` from
  `operand_locs`, counting only `l < NUM_REGS` toward `num_regs` — a scratch aux
  (idx ≥ 8) is correctly NOT counted as a register. `Swapbr`'s rd is already in
  `operand_locs` (`asm.rs:264`), so a scratch SWAPBR rd is already visible to any
  future scratch-count derivation.
- **`num_scratch` is NOT yet per-program narrowed:** `for_program_facts`
  (`types.rs:97-121`) keeps `num_scratch` at the default **8** for every program
  (only `num_regs` and `mem_words` narrow). So aux-in-scratch does **not** change a
  program's scratch-count needs today — all 8 scratch words already exist. (If a
  future B-2.x narrows `num_scratch` by `max_named_scratch`, aux-in-scratch would
  raise a program's scratch high-water mark, and `operand_locs` already exposes the
  SWAPBR scratch rd for that derivation — so it's forward-compatible.)

---

## Q4 — risk / highest-bar failure modes

`lower_trace` is the L15 top-bar code (a bug corrupts every loop it lowers) and
this touches the encoder + real SWAPBR semantics. A POC + fresh-context adversarial
review must check:

1. **Scratch-SWAPBR come-from correctness on real `system_step`** (not just
   `run_on_cpu_fast` / the unit iso). The single most important gap: NO ring run
   has ever executed a scratch-operand instr. A `SWAPBR s_k` must be shown to
   swap the scratch word ↔ BR and produce the correct back-edge PC on the real
   ring, over ≥2 iterations. This is the crux — everything else is downstream.
2. **Aux-in-scratch net-zero.** The four aux (now some in s2..s7) must be 0 at
   every loop-body boundary — assert via the net-zero verifier / a state dump. A
   scratch aux left dirty (mint without matching burn) corrupts the terminal
   quiescence check and any later user of that scratch word.
3. **No s0/s1 collision.** Prove no aux ever lands on s0/s1: the spill iterator
   starts at `SCRATCH_SPILL_BASE=2`; the encoder rejects idx 8/9; the crossbar
   excludes them. A regression: attempt to force an aux onto s0/s1 → must reject,
   not corrupt.
4. **Graceful-reject still fires when even scratch is exhausted.** With aux
   allowed in s2..s7, the fail-closed reject must still trigger when free regs +
   free s2..s7 together can't host the frame (fail-closed, never alias). Pin a
   reject test for the scratch-exhausted case.
5. **Byte-identity for currently-compiling programs.** Aux-in-scratch must be a
   fallback that fires ONLY on register exhaustion — every program that compiles
   today (register-resident aux) must produce a byte-identical ROM. The
   `allocate_aux_frame` byte-identity assert (`compile.rs:814-820`) must still
   hold; add a ROM-compare regression.
6. **CSWAP with two scratch operands.** In a fully-spilled frame the join-dispatch
   `CSWAP ctrl, rc, temp` and route `CSWAP ctrl, temp, rb` could name up to three
   scratch operands. The 3-operand crossbar routes each independently, but the
   `CswapNotDistinct` check (`asm.rs:361-365`) and the datapath's simultaneous
   routing of three scratch operands to fixed slots is an **unexercised
   combination** — the POC must include a CSWAP naming ≥2 distinct scratch
   operands on the real ring.
7. **Aliasing under nesting/Times.** A scratch aux frame must stay disjoint from a
   `Times` sibling's live data (which never lives in scratch today, but confirm)
   and from an outer Trace's aux — the occ pool must include scratch slots so the
   disjointness discipline extends there.

**Adversarial-review target:** *"construct a program where a scratch aux is NOT
net-zero at a body boundary, or where a scratch-SWAPBR back-edge computes the wrong
PC, or where a CSWAP naming two scratch operands corrupts a third."*

---

## Q5 — value / scope assessment

**How niche is the unlocked coverage?** Narrow. Option B already made *aux* depth
O(1) (arbitrary nesting depth fits, bounded only by carrier registers). So Option A
does **not** unlock "deeper nesting" in the common case — B already did. What
Option A newly compiles:

- **Wide carriers.** A single-level Trace whose carrier is `tag + n_input` live
  leaves with `n_input + 1 > 4` (`carrier_reg_budget`). E.g. a Trace over a
  4-leaf-input body: `1 tag + 4 input = 5 > 4` → rejects today. **BUT** —
  crucial caveat — Option A as scoped (spill *aux* to scratch) does NOT lift the
  carrier guard `n_input + 1 > carrier_reg_budget` (`compile.rs:3492`), because
  that guard protects *live input* leaves, which must stay register-resident (no
  live-input relocation — the M-D.e F5.2 barrier). To actually widen the carrier
  ceiling you must spill *aux* to scratch AND raise `carrier_reg_budget` toward
  `NUM_REGS - 0` (aux fully in scratch) — which is a bigger change and re-opens
  the live-input-relocation question the roadmap explicitly deferred.
- **Deep-behind-Plus.** `Plus` tag-wrappers cost 2 non-reusable aux per level
  (per the memory note), so very deep nesting *behind Plus* can still exhaust
  registers even with B's reuse. Aux-in-scratch relieves that — but this is a rare
  shape.

So the honestly-unlocked set with a *minimal* Option A (aux→scratch, carrier guard
unchanged) is: **nested/deep-behind-Plus loops whose AUX pressure (not carrier
pressure) exceeds 8 registers even after B's reuse.** That is a genuinely niche
shape; no current or near-term program is known to need it.

**POC size / can-of-worms assessment.** Two cleanly separable pieces:

- **Piece 1 — encoder relaxation + scratch-SWAPBR authority (SMALL, RECOMMENDED
  NOW).** ~5 lines in `asm.rs` + test repins + a real-ring POC that hand-assembles
  a tiny loop using `SWAPBR s2` (aux placed in scratch by hand, not by
  `lower_trace`) and shows `run_on_cpu == eval` + `eval^R` + terminal quiescent
  over ≥2 iterations. This discharges roadmap open-question #1 (is the guard purely
  conservative — **yes**) and the "no scratch-operand instr ever ran on real
  authority" gap, and is a clean bounded slice. **This is the smallest useful
  POC.**
- **Piece 2 — `lower_trace` aux-in-scratch fallback (MEDIUM, DEFER).** Changing
  `allocate_aux_frame` to a `[Location;4]` frame + the emitter + the reject sites
  touches the L15 top-bar. It is *bounded* (the net-zero discipline is unchanged;
  it mirrors the existing padding spill) but it is **speculative** — it unlocks a
  shape no known program needs, and the *carrier*-widening version drags in
  live-input relocation (a separate, larger review). **Not a can of worms if
  scoped to aux-only, but low ROI.**

---

## Recommendation

**Build Piece 1 now, defer Piece 2.**

1. **Now:** relax `SwapbrRegOutOfRange` → `r_word`/`check_operand` (Q2), repin the
   negative test, add positive encode/decode round-trips for s2..s7, and ship a
   **real-`system_step` scratch-SWAPBR authority POC** (hand-assembled tiny loop
   with a scratch come-from word, ≥2 iterations, `run_on_cpu==eval` + `eval^R` +
   BR/s0/s1 terminal quiescent). This is small, closes the "no scratch-operand
   instr ever ran on authority" gap, discharges roadmap open-question #1, and makes
   the whole ISA uniform on operand legality. It also unblocks Piece 2 for whenever
   a real program needs it.

2. **Defer:** the `lower_trace` aux-in-scratch frame fallback (Piece 2). Revisit
   **only when a concrete well-formed program rejects for aux (not carrier)
   pressure after Option B's reuse.** When it lands it needs the full L15/L20/L21
   bar: fresh-context adversarial review, real-ring depth POC with scratch aux
   genuinely torn down/rebuilt inside a live outer region, byte-identity for all
   currently-compiling programs, and a fail-closed reject test for
   scratch-exhaustion.

The carrier-widening variant (raise `carrier_reg_budget`) is a **separate, larger**
slice that re-opens live-input relocation — do not fold it into Option A.
