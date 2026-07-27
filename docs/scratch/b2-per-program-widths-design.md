# B-2 · Per-program widths — SCOPING DESIGN (read-only investigation)

**Status:** advisor scoping draft, produced by a **read-only** code audit of
`chessai/theseus-v2` (post-B-1 Unit-2, `7b97227`). No `.rs` touched, heavy tier
not run (a design pass needs no lowering). This is the input to a later
delegate-implemented slice sequence; the advisor reviews + commits.

**One-line thesis.** `step(rom)` is already compiled *per program* (the step
netlist is program-specific), so the remaining question is whether the **state
layout** it is compiled against can itself become a per-program parameter. Today
it cannot: `W`, `NUM_REGS`, `NUM_SCRATCH`, `MEM_WORDS`, `W_PC`, and every field
offset are **compile-time `const`s** (`types.rs`), baked into `state_ty()`, into
every gate builder in `decoder.rs`/`step.rs`, and into the marshalling. A simple
4-bit / 3-register / no-mem / 40-instr program pays for the full **184-bit**
state every step. B-2 sizes that state per program under the north-star principle:
*pay only for what you use; a wider state plumbed every step is a standing tax.*

> **The efficiency model this doc optimizes** (framing §2, energy.rs):
> `step_j = balanced_energy(step_net)` = active-cell energy + analytic K0-buffer
> energy; it is a **static cell-inventory price**, `total_j = B · step_j`
> (data-independent). So the win of any width shrink = the cells (and their
> balance buffers) it removes from the one fixed `step` netlist.

---

## 0. The current fixed layout (the baseline being shrunk)

From `types.rs` (post widen-PC, A-0 32-bit IR):

| Component            | const                    | value | bits | notes |
|----------------------|--------------------------|------:|-----:|-------|
| general registers    | `NUM_REGS`               |     8 |   64 | 8 × W |
| PC                    | `W_PC`                   |    16 |   16 | PC_lo@8, PC_hi@22 (appended) |
| BR                    | `W_BR`                   |     8 |    8 | sign-extended into PC |
| IR                    | (32-bit, 4 byte-fields)  |     4 |   32 | ISA encoding width |
| scratch              | `NUM_SCRATCH`            |     8 |   64 | 8 × W; s0/s1 decoder-reserved |
| memory               | `MEM_WORDS`              |     4 |   32 | composite carrier only |
| **CpuState**         | `NUM_CPU_FIELDS`         |    24 |  192 | `STATE_BITS = NUM_FIELDS·W` = **192** |

(The prompt's "184" is the pre-appended-PC-byte accounting; the shipped
`STATE_BITS` is `NUM_CPU_FIELDS·W = 24·8 = 192`, of which the CPU-only ring step
plumbs all 24 fields; the composite adds `MEM_WORDS` and the bus adds 3. The
exact number does not change the analysis — the point is it is *fixed* and paid
every step.)

Everything is `const` and flows into `state_ty()` (a right-nested product of
`NUM_CPU_FIELDS` `word8()` fields) and into ~**299** references across the tisc
crate. `W` in particular drives the **gate count** of every arithmetic/logic core
(the TK adder loops `0..W`, the rotator/xor/exch loops `0..W`), so `W` is not
just a state-width knob — it is a **datapath-width** knob.

---

## Q1. Which widths are per-program candidates, and what bounds each?

| Width | const today | Per-program minimum (what determines it) | Difficulty |
|-------|-------------|-------------------------------------------|-----------|
| **Register count** `NUM_REGS` | 8 (fixed) | max live register index + aux/carrier needs actually named by the compiled ROM (`asm`/`compile` already know every operand). A 3-register program needs 3 (rounded to the select-tree's next power of two — see below). | **Easy** — no proof risk; pure state-shrink. |
| **Scratch count** `NUM_SCRATCH` | 8 (fixed) | the program's spill/frame needs, **but ≥ 2 always** (s0 = operand/imm buffer + config workspace; s1 = the 8 held ALU config one-hots). A no-spill program needs exactly `{s0,s1}`. | **Easy-to-Medium** — the floor is fixed by the shared ALU (Q3). |
| **Memory** `MEM_WORDS` | 4 (fixed) | 0 for a program that never emits MEXCH/MEXCHR; else the max resident word count it addresses. **0 collapses the whole composite/bus machinery** for CPU-only programs. | **Easy** — `system_step` already has a CPU-only `step(rom)` path; a no-mem program uses `step`, not `system_step`. |
| **PC width** `W_PC` | 16 (fixed) | `⌈log₂(B + S_max)⌉` where `B = wcet`. widen-PC *already computes* the narrow-fetch `A_fetch = fetch_addr_bits(rom_len)` per program, but `W_PC` (the PC *field* count + the adder width) is still the global `const 16`. fib needs 5 effective bits; a straight-line 40-instr program needs 6. | **Medium** — the arithmetic parametricity exists (widen-PC proved it on Z/2^k); the *field count* is not yet per-program. |
| **Data word** `W` | 8 (fixed) | the widest `Flat(n)` the program's values inhabit (a 4-bit-value program → W=4). **This is the datapath-width knob** and the highest-value lever (it scales every ALU core's gate count). | **Highest bar** — see Q3; the shared-ALU config-scratch width is the crux. |
| **IR / opcode** `32-bit` | fixed by ISA encoding | The ISA encoding is 32-bit (A-0 `R: fmt(1) op(6) rd(5) rs(5) rt(5)`). A program using a small opcode subset *could* in principle use fewer opcode bits, but the operand fields (5-bit, addressing reg∪scratch) and the fetch/decode machinery are keyed to the 32-bit format. **Recommend: leave IR fixed** — the opcode-width win is small, the encoding-change risk is high, and the fetch CNOT mask already only pays `popcount(rom[p])` per entry (unset IR bits cost nothing in the mask). See Q1-note below. | **Do not pursue** in B-2. |

**Q1-note (IR / opcode).** The 32-bit IR is `4 · W` = 32 bits of state plumbed
every step, but its *cost* is not 32 bits of gates: the fetch mask only emits a
CNOT per **set** ROM bit, and the predicate/decode logic tests a fixed handful of
opcode/operand bits. Narrowing the IR would shrink the plumbed IR *state* (a
`C_state · state_width` term) but not the decode gate count, and would require a
new instruction-word format (A-0-level, highest-bar). If `W` narrows to 4, the IR
byte-fields do **not** narrow with it (opcode/operand bits are format-fixed, not
data-width). **Decision-fork DF-1 (below): couple IR fields to W, or keep IR at a
fixed byte width independent of the data word?**

**Register-count rounding.** `select_operand` is a 4-level binary select tree over
16 = reg(8)∪scratch(8) positions (`decoder.rs` `select_operand`). A per-program
`NUM_REGS = 3` still needs a select tree that addresses ≥ the max operand index;
the tree is naturally power-of-two. So the *effective* register count rounds up to
a power of two (3→4). The state-field shrink is still real (4 reg fields vs 8), and
the crossbar shrinks by a level when the reg∪scratch total drops below 8. **The
minimum is: `next_pow2(max_named_reg+1)` register fields + `max(2, spill_need)`
scratch fields**, with the operand crossbar sized to their sum.

---

## Q2. Interaction with the fixed step-iso / ring-closure / ADR-0034 proofs

**The step is ONE fixed netlist per program** (`step(rom)` in `step.rs`), so
per-program widths just mean a **per-program step-iso instantiated at that
program's widths** — which is *already the operating mode* (the netlist is
program-specific; only the *layout constants* it reads are global). The proofs are
width-**parametric** and survive per-program instantiation:

1. **ADR-0034 total-bijection (D4).** The injectivity argument is "`step = A ∘ E`
   is a composition of bijections", size-independent (D4 note: proved *without*
   reachability). The two width-naming clauses were already re-argued for arbitrary
   `k` by widen-PC:
   - `A : (PC,BR,rest) ↦ (PC+BR+1, BR, rest)` is translation on **Z/2^k** — a
     bijection for *every* k (widen-PC §4.1). Per-program `W_PC = k` is just a
     specific k; the split-field carry-correctness test (the mechanized re-proof)
     must run **at that program's k**, but the *argument* is unchanged.
   - `fetch` injectivity: the narrow-A witness `2^A_fetch ≥ B + S_max > every
     reachable PC` is *already* per-program (`fetch_addr_bits(rom_len)`), so fetch
     is the one width that is **already per-program**.

2. **The data-word bijections (the ALU cores).** The TK adder, rotator, xor, exch,
   ccnot, cswap are all built as loops `0..W`. Each is a bijection on `2^W` for
   **every W** (the TK ripple adder is `x ↦ x + op mod 2^W`; rotate is a
   permutation of W bit-positions; xor/exch/fredkin are involutions). So shrinking
   W to the program's data width preserves per-op bijectivity — **but the whole-
   domain equivalence oracle must be re-run at the narrower W** (cheaper, not
   harder: `2^4` vs `2^8` domains).

3. **Ring closure (`ring_run.rs`, two-body timing).** `CPU_BODY_N_DATA =
   STATE_BITS + NUM_BUS_FIELDS·W` and the `d_pre/d_post/period` timing goldens are
   **functions of the field count and W** (widen-PC §6 re-pinned them for the
   appended PC byte). A per-program layout means these become **per-program
   goldens** — the ring closure *proof* (the two-body split is quiescent at the
   period) is layout-parametric, but every pinned constant is layout-specific.
   **This is the single biggest tax-vs-testing tension: per-program layout means
   per-program golden re-pins, or goldens that are computed-not-pinned.**

**Where a proof/invariant assumes a FIXED width — the real findings:**

- **(F-a) The shared-ALU config one-hots need 8 bits of s1 regardless of W.**
  `s1` (`AUX_PRED_FIELD`) holds **8 held config one-hots** (`CFG_ADD_FWD..
  CFG_CSWAP`, `decoder.rs`). These are computed by `alu_config_decode` into
  `cfg_wire(k) = 32 + k` for k∈0..8 — i.e. **s1 bits 0..8**. If a 4-bit program
  shrinks W to 4, an s1 word is only 4 bits and **cannot hold 8 one-hots**. This
  is the crux (Q3). The config decode's mutual-exclusion / bijectivity argument
  assumes s1 ≥ 8 bits.
- **(F-b) The fetch AND-chain needs ≤14 scratch slots across s0+s1**
  (`step.rs` `fetch_chain`, `MAX_FETCH_A = W_PC - 1`). This budget is `2·W - 2`
  (s0 bits 0..8 + s1 bits 1..7). If W shrinks, the fetch high-zero chain budget
  shrinks — but so does `W_PC` (fewer high bits to zero-check), so this scales
  *together* and is not an independent blocker **for PC** — but it re-couples
  fetch-scratch to W. Needs the config-scratch decoupling (Q3) to stay clean.
- **(F-c) pc_add_br / pc_inc use s0 and s1 as the 16-bit operand scratch**
  (`step.rs`: op low byte in s0, high byte in s1). These read/write full `W`-bit
  scratch words. If the config-scratch (s0/s1) is *decoupled* from the data word
  W (Q3), the PC operand scratch must be sized to `W_PC`, not to the data W —
  another reason to make **config/PC scratch a fixed-width reserved region,
  independent of the data-word width.**

---

## Q3. The B-1 shared-ALU config-scratch interaction — **THE CRUX**

**The subtlety, precisely.** The shared ALU (`shared_alu_block`, `decoder.rs`)
decodes the opcode into **8 held one-hot config lines in s1** and uses **s0** as
the operand/immediate buffer *and* as the 2 transient decode-workspace bits
(prefix/chain, `CFG_PREFIX_S0`/`CFG_CHAIN_S0`). Concretely the shared datapath
*requires*:

- **s1 ≥ 8 bits** to hold the 8 config one-hots (`cfg_wire(k)=32+k`, k up to 7).
  This is exactly what B1.3 fought for — it *moved* the 2 transient bits out of s1
  into s0 to free s1 for all 8 one-hots (roadmap §Unit-2). So s1 is **saturated at
  8 bits by construction**.
- **s0 ≥ W_data bits** to hold the A-operand (a full data word), plus 2 low bits
  for the transient prefix/chain (which live *below* the operand deposit, restored
  to 0 before `imm_to_a`/`fetch_rs` writes s0).

**Therefore: if `W_data` shrinks below 8, s1 shrinks below 8, and the 8 config
one-hots no longer fit.** A naïve "shrink W everywhere" breaks the shared ALU.
This is the key finding: **config-scratch width is coupled to data-word width
today, and B-1 *maximized* that coupling (s1 is now exactly full).**

**The resolution — DECOUPLE config-scratch from data-word width.** The config
one-hots and the decode workspace are **control**, not data. They should live in a
**fixed-width reserved config region** (≥ 8 bits) that does **not** shrink with the
data word. Options, smallest-risk first:

- **(Q3-A, recommended) Reserve s0/s1 at a fixed 8-bit "control width" `W_ctl`
  even when the data word `W_data < 8`.** The register file, the ALU operand
  slots, and the arithmetic cores narrow to `W_data`; the two decoder-reserved
  scratch words s0/s1 stay `W_ctl = 8` (enough for 8 one-hots + the fetch chain +
  the W_PC operand scratch). This is clean because:
  - s0/s1 are **already special** (decoder-reserved, excluded from the operand
    crossbar's level-3 by `select_operand`, never legal operands). Giving them a
    different width from the data registers changes only their field *type*
    (`flat(W_ctl)` instead of `flat(W_data)`), which the marshalling and the
    on-fields lifts already address by field index, not by a uniform W.
  - The A-operand deposit into s0 (`imm_to_a`, `fetch_rs`) moves a **W_data**-bit
    value into a **W_ctl**-bit s0 — a zero-extend, benign (the high s0 bits stay 0,
    the ALU cores only touch bits 0..W_data).
  - **Cost:** s0/s1 stay 8-bit (16 bits of "always-8" state) while the 8 registers
    + operand datapath shrink to W_data. For a 4-bit program: registers 8·4=32
    (vs 64), s0/s1 = 16 (unchanged), a **net state shrink**, and — the real win —
    **every ALU core's gate count halves** (loops `0..4` not `0..8`).

- **(Q3-B) Keep s1 = W_data but re-pack the config to `≤ W_data` lines.** Only
  viable if the program uses ≤ W_data distinct ALU sub-functions — e.g. a program
  that only ever adds needs 1 config line. This is a **per-program config
  minimization** (decode only the op-classes the ROM actually uses). Powerful but
  it re-introduces per-program decode-netlist variability into an already-per-
  program netlist (so not *new* risk), and it interacts with mutual-exclusion
  proofs. **Defer to a later slice; Q3-A is the clean first cut.**

- **(Q3-C, reject) Shrink the config one-hots to a binary-encoded selector**
  (log₂ 8 = 3 bits) instead of 8 one-hots. This changes the ALU gating from
  "gate on one-hot bit" to "gate on a decoded 3-bit pattern" — a decode-depth
  change to the *proven* B-1 mutual-exclusion argument. High risk, defer.

**Verdict on Q3:** config-scratch width **can and should be decoupled** from data-
word width by keeping s0/s1 at a fixed `W_ctl = 8` (Q3-A). This is the design
choice that makes per-program *data-word* narrowing (B-2.1) safe. Without it,
shrinking W corrupts the shared ALU. **This is the crux the implementer must get
right, and the highest-bar sub-slice.**

---

## Q4. Win estimate (order of magnitude — needs real lower_iso to confirm)

Representative "simple" program: **4-bit values, 3 registers (→4 fields), no
memory, 40-instruction straight-line-ish ROM (B ≈ 48, so PC needs 6 bits → 1
byte)**.

Rough model (framing §2): `step_j ∝ N_rom-fetch + |ISA-decode| + state-width
plumbing`, and the **data-word W scales the ALU/datapath gate count** roughly
linearly.

| Lever | Current | This program | Rough step_j effect |
|-------|--------:|-------------:|---------------------|
| Data word `W` (ALU gate count) | 8 | 4 | ALU cores ~**half** their gates. decode_execute is the dominant block (post-B-1 `337459` balanced cells). If ~50–60% of that mass is W-scaled datapath, **≈ −25% to −30% step_j**. |
| PC width `W_PC` | 16 | 8 (1 byte) | drops PC_hi field + halves pc_add_br/pc_inc (widen-PC measured the 8→16 widen at ~+4% total, ~0.5%/bit); reversing → **≈ −4%**. |
| Registers | 8 fields | 4 fields | removes 4 word-fields from state plumbing + shrinks the crossbar by ~a level. Modest on gate count (crossbar is Fredkins), **≈ −3% to −6%**. |
| Memory | composite+bus | CPU-only `step` | no MEXCH/emit/mem/absorb, no bus factor. For a no-mem program this is already available (use `step` not `system_step`); the *per-program* win is confirming the compiler routes no-mem programs to the CPU-only ring. **Already ~free; ensures no mem tax.** |

**Order-of-magnitude combined:** a plausible **≈ 30–40% step_j** reduction for
this simple program vs the fixed 192-bit / W=8 / 16-bit-PC baseline, dominated by
the **data-word W halving** (the ALU gate count). This *stacks multiplicatively*
with B-1's −39.4% (B-1 shrank the decode block; B-2 shrinks the width that block
is compiled at). **Flag: order-of-magnitude only.** The real number requires
`lower_iso(step(rom))` at W=4 + `balanced_energy` — the same measurement B-1 was
gated on. The W-scaling fraction of decode_execute is an *estimate*; only the
lowered cell count is authoritative.

---

## Q5. Slice plan (smallest-first, risk-tagged)

The unifying refactor under all slices: **turn the layout `const`s into a per-
program `Layout` parameter** threaded through `state_ty(layout)`, the field-offset
functions, and the gate builders. This is a large mechanical change (~299
references) but **behavior-preserving at the default layout** — the green oracle
is the guard (every existing test must pass at `Layout::DEFAULT` = today's consts,
byte-identically). Recommend introducing the `Layout` struct + defaulting it
*first*, as its own no-behavior-change slice, so the risky width-shrinks land on a
threaded API rather than a const-substitution.

- **B-2.0a — `Layout` parameter, DEFAULT-only (no width change).** Replace the
  layout `const`s with a `Layout { n_regs, n_scratch, mem_words, w_pc, w_data,
  w_ctl }` threaded through `state_ty`/offsets/builders; `Layout::default()`
  reproduces today's consts. **Gate:** every existing test byte-identical (the step
  netlist fingerprint unchanged); no golden moves. *Risk:* **Medium** (broad
  mechanical change, but behavior-pinned — the diff is large but the oracle is
  exact). This is the enabling refactor; nothing shrinks yet.

- **B-2.0b — per-program `MEM_WORDS` (→0 for no-mem) + `NUM_REGS` shrink.** The
  compiler picks `mem_words = max resident addressed` (0 → CPU-only `step`) and
  `n_regs = next_pow2(max_named_reg+1)`. **No proof risk** (state-shrink only;
  bijectivity of a narrower carrier is trivial; the ALU cores are unchanged at
  W=8). **Gate:** `run_on_cpu == eval` for a no-mem 3-reg program at the shrunk
  layout; energy re-pin shows the drop. *Risk:* **Low.** The easy, safe first win.

- **B-2.1 — per-program data-word `W`, config-scratch DECOUPLED (Q3-A).** Narrow
  `w_data` to the widest `Flat(n)` the program uses; keep `w_ctl = 8` for s0/s1
  (the config region). This is the **datapath-width** slice and the **highest
  bar**: it changes the ALU core widths and the state layout, and its correctness
  rests on the Q3-A decoupling (s1 still holds 8 one-hots, s0 still holds the
  W_PC operand scratch + 2 workspace bits, the A-operand zero-extends). **Gate:**
  fresh-context adversarial review (a *different model* — datapath + config-scratch
  = daemon-equivalent control surface) + **whole-domain per-op equivalence at the
  narrow W** (the 11 reg+imm + Ccnot + Cswap cores, `2^{W}` domains) + config-
  decode mutual-exclusion at W_ctl=8 + `run_on_cpu == eval` on a real 4-bit program
  across a real worktree + ring closure + goldens re-pinned. **A regression test
  that reproduces the Q3 config-scratch collision** (a program where W_data<8 and
  s1 would overflow without the decoupling — fails without w_ctl, passes with it).
  *Risk:* **HIGHEST BAR.**

- **B-2.2 — per-program PC width (wire the widen-PC parametricity).** Make the PC
  *field count* + adder width per-program: `w_pc = 8·⌈log₂(B+S_max)/8⌉` (fib → 1
  byte). widen-PC already made the arithmetic parametric on Z/2^k and computed
  `A_fetch` per program; this slice makes the **field layout** (drop PC_hi when
  1 byte suffices) and the adder width per-program. **Gate:** the split-field
  carry-correctness exhaustive test **at the program's k** (mechanized ADR-0034 A
  re-proof, already the widen-PC gate) + `run_on_cpu == eval` + a B>256 program
  still runs at its wider W_PC + energy re-pin. *Risk:* **HIGH** (state layout +
  the ADR-0034 clause, but the argument is already proved for all k — this is
  wiring, not new proof). Independent of B-2.1; can land in either order.

**Recommended order:** B-2.0a → B-2.0b → (B-2.1 ∥ B-2.2). B-2.0b delivers a safe
first win and exercises the `Layout` API; B-2.1 (data-word, the big win) and B-2.2
(PC) are independent and both build on B-2.0a. **Do not bundle** (one-task-per-
delegation lesson); B-2.1 is the one that needs the full highest-bar discipline.

---

## Q6. Risks / open questions / decision-forks (need user input — user is OFFLINE)

**DF-1 (IR/opcode vs data-word coupling).** When `w_data` narrows to 4, do the IR
byte-fields narrow too? **Recommendation: NO** — keep IR fields at a fixed byte
width (opcode/operand bits are format-fixed, not data). Narrowing IR needs a new
instruction-word format (A-0-level). *User: confirm IR stays fixed in B-2 (opcode
narrowing is out of scope).*

**DF-2 (config-scratch decoupling — Q3-A vs Q3-B).** Recommendation: **Q3-A**
(fixed `w_ctl = 8` for s0/s1, decoupled from the data word) as the clean first cut;
defer per-program config-minimization (Q3-B). *User: is a fixed 8-bit control
region acceptable standing state for a narrow-data program, or do you want the
more aggressive Q3-B config-minimization (a bigger win but a decode-netlist-
variability + mutual-exclusion-proof cost)?* This is the central efficiency-vs-
complexity trade in B-2.

**DF-3 (register-count power-of-two rounding).** The operand select tree is
binary, so `n_regs` rounds up to a power of two (3→4). Accept the rounding
(simple, small waste), or invest in a non-power-of-two crossbar (more complex,
marginal)? **Recommendation: accept rounding.** *User: confirm.*

**DF-4 (per-program goldens vs computed goldens).** Per-program layout means the
ring-timing / energy goldens (`d_pre`, `period`, `CPU_BODY_N_DATA`, `step_j`)
become **per-program**. Either (a) pin a golden per representative program, or
(b) make the tests assert *computed* relations (`total == B·step_j`,
`period == d_pre+d_mem+d_post`) rather than absolute pins. **Recommendation: (b)
for the relations + a small pinned set for a few canonical layouts.** *User:
confirm the testing philosophy (relations vs pins) — this affects how much golden
churn every future program causes.*

**DF-5 (`Layout` as a public API surface).** Threading a `Layout` through
`state_ty` etc. changes public signatures (`state_ty()` → `state_ty(&layout)` or a
thread-local/context). Is a breaking API change acceptable, or should `Layout` be
carried implicitly (e.g. in the compile context)? *User: API preference.* This is
the widen-PC "append vs insert" style decision-fork for B-2.

**Risks (no user decision, flagged for the reviewer):**
- **R-1 (the 299-reference mechanical change).** B-2.0a touches nearly every
  layout consumer. The risk is a *missed* site that keeps a hardcoded 8/16 and
  desyncs the fast-sim from the hardware (exactly the widen-PC BLOCKER pattern —
  "unwidened bveq_netlist state builders"). Mitigation: `Layout::default()` must
  be **byte-identical** to today (netlist fingerprint test), and grep every literal
  `8`/`16`/`24`/`192` in the tisc crate before shrinking anything.
- **R-2 (fetch-chain re-coupling, F-b).** The fetch high-zero AND-chain budget is
  `2·W_ctl - 2` slots; it must be sized against `w_ctl` (the config region), not
  `w_data`. If an implementer sizes it against the shrunk data word, a narrow-data
  program with a wide-ish PC could overflow the chain (silent fetch aliasing — the
  widen-PC F-2 finding). Mitigation: the fetch chain lives in the **w_ctl** region;
  Q3-A keeps it 8-bit.
- **R-3 (lower_iso memory at narrow W is *smaller*, good).** Unlike widen-PC (which
  grew the state and OOM'd), B-2 *shrinks* the state, so the `lower_iso`/`me_a3`
  memory pressure **improves** — narrow-W measurement should be cheaper, not
  harder. No new OOM risk (the lean-`lower_iso` `Arc<Type>` fix already landed).

---

## Appendix — grounding (files read, read-only)

- `docs/src/tisc2-roadmap.md` — B-2 entry (§Phase B), the north-star principle,
  B-1 completion (step_j −39.4%, decode 337459 cells).
- `docs/scratch/arbitrary-theseus-cpu-design-framing.md` §2 — the energy model
  (`step_j = C_fetch·N_rom + C_decode·|ISA| + C_state·state_width + C_mem`), the
  efficiency principle ("pay only for live state").
- `crates/theseus-tisc/src/types.rs` — `W`, `W_PC`, `W_BR`, `NUM_REGS`,
  `NUM_SCRATCH`, `MEM_WORDS`, `NUM_CPU_FIELDS`, `STATE_BITS`, `state_ty()`, field
  offsets (all `const`; `state_ty` = right-nested `word8()` product).
- `crates/theseus-tisc/src/step.rs` — `step(rom)`/`system_step`/`cpu_pre`/
  `cpu_post`; `pc_add_br`/`pc_inc` (s0/s1 as W_PC operand scratch); `fetch` +
  `fetch_addr_bits` (narrow-A, already per-program) + `MAX_FETCH_A`/`fetch_chain`.
- `crates/theseus-tisc/src/decoder.rs` — `shared_alu_block`, `alu_config_decode`
  (8 held one-hots in s1, `cfg_wire(k)=32+k`; transient prefix/chain in s0),
  `select_operand` (4-level tree over reg∪scratch, s0/s1 excluded), every ALU core
  looping `0..W` (the data-word gate-count coupling).
- `crates/theseus-core/src/ty.rs` — `Type::Flat(u8)` (1..=8), the per-program data
  width source; `Arc<Type>` structural sharing (lean-lower_iso).
- `crates/theseus-tisc/src/energy.rs` — `balanced_energy`/`step_energy`/
  `analytic_buffer_count` (static cell inventory → `total_j = B·step_j`).
- `docs/scratch/a6.4-widen-pc-design.md` — the precedent for making a width (PC)
  parametric: decouple axes, re-prove on Z/2^k, narrow-A fetch, golden re-pin
  discipline, POC-first + fresh-model adversarial review.
