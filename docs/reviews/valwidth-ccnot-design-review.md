# Design review — value-width slice-4: byte-parallel `Ccnot`

**Reviewer:** fresh-context Fable (adversarial), did not write the design. **At:** `chessai/theseus-v2`
@ fc79f3b. **Method:** per the §6 mandate, **BUILT the change** in a throwaway worktree (deleted after;
repo untouched): byte-parallel `ccnot_instr` under BOTH candidate conventions (B: BR, A: s2), the
recognizer/fast-sim `&1` drops, the Flat(4) masked fold; ran the decoder differential oracle, full
`step(rom)`, the real balanced system ring, the two-body timing certificate + `validate()`, and the
ENTIRE theseus-tisc suite (release, --no-fail-fast, 2073 s). Every number measured, not derived.

**Summary verdict: SHIP after design revisions.** The crux closes: **candidate (B) — BR as the second
operand buffer — works end to end**, and so does (A); equal cost, B wins on hygiene. Bit-serial (C)
not needed. Two real design defects: §3's "drop the `&1`" is FALSE (the §4 fold needs a new
target-LAST recognizer arm — reproduced), and §7's gate times out on a green run.

## 0. THE DECISION — (B) BR closes. Adopt it.
Construction (~30 LoC replacing the bit-0 body):
```rust
let sel_rt = select_reg(R_RT);
let swap_br = at_fields(Iso::SwapProd(word8(), word8()), &[reg_field(0), BR_FIELD]);
let fetch_rt_br = Iso::seq(sel_rt.clone(), Iso::seq(swap_br, Iso::sym(sel_rt)));
let gates = (0..W).map(|i| gate_at(4*W, &[3*W, W+i, 2*W+i, i], cccnot_prim())).collect();
let core  = on_fields(&[reg_field(0), AUX_OPERAND_FIELD, BR_FIELD, AUX_PRED_FIELD], gates);
let body = Iso::seq(fetch_rt_br.clone(), Iso::seq(with_two_regs(R_RD, R_RS, core), Iso::sym(fetch_rt_br)));
predicated(&r_pred(0b1011), body)
```
= `fetch_operand` pointed at `BR_FIELD`, sequenced OUTSIDE `with_two_regs`. The nesting IS the
sequencing: r[rt]'s r0-slot transit completes + un-routes before the rs fetch / rd routing begin.
The fetch is an EXCHANGE — BR's entry value rides in r[rt]'s cell for the core (which reads BR wires
as controls only) and is written back; **no BR=0 assumption**. Verified: dense bytes with BR=0x99
entry (0xAA∧0x55→0x00, 0xFF∧0x55→0x55, 0xC3∧0xA5→0x81) whole-state equal, BR+scratch restored; full
step with BR=5 → rd^=0x81, PC+=5+1, BR preserved; pred-off brackets bit-exact identity.
**(A) s2 also closes, EQUAL cost — and the design's option-A cost is WRONG:** a swap-fetch is
content-agnostic (doesn't need s2=0, unlike ADDI's CNOT-copy into s0); it preserves s2=0xDE live junk
exactly. No allocator/MAX_LEAVES/SCRATCH_SPILL_BASE change under EITHER option — risk §8.5 evaporates.
**B over A:** BR is decoder-managed (swapbr precedent), keeps the s2..s7=USER contract crisp. **(C)
rejected — not needed.** Measured cost B vs bit-0: 624 cells/24 ancillas vs 593/10 (+31, +5.2%);
d_pre 3316→3345 (+0.9%); crossbar traversals unchanged at 6.

## 1. Reversibility / closure — VERIFIED, three levels
(1) `two_body_timing_certificate` + `validate()==Ok` on the Ccnot ROM, the MEXCH golden ROM, AND the
SWAPBR ROM (BR-user coexistence). No rail skew. (2) Balanced system ring (full fetch/decode/mem-bus/pc
netlist), 1 rev on dense bytes: correct + `ancillas_restored` + `bus_quiescent` every case (the
strongest closure evidence in the repo's vocabulary, green). (3) Value level: whole-`StateVals`
equality, no non-canonical residue at any step boundary.
**MINOR (doc):** aliasing caveat changes character — a hand-crafted rs==rt computes `rd ^= BR_entry ∧
r[rt]` (all state restored — a bijection with unspecified semantics), but `encode` already rejects it
(`CcnotNotDistinct`, asm.rs:216 — f3.5 got it right). decoder.rs:46–51 ("CCNOT's two control-bit
copies bits 5–6 … s1 residue") is now WRONG TEXT — rewrite; s1 bits 5–6 return to free.

## 2. Semantic redefinition of 0b1011 — VERIFIED SAFE empirically
`dot_reduce` GREEN unmodified; `negative_guards` 26/26 GREEN unmodified (§5's claim it needs updating
is WRONG — its 0b1011 assertion is decode-level, not semantics); xor_reduce/compile(65)/composite(19)/
valwidth*(12+7+8)/memexch(17)/fib_cpu/step(12)/state_instr(18) all green untouched. Only
`t_dense_bit0_only` flips (its declared purpose). The ONLY test-side semantic edit is `reference()`'s
0b1011 arm in tests/decoder.rs (drop `&1`) — and the existing `op_samples(Op::Ccnot)` states become
FIRING dense-byte cases under byte-parallel (closes f3.5's M-1 vacuous-oracle gap for free).
**Redefinition > new opcode 0b1100:** no Bit-domain user distinguishes them (suite-verified), Op::ALL
stays [Op;15] (no match rot, no decode change, no second energy tax for a bit-0 op nobody wants).

## 3. Prim wall + Flat(4) program — MAJOR design defect (recognizer)
**§3 is incomplete — the §4 program does NOT compile with only the `&1` drop.** `fold`'s body carrier
is `(elem, (key, acc))` — the mutated leaf (acc) is leaf **2**, but the merged recognizer arm is
target-FIRST (leaf 0). Empirical: `compile(maskedFold)` → `UnrecognizedPrim("maskedStep")`. Required
addition (verified, no ambiguity — no 3-leaf table satisfies two of {cswap, target-first, target-last}):
```rust
// ccnot TARGET-LAST (fold-body shape): (c1, c2, t) ↦ (c1, c2, t ^ (c1 & c2))
... Instr::Ccnot { rd: regs[2], rs: regs[0], rt: regs[1] }
```
With it: D=3 maskedFold → 26 instrs, exactly 2 Ccnots (Mexch-paged: acc+elem page from Mem, key rides
resident); whole-domain (lists ≤2 × key × acc over Flat(4) = **69,888 inputs**) `run_rom_fast == eval ==
independent ref acc₀ ⊕ ⊕ᵢ(key ∧ elemᵢ)`; real CPU green INCL the discriminating case `elems=[2],key=2`
(bit-0 → 0, byte-parallel → 2, so Flat(4) genuinely exercises parallel AND). **Flat(8) wall real**
(256³ ≈ 16.7M table, not constructible) but a gradient — Flat(5)³=32k / Flat(6)³=262k are legal
optional strengthenings, not required.
**T-instr adequacy:** adequate IF upgraded to the **system-ring dense-byte tier** (`system_ring(rom).run`
+ `ancillas_restored` + `bus_quiescent`, ~90s release) — genuine CPU authority (L19), not a self-written
checker.

## 4. Golden re-pins — exact measured values
Full suite: 27 targets green; exactly 3 fail, all anticipated:
| Golden | Old | New (measured) |
|---|---|---|
| ring_two_body_timing d_pre | 3316 | **3345** |
| d_post / d_mem | 289 / 35 | unchanged |
| delta1 / delta2 | 0 / 1 | **3 / 1** |
| delta_hold | 36 | **39** |
| d_locked = period | 3644 | **3676** |
| energy::me_a3 total_j | 7.2291138832e-8 | **7.3674162103e-8** (+1.91%) |
| step_j | 3.0121307847e-9 | **3.0697567543e-9** |
| B / T | 24 / 20 | unchanged |
`validate()==Ok` at the new timing values (legit re-pin). Energy tax +1.91% (vs f3.5's +9.15% — no
opcode added, only the 0b1011 body grows). fib_cpu survives. Near-ceiling allocator tests green (zero
allocator impact). **Embed these values in the task spec** so the worker can't ship "NEEDS RE-PIN".

## 5. Else
**MAJOR (gate):** §7 `timeout 1800 cargo test -p theseus-tisc` KILLS a green run — full suite = 2073s
release (debug slower; energy target alone 248s release). Use `--release` + `timeout 5400`. Allowlist
`crates/theseus-tisc/**` adequate (covers tests/energy.rs, which f3.5 missed). Doc-rot: decoder.rs
module doc (delete s1-bits-5/6 sentence; document the BR swap-fetch + value-level aliasing caveat),
asm.rs Instr::Ccnot doc (bit0→byte-parallel), tests/ccnot.rs header, status.md joules/step_j. exec_instr
`&1` drop is the complete fast-sim change; recognizer needs the target-LAST arm IN ADDITION to `&1` drop.

## SHIP / DO-NOT-SHIP-UNTIL
**DO-NOT-SHIP-UNTIL (design revision):**
1. §2 → pin convention (B) with the exact construction (fetch-rt→BR outside with_two_regs; 4-field core
   [r0-slot, s0, BR, s1]). Delete/correct the option-A cost paragraph (no scratch reservation needed);
   drop (C).
2. §3 += the target-LAST recognizer arm (`Ccnot{rd:regs[2], rs:regs[0], rt:regs[1]}`) — the §4 program
   is unbuildable without it (reproduced).
3. §5: T-instr = system-ring dense-byte tier (run + ancillas_restored + bus_quiescent); remove
   "negative_guards updated" (no change); add the s1-residue doc rewrite to the pre-existing-edit list.
4. §7 gate = clippy + `timeout 5400 cargo test -p theseus-tisc --release` (2073s floor).
5. Embed the measured re-pin tuple (d_pre 3345 / delta1 3 / delta_hold 39 / d_locked=period 3676; me_a3
   total 7.3674162103e-8, step_j 3.0697567543e-9) + the doc-rot list in the spec.

**SHIP-OK (verified by build):** convention B closes (validate()==Ok ×3 ROM classes, real-ring
ancilla/bus clean, PC+=BR+1 exact BR∈{0,5}, pred-off identity); redefining 0b1011 (dot_reduce + 26
targets green untouched, only t_dense flips); the Flat(4) masked fold (2 Ccnots, 69,888 whole-domain,
real-CPU incl the bit-0-discriminating case); zero allocator impact; encode aliasing protection in
place; cost +31 cells / +0.9% d_pre / +1.91% step_j.
