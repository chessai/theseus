# f3.5 — masked-XOR reduce (`acc ⊕= key ∧ arr[i]`) via a new `Ccnot` ISA instruction

**Milestone.** Successor to the M-D.b2 map+reduce pair (f1 emission / f2 map / f3 XOR
reduce). f3 reduced with a *linear* step (`acc ⊕= arr[i]`, a CNOT); f3.5 reduces with a
*non-linear* step (`acc ⊕= key ∧ arr[i]`, a **Toffoli**). It is the **first milestone
that adds a user-facing ISA instruction and a `system_step` decoder arm** — it touches
the reversible CPU authority, not just codegen. Full adversarial-review +
reproducing-test bar (CLAUDE.md; L19/L20/L21).

> **Revision r2** — incorporates the fresh-context Fable design review
> (`docs/reviews/f35-design-review.md`). Route **β (new instruction)** chosen over
> route α (recognizer-only carry-trick) — see §0. Decoder arm is **bit-0 semantics**
> (F35-1a), carrier/recognizer are **target-first** (F35-6), allowlist fixed (F35-9),
> dense-byte semantics test added (F35-10).

## 0. Route choice: new instruction vs. the 7-instruction carry trick

The reduce step `(acc, key, data) ↦ (acc ⊕ (key ∧ data), …)` is a Toffoli. It is **not
a necessity** to add an instruction — the review (F35-10) correctly refuted the earlier
"not synthesizable from linear ops" claim. Two viable routes:

- **α (recognizer-only):** emit the 7-instruction, zero-ancilla, existing-ISA sequence
  `Rotli acc,1; Xor acc,key; Xor acc,data; Add data,key; Xor acc,data; Sub data,key;
  Rotri acc,1` (the carry bit of `key+data` *is* `key∧data` on 0/1 bytes). No ISA /
  decoder / closure / golden changes. Cost: 7 instrs/element, bit-domain only, ISA
  unchanged.
- **β (new `Ccnot` instruction) — CHOSEN.** 1 instr/element. Strategic: the **first
  user-facing CPU-authority instruction**, and the primitive that a byte-wide masked
  accumulate (M-D.c value-width) will reuse directly. Higher bar: a `system_step`
  decoder arm → closure re-validation + 3 golden/test re-pins + full adversarial
  impl-review. The reversible decoder already contains the Toffoli gate
  (`toffoli_prim`/`cccnot_prim`, decoder.rs:114/123) used internally for ADD's carry;
  β exposes it as an instruction.

Trade-off accepted deliberately: we pay the CPU-authority-change cost now to bank a
reusable ISA primitive and clean 1-instr energy, rather than a domain-restricted trick.

## 1. The instruction: `Ccnot { rd, rs, rt }` — **bit-0 semantics**

- **Semantics:** `r[rd].bit0 ^= r[rs].bit0 & r[rt].bit0` (all other bits of `r[rd]`
  unchanged). This matches `Cswap`'s existing **bit-0 control convention** and is what
  makes the decoder arm pure existing machinery (§2, F35-1a). For the Bit-valued reduce
  this is exactly `acc ⊕= key ∧ data`. Byte-wide AND is explicitly **out of scope** for
  f3.5 (it needs a new decoder buffer convention — F35-1b; deferred to M-D.c).
- **Self-inverse** for pairwise-distinct `rd,rs,rt` (the same bit-0 mask XORed twice
  cancels; `rs,rt` are read-only). *Not* self-inverse under aliasing — hence:
- **Operand constraint:** pairwise distinct `rd, rs, rt` (mirrors `Cswap`). `rs==rt`
  would be a plain `Xor` (caught by the 2-leaf Xor arm, never reaches Ccnot); `rd==rs`/
  `rd==rt` are non-injective and can never be a PrimDef (F35-8).
- **Encoding:** R-format, **opcode `0b1011`** (first free slot; `0b1011`–`0b1111` are
  `_ => None` today). Fields `rd`(8–10) `rs`(5–7) `rt`(2–4), exactly like `Cswap`:
  `r_word(0b1011, rd, rs, rt)`.

### asm.rs changes
1. `Instr::Ccnot { rd, rs, rt }` (doc: `r[rd].bit0 ^= r[rs].bit0 & r[rt].bit0`,
   pairwise distinct).
2. `encode`: distinctness check → new `AsmError::CcnotNotDistinct { rd, rs, rt }`
   (mirror `CswapNotDistinct`), then `r_word(0b1011, rd, rs, rt)`.
3. `decode`: `0b1011 => Some(Instr::Ccnot { rd, rs, rt })`.
4. Header doc: "the 14 architected instructions" is already stale (16); update to 17.

## 2. `system_step` decoder arm (the reversibility-critical change) — via `cswap_instr()`

Build `ccnot_instr()` as the **exact structural sibling of `cswap_instr()`** (a 3-reg,
bit-0-predicated op). The decoder colocates the target in the r0 slot via one crossbar
(`with_target`, decoder.rs:468) and swap-fetches at most one full-byte operand into s0
(`fetch_operand`, decoder.rs:458). `Ccnot` needs only **one bit** of each control (like
`cswap` needs one bit of rd), so both controls fit in free s1 bits:

- Copy `r[rs].bit0` → s1 bit 5 and `r[rt].bit0` → s1 bit 6 via two `with_target`-style
  CNOT copy brackets (s1 bits 5–6 are free — s1 layout: 0=pred, 1–3=chain, 4=cswap
  copy, **5–6 free**, so no collision with the pred chain or the cswap control wire).
- One predicate-gated `cccnot_prim` (a `Tk::T` predicated Toffoli — the same shape gated
  ADD already emits) writing `r[rd].bit0`: `(pred ∧ s1.5 ∧ s1.6) → r0slot.bit0`.
- Uncompute the two copy brackets (self-cancelling). Predicate bracket unchanged.

### decoder.rs changes
1. `Op::Ccnot` enum variant; add to `Op::ALL` (which becomes `[Op; 15]`).
2. Dispatch: `Op::Ccnot => ccnot_instr()` in **`instruction()`** (decoder.rs:784 — NOT
   `op_gates`, which does not exist).
3. `fn ccnot_instr() -> Iso`: the two bit-0 copy brackets + gated `cccnot_prim` above,
   structured like `cswap_instr()`. **Do not** use `cross_toffoli` (that is the
   cross-factor (Cpu,Bus) helper for the MEXCHR block — wrong tool).
4. Module doc sweep: decoder.rs:30 "Reserved opcodes R 1011–1111" → 1100–1111; Op doc
   "16 live"/"14 CpuState-only" → 17/15.

### The obligation this creates (adversarial-review target #1)
Adding `Op::Ccnot` to `Op::ALL` changes the `system_step` iso ⇒ **the whole-step
reversibility closure certificate must re-validate** (`validate() == Ok`; ring closure /
round-trip tests still pass). The reviewer/worker must confirm: `ccnot_instr()` is
self-inverse and touches **only** [rd,rs,rt] under its predicate; opcode `0b1011` is
exclusive with every other opcode (was `None`; nothing else claims it — so exactly one
op-core fires per fetch, preserving the disjoint-predicated-sum structure); the s1 bits
5–6 chosen do not collide with `CSWAP_CTRL_WIRE_IN_S1` (bit 4) or the pred chain (bits
1–3), and each bracket restores s1 to 0.

## 3. Recognizer arm (compile.rs `recognize_prim`, `n_in == 3` block ~L943) — **target-first**

After the `cswap` arm, add a **target-first** pattern (chosen over target-last to save
Exch plumbing — F35-6):
```rust
// ccnot (target-first): (t, c1, c2) ↦ (t ⊕ (c1 ∧ c2), c1, c2)
if check_pattern_3reg(&def.forward, &vals, |t, c1, c2| (t ^ (c1 & c2), c1, c2)) {
    return with_registers(ctx, &leaves, |regs| {
        // rd = target (leaf 0); rs, rt = controls (leaves 1, 2).
        Ok(vec![Instr::Ccnot { rd: regs[0], rs: regs[1], rt: regs[2] }])
    });
}
```
- **Leaf-order contract (target FIRST):** prim leaves ordered `(acc, key, data)` with
  the **target leaf 0**. `with_registers` gives `regs[i] = home_of(leaf i)`, so
  `Ccnot.rd = regs[0]` writes the acc home directly. Because the reduce carrier already
  presents `(acc, key, data)` as its first three leaves (§5), the reduceStep applies
  **with no Exch dance** — so unlike f3's F3-1 trap, `regs[0]` at the prim point *is*
  the acc home (no swapped-register subtlety). This is the payoff of target-first.
- **No cswap collision:** cswap is `(c,a,b)↦(c, cond-swap)`; ccnot xors leaf 0. No table
  satisfies both (c=1,a=0,b=1: cswap→(1,1,0), ccnot-target-first with (t,c1,c2)=(1,0,1)
  →(1⊕0,0,1)=(1,0,1) vs cswap dispatch differs). `check_pattern_3reg` verifies the
  **whole** forward table, so arm order is immaterial; place adjacent to cswap.
- **Shape pin (F35-5):** `check_pattern_3reg` requires input type **exactly**
  `Prod(B, Prod(B, B))` — a trailing `One` breaks the inner `Value::Pair` destructure →
  `UnrecognizedPrim`. The reduceStep3 prim (§5) must have exactly this 3-leaf type.

## 4. Fast-sim arm (compile.rs `exec_instr`) — bit-0, matching §1

`Instr::Ccnot { rd, rs, rt } => { state.regs[rd] ^= (state.regs[rs] & state.regs[rt]) & 1; }`

Bit-0 only (must agree with §1 and the decoder arm §2, or a future Word8 program
diverges silently — F35-10). The fast sim is the L19 cross-check oracle; it must match
both `eval` and the real `system_step`.

## 5. The demonstrating program: masked-XOR reduce

Carrier: `Nat_d × (acc, key, data, idx, arr0, arr1)` — **acc, key, data first** so the
target-first reduceStep applies with no plumbing.

- `reduceStep3 : (acc, key, data) ↦ (acc ⊕ (key∧data), key, data)` — 3-leaf prim, type
  exactly `Prod(B,Prod(B,B))` (§3 shape pin) → recognizes to a single `Ccnot`.
- `data = arr[idx]` loaded via `memexch` (f1) borrow-swap (non-destructive read), as in
  f3; `key` a fixed carrier bit; `idx` advances via `succ`.
- `dotReduce = iter Nat_d [ load arr[idx] into data; reduceStep3(acc,key,data); restore
  arr[idx]; succ idx ]`. Body plumbing mirrors f3's `reduce_iso` (array_histogram.rs) —
  the memexch borrow-swap wraps a 3-leaf reduceStep instead of a 2-leaf one; the exact
  Assoc/times skeleton is pinned by the probe (§7).

### Register budget (F35-6 — NOT f3's clean regime)
`numeral_ty(d)` has `d` leaves; at **d=3** payload is 4 (acc,key,data,idx) ⇒ head = 3+4
= **7** ⇒ Reg0..6 + one free, but `lower_data_dependent_iter` needs **2** aux ⇒
`allocate_aux` displaces the last non-counter leaf: **idx → Mem(2)** (arr occupies
Mem0/1). This is exactly f3's *d=4* displaced regime (proven safe by f3's T-F2-7/T-d4;
paging spare is aux `rb`). Consequence: idx `succ` pages a `Mexch` pair each body; the
spine gains the displacement pair. **Numbers (B, T(k)) are measured from the probe**,
not asserted — expect L_b ≈ f3-d4's body length. (Alternative weighed and rejected:
keeping `key` resident at Mem(2) to shrink head to 6 — saves the idx displacement but
pages around every Ccnot instead; the probe may revisit if the histogram is ugly.)

### WCET / park-tax shape (F35-7)
T(k) = (d−1)·S + L_b·k, B = (d−1)·(S + L_b) — affine, monotone, T(k_max)=B; tightness
structural (k_max = d numeral). **Park tax → 0 at k = d−1** (Nat_d numerals are 0..d−1;
f3 d=3 hit 0% at k=2). Pin the measured B/T(k) in T6a **after** the probe.

## 6. Test plan (ships with the change — L20/L21)

Instruction tier (`tests/ccnot.rs`, new):
- **T1** — bare `reduceStep3` prim compiles to exactly `[Ccnot{rd:acc_home,
  rs:key_home, rt:data_home}]` (pins target-first leaf-order §3; fails if rd/rs/rt
  permuted).
- **T2** — `run_on_cpu == eval` for `reduceStep3` across all 8 input assignments (real
  `system_step`, L19 — the authority check that the decoder arm is correct).
- **T3** — `Ccnot` encode/decode round-trip (opcode `0b1011`); distinctness rejects
  (`CcnotNotDistinct` on each aliased pair).
- **T4** — `Ccnot` self-inverse on the CPU (apply twice → identity) for distinct
  operands.
- **T-dense** (F35-10) — dense-byte semantics: on states with `r[rs]=0xAA, r[rt]=0x55`
  etc., assert `run_on_cpu`/`reference()` change **only bit 0** of `r[rd]` (guards the
  bit-0 vs byte-parallel divergence on bits 1–7). Use the `tests/decoder.rs`
  `op_samples`/`reference` harness once extended (see below).
- **T-neg** (F35-8, corrected) — a **2-leaf** prim `(c,t)↦(c,t⊕c)` recognizes as `Xor`,
  not `Ccnot`; an aliased-operand `Ccnot` program is rejected at `encode`.

Integration tier (`tests/dot_reduce.rs` — the "reproduces the real scenario" bar):
- **T5** — masked reduce over a `d`-element array: `run_on_cpu == eval` for several
  key/array assignments incl. **key=0** (⇒ acc unchanged — distinguishes Toffoli from
  CNOT), **key=1** (⇒ reduces to f3's XOR parity), and mixed. Exact-instruction assert
  on the body (target-first ⇒ rd is the acc home directly, no swap subtlety).
- **T6a** — default histogram: `B == <measured>`, `T(k) == <measured formula>`,
  monotone, park tax 0 at k=d−1 (pins the probe numbers).
- **T6b** (`#[ignore]`, L19 pricing tier) — energy via `rom_program_energy_resident`.
- **T7** — SystemRing closure `validate() == Ok` with `Op::Ccnot` in `Op::ALL`.

Pre-existing-test edits the new opcode forces (F35-3/4/9 — worker must be told):
- `tests/negative_guards.rs::decode_reserved_r_format_opcode_1011` — flip
  `decode(0xD800)==None` to the `Ccnot` decode assertion.
- `tests/decoder.rs` — extend `op_samples()` (exhaustive `match op`, no catch-all → hard
  compile error otherwise) + add the 0b1011 arm to `reference()`; this simultaneously
  provides T-dense's coverage.
- `tests/ring_two_body_timing.rs::golden_pin_mexch_rom` — re-pin d_pre / d_locked /
  period from the probe's measured values (the netlist grows by one op; same class as
  the MEXCHR re-pin; `validate()==Ok` ⇒ legit re-pin, not a regression).

Whole-suite (mandatory around merge — the f2 lesson): full `cargo test -p theseus-tisc`.

## 7. Probe (worker's FIRST step, before writing the full suite)

1. asm.rs (`Ccnot` + encode/decode) + `exec_instr` (bit-0 fast-sim).
2. recognizer arm (§3, target-first).
3. decoder arm §2 + `Op::Ccnot` in `Op::ALL`.
4. T1 + T2 (single reduceStep3: one `Ccnot`, `run_on_cpu == eval`) **and** `validate()
   == Ok` — this is the go/no-go on the decoder arm's reversibility.
5. Build the reduce program (§5); read its real `B`, `T(k)`, histogram → pin T6a.
If the closure breaks or the plumbing is subtler than f3, STOP and report — the design
changes before proceeding (same gate as f1/f2/f3).

## 8. Delegation shape

One maestro task (tier 0, theseus profile): the `Ccnot` instruction (asm + decoder +
recognizer + fast-sim) **plus** the dot-reduce integration test — the instruction is
unusable without a program exercising it, one coherent deliverable.

`file_allowlist`:
- `crates/theseus-tisc/src/asm.rs`
- `crates/theseus-tisc/src/decoder.rs`
- `crates/theseus-tisc/src/compile.rs`
- `crates/theseus-tisc/tests/ccnot.rs`
- `crates/theseus-tisc/tests/dot_reduce.rs`
- `crates/theseus-tisc/tests/negative_guards.rs`  *(pre-existing 0xD800 edit — F35-9)*
- `crates/theseus-tisc/tests/decoder.rs`           *(exhaustive match + reference — F35-3)*
- `crates/theseus-tisc/tests/ring_two_body_timing.rs` *(golden re-pin — F35-4)*

(Dropped `src/instr.rs`: the Op table lives in decoder.rs; nothing in instr.rs
enumerates opcodes — F35-9.)
`check_commands`: `cargo test -p theseus-tisc` (full — not scoped, per the f2 lesson).

## 9. Concurrency note (Thread A of the f3.5 ‖ spill pair)

f3.5 touches `asm.rs`, `decoder.rs`, and `compile.rs::{recognize_prim, exec_instr}`.
Thread B (loop-carrier spill) touches `compile.rs::{lower_trace, home_of,
MAX_LEAVES_LOOP}` — **disjoint files; disjoint compile.rs functions** (the review
confirmed §9: Thread B does not touch the `iter` guarded-unroll path f3.5 uses). Only
`compile.rs` is shared, in non-overlapping regions. Merge f3.5 **first** (bigger
blast-radius, and spill can rebase onto it); spill rebases/re-gates on the new tip
([[maestro-no-merge-under-inflight-task]]). widen-PC is deferred until after f3.5 merges
(it collides with f3.5 on asm.rs/decoder.rs/step.rs — never run concurrently).
