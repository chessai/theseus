# TISC-1 concrete specification and C3 static budget

This document concretizes the TISC-1 ISA described in ADR-0030 and
derives the static resource budget required by C3 before the CPU is
built. Every claim is grounded in the ratified ADRs; items marked
**PROPOSAL** are reasonable choices on points where the ADRs are silent.

---

## 1. State layout

Per ADR-0030 P5 (W=8, 8 registers, 16-bit instruction words) and
ADR-0029 (InstrReg, register file, Option-A scratch):

| Field | Width (bits) | Count | Total bits | Notes |
|-------|-------------|-------|------------|-------|
| General registers r0–r7 | 8 | 8 | 64 | P5: W=8, 8 registers |
| Program counter (PC) | 8 | 1 | 8 | **PROPOSAL:** W-wide; addresses 256 ROM words, sufficient for T≤64 with room |
| Branch register (BR) | 8 | 1 | 8 | P3: PC-exchange target; same width as PC |
| Instruction register (IR) | 16 | 1 | 16 | P4/P5: 16-bit instruction word; provably zero between steps |
| Scratch words s0–s7 | 8 | 8 | 64 | ADR-0029 Option-A: ~8 scratch words as product type in State |
| **Total** | | | **160** | |

**Rail count:** dual-rail encoding (ADR-0027) → 2 rails per bit →
**320 rails**.

**Reconciliation with ADR-0030 risk note:** the risk register states
"~150 State bits ≈ 300 rails." Our 160-bit count exceeds the estimate
by 10 bits (20 rails), attributable to the 8-bit PC and BR (the risk
note likely assumed narrower counters or fewer scratch words). The
difference is within noise for the static budget; 320 rails is the
figure carried forward.

---

## 2. Instruction set

Every instruction is an iso `State <-> State` (P1). All data writes
are merges or exchanges (P2); no destructive MOV; shifts are excluded
(they erase), rotates are used instead. Control flow is PC-exchange
(P3). Each instruction lists its forward semantics, its adjoint, and
whether it is self-inverse.

In the table, `rd`, `rs`, `rt` are general registers (r0–r7); `imm8`
is an 8-bit immediate embedded in the instruction word; `sx` is a
scratch word addressed by a 3-bit field. All arithmetic is mod 256.

### Register-register instructions

| Mnemonic | Semantics (forward) | Adjoint | Self-inverse? |
|----------|--------------------|---------|----|
| `ADD rd, rs` | `rd ← rd + rs` | `SUB rd, rs` | no |
| `SUB rd, rs` | `rd ← rd − rs` | `ADD rd, rs` | no |
| `XOR rd, rs` | `rd ← rd ⊕ rs` | `XOR rd, rs` | yes |
| `EXCH rd, rs` | `rd ↔ rs` | `EXCH rd, rs` | yes |
| `ROTL rd, rs` | `rd ← rd ⊲ (rs mod 8)` | `ROTR rd, rs` | no |
| `ROTR rd, rs` | `rd ← rd ⊳ (rs mod 8)` | `ROTL rd, rs` | no |
| `CSWAP rd, rs, rt` | if `rd[0]`: `rs ↔ rt` | `CSWAP rd, rs, rt` | yes |

### Register-immediate instructions

| Mnemonic | Semantics (forward) | Adjoint | Self-inverse? |
|----------|--------------------|---------|----|
| `ADDI rd, imm8` | `rd ← rd + imm8` | `SUBI rd, imm8` | no |
| `SUBI rd, imm8` | `rd ← rd − imm8` | `ADDI rd, imm8` | no |
| `XORI rd, imm8` | `rd ← rd ⊕ imm8` | `XORI rd, imm8` | yes |
| `ROTLI rd, imm8` | `rd ← rd ⊲ (imm8 mod 8)` | `ROTRI rd, imm8` | no |
| `ROTRI rd, imm8` | `rd ← rd ⊳ (imm8 mod 8)` | `ROTLI rd, imm8` | no |

### Scratch exchange

| Mnemonic | Semantics (forward) | Adjoint | Self-inverse? |
|----------|--------------------|---------|----|
| `XCHG rd, sx` | `rd ↔ s[sx]` | `XCHG rd, sx` | yes |

This is the Option-A scratch access path (ADR-0029): an exchange, not
a copy. The source routes it through a named iso, preserving the
Option-B seam.

### Control flow (ADR-0034)

| Mnemonic | Semantics (forward) | Adjoint | Self-inverse? |
|----------|--------------------|---------|----|
| `SWAPBR rd` | `BR ↔ r[rd]` | `SWAPBR rd` | yes |

Per ADR-0034 (Pendulum/PISA lineage, Frank/Vieri): SWAPBR swaps BR
with a general register, **not** with PC. BR is a PC *offset*
consumed by the uniform end-of-step update `PC += BR + 1 (mod 256)`.
BR = 0 is fall-through (`PC += 0 + 1 = PC + 1`). No instruction
writes PC during execute; the execute phase's write set is restricted
to `{r0..r7, s0..s7, BR}`. Conditional branching is achieved by
preceding SWAPBR with a CSWAP that conditionally exchanges BR with a
target-offset register.

### Workload coverage

- **Fibonacci:** ADD, SUB, EXCH (register swap for shift), SWAPBR
  (loop), ADDI (constants). Sufficient.
- **Feistel round:** XOR, ROTL/ROTR (in-place rotation), XCHG
  (scratch for round keys), EXCH. Sufficient.

### Bijectivity argument

Every instruction is an iso by construction (P1/P2): merges (+=, −=,
⊕=) with a known second operand are invertible; exchanges are
self-inverse; rotates are invertible. The coverage checker enforces
totality: undefined opcodes are compile errors. The adjoint of `step`
is the sequence of individual adjoints in reverse order.

---

## 3. Opcode encoding (PROPOSAL — TBD-9)

Per ADR-0030 P5, opcode encodings are an implementation-time document.
The following is a **proposal** for the 16-bit instruction word layout;
the coverage checker (P1) will enforce ISA totality regardless of the
final encoding.

### Formats

Two formats, distinguished by bit 15:

**R-format** (bit 15 = 1): register-register and 3-register operations.

```
 15  14      11  10    8   7    5   4    2   1   0
[1 ][ opcode  ][ rd    ][ rs    ][ rt    ][ pad  ]
```

**I-format** (bit 15 = 0): register-immediate operations.

```
 15  14    12  11    9   8                      1   0
[0 ][ opc  ][ rd    ][      imm8               ][ p]
```

### Bit allocation

| Format | Field | Bits | Width | Notes |
|--------|-------|------|-------|-------|
| R | type | 15 | 1 | = 1 |
| R | opcode | 14–11 | 4 | 16 R-format slots |
| R | rd | 10–8 | 3 | destination/first register |
| R | rs | 7–5 | 3 | source/second register |
| R | rt | 4–2 | 3 | third register (CSWAP); scratch addr (XCHG); unused otherwise |
| R | pad | 1–0 | 2 | reserved zero |
| I | type | 15 | 1 | = 0 |
| I | opc | 14–12 | 3 | 8 I-format slots |
| I | rd | 11–9 | 3 | destination register |
| I | imm8 | 8–1 | 8 | immediate value (full W-width) |
| I | pad | 0 | 1 | reserved zero |

### Proposed opcode map

R-format (16 slots):

| opcode[3:0] | Instruction |
|-------------|-------------|
| 0000 | ADD rd, rs |
| 0001 | SUB rd, rs |
| 0010 | XOR rd, rs |
| 0011 | EXCH rd, rs |
| 0100 | ROTL rd, rs |
| 0101 | ROTR rd, rs |
| 0110 | CSWAP rd, rs, rt |
| 0111 | XCHG rd, rt (scratch exchange; rs ignored) |
| 1000 | SWAPBR rd (rs, rt reserved zero; ADR-0034) |
| 1001–1111 | reserved (compile error per P1) |

I-format (8 slots):

| opc[2:0] | Instruction |
|----------|-------------|
| 000 | ADDI rd, imm8 |
| 001 | SUBI rd, imm8 |
| 010 | XORI rd, imm8 |
| 011 | ROTLI rd, imm8 |
| 100 | ROTRI rd, imm8 |
| 101–111 | reserved (compile error per P1) |

This encoding is sufficient for both D3 workloads and leaves room for
expansion. **The final encoding is TBD-9; the coverage checker
enforces totality regardless.**

---

## 4. The step iso (ADR-0034)

`step : State <-> State` is a single iso composed of five phases
(ADR-0029, ADR-0030 P4, ADR-0034):

1. **Fetch.** CNOT the ROM word at address PC onto the zeroed IR.
   Copy-onto-known-zero is reversible (ordinary clauses). The ROM is a
   Fredkin tree (Branch tree on PC bits steering controlled swaps),
   i.e. an addressed exchange with a known-zero register — exactly the
   ADR-0027 cell pattern. The ROM threads through State linearly.

2. **Decode + Execute.** Branch tree on the opcode bits of IR (P1).
   Each leaf is one instruction iso. The coverage checker ensures
   every opcode maps to exactly one leaf (left-exhaustive) and no two
   leaves overlap (right-exhaustive / non-overlap). Undefined opcodes
   are compile errors. The execute phase mutates a subset of
   `{r0..r7, s0..s7, BR}` via the merge/exchange rules of P2
   (ADR-0034 D1: **no instruction writes PC**). IR bits act purely as
   *controls* (the one legal fanout, ADR-0027); data in IR is never
   consumed.

3. **Unfetch.** CNOT IR against the same ROM word to re-zero IR (P4:
   IR provably zero between steps). Because PC is constant across
   fetch → execute → unfetch (D1: nothing writes it), the unfetch
   XORs the same ROM word the fetch did, unconditionally (ADR-0034 D3).

4. **PC += BR.** Add the branch register (offset) into PC via the
   Takahashi–Kunihiro zero-ancilla in-place ripple adder (≤ K2). BR
   is preserved. BR = 0 is fall-through.

5. **PC += 1.** Ripple increment (the existing pc_inc).

Together phases 4–5 implement the uniform end-of-step PC update
`PC += BR + 1 (mod 256)` (ADR-0034 D2). The entire
fetch–execute–unfetch–advance sequence is one iso. Running T steps =
unrolling `step` T times (C2.4, unroll-only in v1). The adjoint
`~step` runs the five phases in reverse order, each replaced by its
adjoint. `step` is a total bijection on the full State space
(ADR-0034 D4).

---

## 5. C3 static budget (go/no-go gate)

**These are principled estimates with the derivation shown, not
measurements. The estimate becomes exact when `step` is compiled and
cells are counted — deferred until `step` exists.**

### 5a. Reference data (from rung0-chardb.ron, t_ramp = 1e-7 s)

| Cell | Energy (J) | Ratio to k0 |
|------|-----------|-------------|
| cswap-k0 (buffer) | 9.53e-16 | 1× |
| cswap-k1 (CNOT) | 5.38e-15 | 5.6× |
| cswap-k2 (Toffoli) | 9.53e-15 | 10× |
| cswap-k3 | 1.44e-14 | 15× |

Reference structure: reversible full adder (1-bit, M3a bench) = **150
cells** (balanced, placed; exercises k2).

### 5b. State rails per tick

160 State bits × 2 rails/bit = **320 dual-rail signals**, all riding
k0 buffers every tick of pipeline depth (storage is motion, C2.1).
This matches the ADR-0030 risk note "~150 bits ≈ 300 rails" within
the 10-bit reconciliation from §1.

### 5c. Per-step cell count estimate

| Component | Cell estimate | Cell type(s) | Derivation |
|-----------|--------------|-------------|------------|
| **Fetch** (ROM → IR) | ~96 | k1 | Fredkin tree: depth log₂(64)=6 levels × 16 IR bits; each node is one k1-cswap |
| **Unfetch** (IR → 0) | ~96 | k1 | Same tree traversed in reverse |
| **Decode** (Branch tree) | ~512 | k1 | 4 opcode bits → depth 4; each level routes ~128 data bits (8 regs + 8 scratch × 8 bits) through k1-cswap |
| **ADD/SUB datapath** | ~1200 | k1, k2 | 8 × full-adder (150 cells each); carries are depth-sequential |
| **XOR datapath** | ~50 | k1 | 8 CNOT + balancing buffers |
| **EXCH datapath** | ~30 | k0 | 8 rail-pair swaps (wiring + buffers) |
| **ROTL/ROTR datapath** | ~200 | k1 | Barrel rotator: 3 levels × 8 conditional swaps + balancing |
| **CSWAP datapath** | ~80 | k1 | 8 controlled swaps + control routing |
| **ADDI/SUBI datapath** | ~1200 | k1, k2 | Same as ADD; immediate supplied from IR bits |
| **XORI datapath** | ~50 | k1 | 8 controlled NOTs + buffers |
| **ROTLI/ROTRI datapath** | ~200 | k1 | Same as ROTL/ROTR |
| **XCHG datapath** | ~80 | k1 | Fredkin tree on 3 scratch-addr bits × 8 data bits |
| **SWAPBR datapath** | ~80 | k0, k1 | 8 rail-pair swaps + rd crossbar (ADR-0034: BR↔r[rd]) |
| **PC += BR (offset add)** | ~46 | k1, k2 | TK zero-ancilla adder (same family as ADD) |
| **Instruction datapaths total** | **~3120** | mixed | Sum of all leaves in decode Branch tree |
| **Balancing buffers** | ~1800 | k0 | ~40% overhead (C2.2: mandatory minimal-buffer balancing) |
| **PC increment** | ~150 | k1, k2 | 8-bit reversible increment ≈ 1 full adder |
| **Per-step compute cells** | **~5774** | | |

**Pipeline depth of one step** (estimated, determines state-buffer
count):

| Phase | Depth (ticks) |
|-------|--------------|
| Fetch tree | 6 |
| Decode tree | 4 |
| Deepest instruction (ADD: 8-bit ripple carry) | ~30 |
| Decode return | 4 |
| Unfetch tree | 6 |
| PC increment | ~8 |
| **Total** | **~58** |

**State-riding buffers per step:** 320 rails × 58 ticks = **~18,560
k0 cells**. Most state bits are idle during any given pipeline phase
and must ride k0 buffers to maintain synchronization (C2.2).

**Total cells per step:** ~5,800 compute + ~18,600 state-riding ≈
**~24,400 cells**.

### 5d. Scaling to T steps (unrolled machine)

| T | Compute cells | State-riding k0 | Total cells | Total rails (peak) |
|---|--------------|-----------------|-------------|-------------------|
| 16 | 92,800 | 297,000 | 390,000 | 320 + circuit |
| 32 | 185,600 | 594,000 | 780,000 | 320 + circuit |
| 64 | 371,000 | 1,188,000 | 1,559,000 | 320 + circuit |

### 5e. Energy estimate per run (t_ramp = 1e-7 s)

Per-step energy breakdown:

| Component | Cells | Avg energy/cell | Energy (J) |
|-----------|-------|-----------------|-----------|
| Compute (fetch+decode+datapaths+PC) | ~4,000 k1-equiv | 5.4e-15 | 2.2e-11 |
| Balancing buffers | ~1,800 k0 | 9.5e-16 | 1.7e-12 |
| State-riding buffers | ~18,600 k0 | 9.5e-16 | 1.8e-11 |
| **Per step** | | | **~4.2e-11** |

Per-run totals:

| T | Energy/run (J) | Energy/step (J) |
|---|---------------|----------------|
| 16 | ~6.7e-10 | 4.2e-11 |
| 32 | ~1.3e-9 | 4.2e-11 |
| 64 | ~2.7e-9 | 4.2e-11 |

For context: the compiled Fibonacci oracle (specialized, no CPU
overhead) achieves 2.73e-12 J/op. The CPU step estimate of ~4.2e-11
J/step is ~15× larger — expected, since the CPU pays for
fetch/decode/state-buffering overhead that the specialized circuit
eliminates. This ratio is the quantitative cost of generality; the
ADR-0030 duality note observes that specializing step to a fixed ROM
constant-folds the CPU into the program's circuit, recovering it.

### 5f. Go / no-go recommendation

**GO for M4-v1 at T = 16–32.**

Rationale:
- At T=32, the estimated ~780k cells and ~1.3 nJ/run are within
  plausible compile and simulation budgets. The circuit is large but
  structurally repetitive (T copies of one step).
- State buffering dominates (~76% of cells), confirming the ADR-0030
  risk note's concern. This is inherent to the unrolled-T architecture
  and is the quantitative motivation for Option-B (ADR-0029) and the
  ring machine (C2.4/M4-v2).
- At T=64, the ~1.6M cell count approaches the boundary of practical
  compilation; recommend starting at T=16 and scaling.
- The energy figures are at t_ramp = 1e-7 s (100 ns ramp);
  slower ramps reduce energy proportionally (adiabatic scaling).

**What makes the estimate EXACT:** compiling `step` as a theseus iso
through the adiabatic backend and reading the C3 static energy report
(ADR-0028 C3.5/C3.6). This counts actual cells, actual balancing
buffers, actual pipeline depth — replacing every "~" above with a
measured number. Deferred until `step` exists; this budget authorizes
building it.

---

## 6. Optimization horizon (non-normative)

This section is non-normative; it records what the design admits for
later versions without committing to any of it.

**Systolic throughput-pipelining.** The adiabatic fabric is inherently
pipelined at single-gate granularity (ADR-0026); the unrolled-T
machine already benefits — each step's pipeline is fully occupied.
Throughput scales with pipeline depth without additional machinery.

**Step specialization (CPU → circuit duality).** Specializing `step`
to a fixed ROM constant-folds the decoder and fetch, collapsing the
CPU into the program's compiled circuit (ADR-0030 duality). This
eliminates fetch/decode overhead and most state-buffering, recovering
energy parity with directly compiled programs.

**Spatial ILP.** Register-disjoint instructions within a single step
can execute in parallel lanes, reducing pipeline depth and
state-buffering cost. Requires dependency analysis in the compiler.

**Ring machine (M4-v2).** The recirculating architecture (C2.4) reuses
one physical step block across T iterations, eliminating the T×
area multiplier. Wake condition: unrolled area unaffordable (formally
declared by M4; the budget above quantifies the threshold).

**Reversible retiming and Bennett pebbling.** Time-space trade-offs
within the unrolled pipeline: retiming shifts buffers to minimize
total energy (C2.1 cost model); Bennett-style pebbling strategies
can reduce peak state at the cost of additional steps.
