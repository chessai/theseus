# ADR-0041: Execution models -- per-program hardware vs stored-program CPU

**Status:** Proposed (map + open design questions; no locked decisions)

## Context -- two ways to run a theseus program

theseus is a reversible language whose type system guarantees every
well-typed program denotes a bijection. That bijection can be *realized*
in two fundamentally different ways, and conflating them is the source of
most confusion about "compiling" and "running" theseus code. This ADR
names the two execution models, records what already works, identifies
the missing backend that bridges them, and captures the open hardware
bring-up questions for when physical silicon is eventually on the table.

## 1. The two execution models

### (a) Spatial / per-program hardware

A theseus surface program compiles to its **own reversible circuit**.
The pipeline is parse → check → elaborate → lower to k-CSWAP/R1 cells.
Each program *is* the machine; "running" it means clocking that specific
circuit. This is hardware synthesis (analogous to Verilog → gates →
layout, or HLS), **not** compiling-to-an-ISA.

The natural emitted artifact is a **netlist**, not a ROM.

Example: `conformance/programs/0022-fib.thse` -- a surface-level
reversible Fibonacci using `iter Nat fibStep` -- elaborates to a core
`Iso`, lowers to a cell netlist, and its untimed evaluation matches the
interpreter over the entire finite domain
(`crates/theseus-netlist/tests/fib_lowering.rs`).

### (b) Temporal / stored-program

The TISC-1 CPU is itself **one** theseus program -- its `step` iso
(`crates/theseus-tisc/src/step.rs`), which is a model-(a) circuit when
compiled. Once that circuit is fabricated, you run arbitrary code on it
by loading a **ROM of TISC-1 machine code** and recirculating
(`crates/theseus-tisc/src/ring_run.rs`). Here the "compiler that
produces a ROM" is the **assembler** (`crates/theseus-tisc/src/asm.rs`)
and the target is the TISC-1 ISA (ADR-0030). The ROM belongs to this
model only.

The common intuition -- "every compiler emits an executable; ours emits a
ROM" -- **conflates these two models**. theseus-the-language emits a
circuit (model a); a ROM is the stored-program CPU's program input
(model b). They are related by the temporal/spatial duality noted in
ADR-0030: a CPU is an un-specialized spatial circuit; specializing `step`
to a fixed ROM constant-folds the CPU back into the program's own
circuit.

## 2. What already works

Arbitrary well-formed `.thse` programs are **already executable and
simulatable** today via two paths:

1. **The interpreter** -- `theseus_eval::eval`
   (`crates/theseus-eval/src/`) evaluates the core `Iso` directly on
   `Value`s, forward and backward. Brute-force differential testing
   verifies bijectivity over entire finite domains (ADR-0008).

2. **Lowering to cells + untimed netlist simulation** -- the differential
   oracle chain: surface → core `Iso` → `PhasedCellNetlist` (k-CSWAP/R1
   cells) → dual-rail untimed evaluation. The netlist result is checked
   against `eval` for every input. This is the path that
   `fib_lowering.rs` exercises.

Both paths handle the full v0.2 software dialect (μ-types, `iter`,
`fold`, `unfold`). "We can't run it yet" is therefore an overstatement:
what is missing is specifically **stored-program execution on the TISC-1
CPU** -- the ability to compile a surface `.thse` program into a TISC-1
ROM and run it on the ring machine.

## 3. The missing backend: surface theseus → TISC-1 machine code

There is currently **no compiler from surface theseus → TISC-1 machine
code**. The gap is nontrivial because the two sides are structurally
different:

- **Source side:** theseus programs are reversible isos over
  arbitrary-size algebraic types -- `Sum`, `Prod`, and recursive `μ`
  types whose inhabitants can be unbounded (e.g. `Nat = Zero | Succ
  Nat`). Control flow is `iter`/`fold`/`unfold` + `Branch` dispatch on
  sum tags.

- **Target side:** TISC-1 is a fixed-width (W = 8 bit) reversible
  register machine with 8 general registers, 8 scratch words, a branch
  register, and a PC -- all 8-bit. It has no heap, no stack, and no
  memory bus beyond registers (Option-A, ADR-0029).

Bridging them requires three pieces:

**(i) A memory/heap model.** Arbitrary-size typed values cannot fit in
8 registers. The CPU needs addressable storage -- this **wakes the parked
Option-B memory bus** (ADR-0029: memory as a separately compiled theseus
iso with a swap-bus interface; `step` becomes `(CpuState, BusMsg) <->
(CpuState, BusMsg')`). At scale, Option-C (adiabatic SRAM macro prim)
may follow.

**(ii) A reversible codegen backend.** Instruction selection, register
allocation, and -- critically -- **Bennett-style uncomputation of
temporaries**: every intermediate value the codegen materializes must be
reversibly erased, either by inline uncompute or by ancilla/garbage
discipline. Lowering `iter`/`fold`/`Branch` to TISC-1 control flow means
emitting `SWAPBR` loops with proper come-from pairing (ADR-0034 D5) and
conditional branching via `CSWAP`-then-`SWAPBR`.

**(iii) A reversible calling convention / ABI.** Ancilla and garbage
discipline across call boundaries: callee-save vs caller-save in a world
where "save" means "exchange into a known scratch location" (not copy),
and every temporary must be returned to its init state by the time
control returns. The Pendulum `CALL`/`RETURN` linkage (ADR-0034 D5) is
the control-flow primitive; the data convention is new work.

**Position:** this is a **v3 milestone** -- "the software dialect targets
the hardware CPU." It depends on the Option-B memory bus, the reversible
codegen, and a calling convention, none of which exist yet.

## 4. Tractable stepping stone: the word-oriented subset

A full surface→ISA backend is v3 work, but a useful intermediate is
reachable sooner.

**Proposal (v2.5 / early-v3 increment):** a compiler for the
**word-oriented subset** of `.thse` -- fixed-width integers/bits and
bounded `iter` loops with no recursive types deeper than the register
file can hold -- targeting TISC-1 ROMs.

**Demonstration:** compile a surface-level `fib` (the `swap ; addInto`
step under `iter`, exactly `conformance/programs/0022-fib.thse` but over
8-bit words instead of 1-bit) to the same `ADD ; EXCH` ROM the CPU
already runs, and check it against `eval`. This needs **no new memory
subsystem** -- the values fit in registers.

**Prerequisites already done (v2):**

- The software dialect with μ-types + `iter`/`fold`/`unfold`
  (ADRs 0035-0039).
- Reversible branching with `SWAPBR` / come-from pairing (ADR-0034).
- The ring machine with zero-garbage recirculation (ADR-0033, ADR-0040).

**What this increment adds:**

- A type-directed lowering pass: surface Iso (word-oriented subset) →
  TISC-1 instruction sequence.
- Register allocation for the fixed-register case (trivial: 8 registers,
  known layout).
- Integration test: compiled ROM runs on `StepRing::run`, result matches
  `theseus_eval::eval`.

This de-risks the full backend by exercising the instruction-selection
and ROM-emission machinery on a case where memory is not a variable.

## 5. Running on physical silicon -- open bring-up questions

These are gated behind **M5 / idealization rungs 3+** (ADR-0032), so
this section captures the questions now for solution at M5.

### Load

Getting the ROM + seed state into the chip. The recirculating ring needs
the ROM **fabric-resident** -- the fetch phase (CNOT ROM word onto IR,
`step.rs` §1) addresses the ROM by PC every revolution. Option-B
(ADR-0029) envisions memory as a swap-bus iso; the ROM is a special case
(read-only under normal execution, loaded once at initialization). How
the initial load crosses the chip boundary is an open question (likely a
serial scan-chain or a conventional I/O shell -- see below).

### Clock

Driving the **adiabatic power-clock** through its ramp phases for R
revolutions. The power-clock is not a square wave -- it is a multi-phase
sinusoidal/trapezoidal ramp whose period directly controls the
energy/operation tradeoff (ADR-0028 C3.1: `E(T) = k_adia/T + k_leak·T +
k_res`). This requires an **external resonant clock generator**
(LC-tank or piezoelectric resonator).

Note: the money-plot claims boundary **already excludes**
clock-generation/resonator losses and I/O (ADR-0028 C3.3). The adiabatic
energy story is about the logic core's dissipation under ideal
power-clocks; the clock generator is a separate engineering artifact with
its own efficiency budget.

### Readout

Sensing the final dual-rail register state after R revolutions.
Non-destructive readout of reversible logic is subtle: measuring a
dual-rail pair conventionally (connecting it to a CMOS sense amp) is
itself an irreversible operation that injects entropy into the rail. The
readout scheme must either accept this (it happens once, at the end) or
use a reversible-to-conventional transducer at the I/O boundary.

### The reversible-core ↔ irreversible-world boundary

Every bit crossing in or out of the adiabatic core is a **conventional
lossy transfer**. The machine is a reversible core in an irreversible I/O
shell: load, readout, and clock generation are all irreversible
operations that happen at the boundary. This is by design -- the energy
win is in the core computation, which runs for R revolutions between
boundary events. The ratio of core work to boundary cost scales with R.

## 6. Simulating the hardware without a board

A physical FPGA is **not required** for functional validation. The M2
path lowers the design to RTL and validates it with **Verilator** --
cycle-accurate software RTL simulation. Full functional bring-up (load
mechanics, clock sequencing, readout protocol) can be prototyped and
tested against the RTL sim with no board whatsoever. Out-of-stock
development boards are irrelevant to simulation.

If real-hardware runs are ever wanted before tape-out, cloud FPGAs
(e.g. AWS F-instances with Xilinx Alveo) exist and can host the design
without procuring physical boards.

**Crucial caveat:** an FPGA or RTL simulation validates **function
only** -- it runs on standard CMOS, so the **adiabatic energy story does
not appear there**. Energy lives solely in the SPICE/adiabatic backend
(ngspice characterization, the rung-0 char DB, the money-plot sweep --
ADR-0028, ADR-0025). FPGA results must never be mistaken for energy
evidence. The two validation axes are orthogonal: RTL/FPGA for
functional correctness, SPICE for energy scaling.

## Consequences

- The two execution models are named and distinguished; no future
  discussion should conflate "compile to a circuit" with "compile to a
  ROM."
- The missing surface→ISA backend is positioned as v3 work with clear
  prerequisites (Option-B memory bus, reversible codegen, ABI).
- The word-oriented-subset stepping stone provides a de-risking path
  that exercises ROM emission without requiring the memory bus.
- Hardware bring-up questions (load, clock, readout, boundary) are
  captured for M5 without blocking current work.
- RTL/Verilator simulation is confirmed as the functional validation
  path; energy validation remains in SPICE.

## Cross-references

- ADR-0028 -- energy annotation methodology; claims boundary (C3.3)
- ADR-0029 -- memory: exchange; Option-A/B/C stratification
- ADR-0030 -- TISC-1 ISA principles
- ADR-0033 -- ring machine: recirculating execution
- ADR-0034 -- reversible branching: SWAPBR / come-from
- ADR-0040 -- R1 cell: the zero-garbage ring
- ADRs 0035-0039 -- μ-types, iter/fold/unfold, v0.2 surface syntax,
  netlist lowering
- `crates/theseus-tisc/src/step.rs` -- the TISC-1 step iso
- `crates/theseus-tisc/src/asm.rs` -- the TISC-1 assembler
- `crates/theseus-tisc/src/ring_run.rs` -- the recirculating ring runner
- `crates/theseus-eval/src/` -- the interpreter (`eval`)
- `conformance/programs/0022-fib.thse` -- surface Fibonacci
- `crates/theseus-netlist/tests/fib_lowering.rs` -- differential oracle
  for compiled fib
