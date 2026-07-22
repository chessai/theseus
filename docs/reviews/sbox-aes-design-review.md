# DESIGN REVIEW — TP-5 byte S-box (AES-8 vs PRESENT-4), bit-sliced over Flat(8) (adversarial, fresh-context, with POC)

**Reviewer:** fresh-context Fable 5, did not write the design. POC built + run in an isolated worktree `/tmp/sbox-poc` detached at `7c78692` (theseus-v2 tip). 8/8 tests green (5 fast <0.01s; 3 heavy `#[ignore]`: reversibility + energy 876s, system-ring 162s). HEAD unchanged, single untracked `crates/theseus-tisc/tests/sbox_poc.rs`; `/home/chessai/rust/theseus` untouched. Reviews `docs/scratch/sbox-aes-design.md`, mirroring the merged Keccak-χ slice.

*(Advisor's faithful capture, L21. POC preserved at `docs/scratch/sbox_present_poc.rs.ref`.)*

## VERDICT: BUILD-PRESENT-4-FIRST

The register-pressure question the design flagged as "THE crux" is decisive and settles the target against AES-8. A bit-sliced Boyar–Peralta AES S-box has a **peak of 30 simultaneously-live signals** (37 if inputs are preserved); this machine has **8 registers + 6 usable Scratch slots + 4 Mem words = 18 addressable live slots**. 30 live means ≥22 must sit outside registers, but only 10 spill slots exist — **AES-8 literally cannot hold its live set on this machine**, let alone garbage-free, without a fundamentally different recomputation-heavy schedule that trades a large gate blow-up to shrink the live window. PRESENT-4 fits in **6 registers with a single product temp and zero paging**, is a genuine full-byte cryptographic S-box, and is the correct incremental step between χ and a future AES-8 (which needs a wider machine or a dedicated streaming-schedule slice). The POC builds PRESENT-4 end-to-end, proves it against the published 16-entry table over the whole domain + a discriminating packing + golden vectors + bijectivity, and confirms **standalone garbage-free** (inputs preserved ⇒ every temp is uncomputable — strictly stronger than χ's garbage-free-under-adjoint). Zero lower/recognizer/decoder change. Ship PRESENT-4 as slice-1; AES-8 is a documented follow-up.

## 1. Register-pressure analysis — THE headline (peak-live for both candidates)

**Machine budget (verified against `src/types.rs` + `src/asm.rs`):** `NUM_REGS=8` (r0..r7), `NUM_SCRATCH=8` but `Xchg` rejects `sx<2` (s0/s1 decoder-reserved) ⇒ **6 usable scratch** (s2..s7), `MEM_WORDS=4` (Mexch/MexchReg). Ccnot/Xor operate on **registers only** — every live value participating in a gate must be paged into a register first. Total addressable live capacity = **8 + 6 + 4 = 18**.

**AES-8 (Boyar–Peralta CHES-2012 circuit).** I reconstructed the full 115-gate netlist (32 AND + 83 XOR/XNOR — top/mid/z/bottom linear layers) and **validated it independently**: it reproduces the AES S-box table (GF(2⁸)-inversion+affine, computed separately via `x²⁵⁴`) on **all 256 inputs** (with U0 = MSB), self-checked at S(0x00)=0x63, S(0x53)=0xED, S(0xFF)=0x16. Live-range analysis over that verified schedule:

| variant | PEAK LIVE | peak location | fits 18 slots? |
|---|---|---|---|
| inputs consumable (in-place OK) | **30** signals | gate 37 (`t16`, deep in the middle GF(2⁴)-inversion layer) | **NO** (needs ≥22 spill, have 10) |
| inputs preserved (out-of-place) | **37** signals | gate 37 | **NO** |

A furthest-next-use paging simulation (8 regs + a 10-slot spill pool, in-place XOR reuse) **overflows the spill pool** and would need ~111 swap instructions (~232-instr ROM). The blocker is not "expensive paging" — it is that the **middle-layer live set (30) exceeds total machine storage (18)**. AES-8 is infeasible on the current machine without a schedule that recomputes intermediates to keep <18 live (a research-grade slice with its own review), or a wider register file.

**PRESENT-4.** ANF via Möbius transform of the published table `[C,5,6,B,9,0,A,D,3,E,F,8,4,7,1,2]` (LSB=x0):
```
y0 = x0 + x2 + x1x2 + x3
y1 = x1 + x0x1x2 + x3 + x1x3 + x0x1x3 + x2x3 + x0x2x3
y2 = 1 + x0x1 + x2 + x3 + x0x3 + x1x3 + x0x1x3 + x0x2x3
y3 = 1 + x0 + x1 + x1x2 + x0x1x2 + x3 + x0x1x3 + x0x2x3
```
Peak live with inputs held in r0..r3 (preserved), one output accumulator, one product temp: **6 registers** (r0-r3 inputs + r4 acc + r5 temp), **0 scratch for computation** (scratch used only to park finished outputs). Comfortably inside 8 registers, **no paging**. Quadratic terms accumulate straight into the accumulator via `Ccnot{acc,xi,xj}` (no temp); only degree-3 terms need the single temp r5, which is uncomputed immediately (inputs still resident ⇒ `Ccnot` twice = 0).

## 2. Chosen target: PRESENT-4, and why

PRESENT is a real lightweight-crypto S-box (the nonlinear layer of the PRESENT block cipher), not a toy — a genuine 4→4 byte substitution, exactly the "full byte S-box" milestone, at a size the machine can carry garbage-free. It exercises the **Flat(8) byte-parallel masked-AND at S-box scale** (31 AND-gates, degree-3 ANF, product-temp discipline with uncompute) — a strict step up from χ's 5 masked-ANDs — while staying Rust-composed and off the lowering path. AES-8 is deferred to a follow-up slice that first solves the >18-live scheduling problem.

## 3. POC results (`crates/theseus-tisc/tests/sbox_poc.rs`, 8/8 pass)

- **Assembles?** Yes — `assemble(&sbox_instrs())` succeeds; all 47 instrs are `{Xor, Ccnot, Xori, Xchg}`, already encodable. Rust-composed `Vec<Instr>`, assembled directly — **never enters lowering/recognize_prim/decoder** (the strong no-new-machinery answer, same as χ).
- **Whole-domain == TABLE** (`sbox_whole_domain_packed`): all **16 inputs** packed ×8, `run_rom_fast` output nibble == `PRESENT_SBOX` table on every lane, plus whole-byte 0x00/0xFF check. The table (a plain 16-entry Rust constant) is the **independent oracle** — never checked against the gate logic (no L19 self-checker).
- **Discriminating packing** (`sbox_discriminating_packing`): 8 *distinct* inputs one-per-bit, each bit position independently == its own table entry (catches an 8-lane / bit-0-only-decoder bug).
- **Golden vectors** (`sbox_golden_vectors`): S(0)=C, S(7)=D, S(A)=F, S(F)=2 pinned from the published table and matched by the ROM.
- **Bijection** (`sbox_forward_bijective`): the 16 outputs are all distinct — a permutation (the defining S-box property).
- **Reversibility** (heavy, `sbox_reversible_roundtrip_eval`): step-iso `eval` forward 47 cycles then adjoint 47 cycles restores the exact input **including temps and parked scratch** → 0. Real-machine (step-iso) authority.
- **System-ring L19 authority** (heavy, `sbox_system_ring_authority`, 162s): the **closed real CPU** (`system_ring`, 47 revolutions) reproduces the table over 6 discriminating inputs, with **ancillas restored + bus quiescent** on the start states and inputs preserved on the ring.

## 4. Measured energy + 8× bit-slicing + instruction count

`sbox_energy_and_bitslicing` (real rung-0 char DB, `t_ramp=1e-7`, `balanced_energy(lower_iso(step(rom)))`):
```
PRESENT S-box ROM: 47 instrs, 31 byte-parallel Ccnots
step_j = 5.9974e-9 J/cycle ;  B = 47 cycles ;  total = 2.8188e-7 J
bit-sliced (Flat8, 8 evals): 3.5235e-8 J / S-box
un-sliced  (Flat1, 1 eval):  2.8188e-7 J / S-box
→ 8× throughput for identical ROM + identical energy
```
Data-independent model (`total_j = B×step_j`): the same 47-instr ROM burns 2.82e-7 J whether its 8 bit-lanes carry 1 or 8 evaluations → **3.52e-8 J/S-box sliced vs 2.82e-7 J un-sliced, the 8× win explicit**. One 8-way S-box at 2.82e-7 J is ~3.9× fib (7.3e-8 J) — a real workload. Inventory pin (`sbox_instruction_inventory`): **47 = 31 Ccnot + 10 Xor + 2 Xori + 4 Xchg**. (A factored circuit sharing sub-products — e.g. p01, q013, q023 computed once — could cut the Ccnot count materially; not needed for the POC, noted as an optimization for the delegation or a follow-up.)

## 5. Garbage-free honesty (standalone vs under-adjoint; scratch usage)

**STANDALONE garbage-free** — stronger than χ's Rev-A. Because inputs r0..r3 are **never mutated**, every product temp is uncomputable within the forward pass: a degree-3 term does `Ccnot{r5,xi,xj}; Ccnot{acc,r5,xk}; Ccnot{r5,xi,xj}` and the second `Ccnot` returns r5 to 0. The whole-domain test **asserts** r4 (acc) = 0 and r5 (temp) = 0 and r0..r3 = the original inputs after every one of the 16 runs. Scratch usage: s2..s5 hold the four output nibbles (parked via `Xchg{acc,sx}`, which also reloads the now-zero old scratch value into acc for the next output — free re-zeroing); s0/s1 untouched (decoder-reserved). So forward-only leaves the machine clean: inputs preserved, outputs in s2..s5, acc/temp zero. No dirty-ancilla caveat (unlike χ). The `#[ignore]` reversibility test additionally proves the adjoint restores everything, and the system-ring proves the closed physical machine reproduces this.

## 6. Does it need any src/recognizer/decoder change? NO — with evidence.

The POC is a hand-composed `Vec<Instr>` of `{Xor, Ccnot, Xori, Xchg}` — all encode + execute today; byte-parallel `Ccnot` is the exact gate `valwidth_masked`/χ ship at Flat(8); `Xchg{rd,sx}` (sx≥2) is the exact scratch-paging `mde_spill`/`valwidth_masked` use. Nothing compiled from an Iso → `recognize_prim`, `selection_by_equiv`, `lower_trace`, decoder all untouched. Confirmed by: `assemble` succeeds with no lowering call; the ROM runs on `run_rom_fast`, step-iso `eval`, and the full `system_ring` unchanged. PRESENT-4 is **off the daemon control-flow / lowering hot path** — the lighter (non-BVEQ-class) review bar, needing only L19 real-run authority (delivered: eval-adjoint + system-ring).

## 7. Recommended delegation scope + what the POC settled

**Settled by this POC (worker lifts verbatim):** the target decision (PRESENT-4, AES-8 infeasible — peak-live 30 vs 18-slot ceiling); the exact 47-instr garbage-free schedule (`sbox_instrs()`); the independent 16-entry table oracle; the whole-domain / discriminating / golden / bijection / reversibility / energy / system-ring battery; the register map (inputs r0-r3 preserved, acc r4, temp r5, outputs s2-s5); standalone-garbage-free framing with the acc/temp-clean + inputs-preserved assertions.

**Delegation builds/decides (ONE fast-tier delegation, TEST-ONLY, allowlist `crates/theseus-tisc/tests/**`):**
1. Promote `sbox_poc.rs` → `crates/theseus-tisc/tests/sbox.rs`; the 5 fast tests default-tier, the 3 heavy tests behind the repo's `heavy-tests` tiering (`#[ignore]` + advisor runs `--features heavy-tests -- --ignored --test-threads=1` pre-merge), mirroring `feistel`/`valwidth_masked`. NOTE: the POC keeps the 3 heavy tests as plain `#[ignore]` — the delegation should decide the exact tiering to match the repo's current heavy-tests convention (a fast binary with #[ignore]d heavy tests stays in the default gate; don't file-`cfg` it out).
2. Module docs stating **standalone garbage-free** honestly (inputs preserved ⇒ temps uncomputed; contrast χ's under-adjoint-only), with the acc/temp-clean assertion as the L20 regression guard.
3. (Optional, in-slice or follow-up) a factored schedule sharing sub-products to cut the 31→fewer Ccnots + re-pin the energy/inventory goldens.
4. **OUT of slice-1 (own slice + own review):** AES-8 — requires first solving the >18-live scheduling problem (recomputation-to-bound-live-set, or a wider machine); the peak-live analysis here is the starting brief.

**POC worktree git status (final):** `/tmp/sbox-poc` detached at `7c78692` (HEAD unchanged), one untracked file `crates/theseus-tisc/tests/sbox_poc.rs`. Nothing committed. `/home/chessai/rust/theseus` untouched.
