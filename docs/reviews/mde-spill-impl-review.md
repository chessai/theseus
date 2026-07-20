# Adversarial implementation review — M-D.e slice 1 (loop-carrier scratch-spill)

**Reviewer:** fresh-context Fable (adversarial), did not write the change. Reviewed worktree
`01KY0C76H9D45244V6CQ3HDFF4` @ `7be4a82` (base `chessai/theseus-v2`), diff confined to
`crates/theseus-tisc/src/compile.rs` (+127/−10) and new `crates/theseus-tisc/tests/mde_spill.rs` (+359).
Every claim below re-verified against the code and the actually-emitted 29-instruction ROM (dumped and
inspected instruction-by-instruction); fast tests + clippy independently re-run.

**VERDICT: SHIP-WITH-FIXES** — no code change required; the fixes are two doc-only items ((2a) design-doc
paragraph, (3b) `home_of` comment) that sat outside the worker's `crates/theseus-tisc/**` allowlist and
belong to the advisor finish. No BLOCKER, no MAJOR. The mechanism is correct as built.

## A. Byte-map exactness (highest risk): CHECKED, HOLDS — exact, instruction by instruction

Emitted sequence (`compile.rs:1141–1153`) vs `mde_walk_bytes` (`compile.rs:1170–1186`), leaf-ordered
(`regs[i]` = leaf *i*'s register, `recognize_prim` passes `leaves = (0..n_in).collect()` at 861):

| # | Instr | Byte op | Match |
|---|-------|---------|-------|
| 1 | `Xor t,p1` | `b[0]^=b[1]` | ✓ |
| 2 | `Xori t,1` | `b[0]^=1` | ✓ |
| 3 | `Xori p1,1` | `b[1]^=1` | ✓ |
| 4 | `Xor p2,p3` | `b[2]^=b[3]` | ✓ |
| 5 | `Xor p3,p4` | `b[3]^=b[4]` | ✓ |
| 6 | `Xori t,1` | `b[0]^=1` | ✓ |
| 7–9 | `Cswap t,p1,p2; t,p2,p3; t,p3,p4` | `if b[0]&1==1 { swap(1,2); swap(2,3); swap(3,4) }` | ✓ (see below) |
| 10 | `Xori t,1` | `b[0]^=1` | ✓ |
| 11 | `Xor p3,p4` | `b[3]^=b[4]` | ✓ |
| 12 | `Xor p2,p3` | `b[2]^=b[3]` | ✓ |
| 13 | `Xor p1,p2` | `b[1]^=b[2]` | ✓ |

- **Cswap condition and `t`-immutability:** fast sim `exec_instr` (`compile.rs:2966–2970`) tests
  `regs[rd] & 1 == 1` and swaps `rs,rt` — `rd` is read-only. Real decoder agrees: `decoder.rs:548` "CSWAP
  rd, rs, rt: copy the control bit r[rd][0] into s1 bit 4" (bit 0). The three swaps touch only indices
  1–4, never index 0, and no Cswap writes `t` — so one `if b[0]&1` guarding all three sequential swaps is
  exactly equivalent to three sequentially-evaluated Cswaps. Operands are pairwise-distinct registers
  (homes R1,R0,R2,R3 + spare r7, spare excluded from `used_regs` by construction) — no aliasing hazard.
- **Bijection:** each of the 13 steps is invertible on `(Z/256)^5` (xor-with-distinct-source, `Xori`
  involutions, guard-untouched conditional swaps), so the composition is a genuine bijection; on the 7
  canonical values `build_walk_body` (test) asserts closure-on-canonical + injectivity at table-build
  time, and `check_pattern_kleaf` re-verifies the total forward table against the same byte map at
  compile time (mismatch → falls through to `UnrecognizedPrim`, reject-don't-corrupt).
- **One layout authority:** `check_pattern_kleaf` uses `value_to_leaf_bytes` (`compile.rs:163`);
  `marshal → marshal_resident` (2702/2718) uses the same fn + `home_of`. The test's `sv.regs[..5]` read is
  canonical `Ctx::new` layout (leaf *i* → Reg(*i*)) — consistent. No split.
- **Trajectory coverage:** input 0's 6-iteration run visits all 7 canonical values
  (R0→L4→L3→L2→L1→L0→R1), so the CPU-vs-eval oracles exercise the full table, not a corner.
- Minor note (no action): `check_pattern_kleaf` checks `bytes.len()==k` on input only; output length is
  enforced implicitly by `Vec` equality against the length-k buffer — equivalent. Backward-table
  consistency is not verified — same as the pre-existing `check_pattern_2leaf` (1272); not a regression.

## B. Guard soundness: CHECKED, HOLDS — with one honesty note for the record

`carrier_reg_budget = NUM_REGS − 4 = 4` (2258); guard `n_input+1 ≤ 4` (2262). Walk of `body_homes`
(2270–2308): tag → `Reg(n_input)`; inputs 0..n_input → `ctx.homes[i]`; extras → `Reg(n_input+1+j)` while
`< 4`, else `Scratch(2+spill_count)`.

- **No admitted case scratch-homes a live input.** The canonical/unshared guard (2225–2232) forces
  `ctx.homes == Ctx::new(n_input).homes`, and `home_of` maps leaf *i* < 8 → `Reg(i)`; with `n_input ≤ 3`
  enforced, inputs are always Reg(0..2). A live-input-in-Scratch home would literally require
  `n_input ≥ 9` (canonical leaf 8 → Scratch(2)), which `n_payload ≤ 4` forbids twice over.
- **`n_extra` is exactly the padding count:** `n_carrier − 1 − n_input = max(|u|,|a|) − |a|` — the
  zero-extension leaves of `Right(a)` at entry (and of `Right(b)` at exit; they hold live `u` data only
  *mid-loop*, which is precisely what the bracket pages). They have no input home by construction.
- **Worst admitted case, constructed:** `n_input=3, n_payload=4` → tag=R3, inputs R0–R2, one extra at
  candidate reg 4 → spilled to Scratch(2). Carrier regs {0–3} = budget exactly; `max_carrier_reg=3`,
  aux 4–7, `ctrl=7 < NUM_REGS` (2381 passes). At most **one** spill is ever possible: spills =
  `n_extra − (3 − n_input) ≤ (4−n_input) − (3−n_input) = 1`, so `Scratch(3+)` and `spill_count` overflow
  are unreachable.
- **Honesty note (record, no action):** today the guard is belt-and-braces — any `n_input ≥ 4` case also
  dies at the aux sanity check `ctrl ≥ NUM_REGS` (2381–2386). Its value is as the *explicit* contract
  against a future cap/budget change (the r2 break scenario). The reject test is the contract pin;
  future editors must not read the redundancy as license to drop either guard.

## C. Allocation / collision — M-D.d neighbors: CHECKED, UNCHANGED

- `home_of` (394–408): leaf 8 → `Scratch(8−6) = Scratch(2) = SCRATCH_SPILL_BASE`. Unreachable under
  Trace at cap 4 (needs `n_input ≥ 9`, see B); collision pinned by comment at the spill site
  (2303–2304).
- **Canonical/unshared-ctx guard (2225–2232): untouched by the diff** — scratch guaranteed empty at
  Trace entry (occ ⊋ homes ⇒ reject), so the spill slot is genuinely zero when marshal hasn't touched it.
- **`body_occ` covers Scratch(2):** the occupancy loop (2311–2314) sets every `body_homes` entry, which
  now includes `Scratch(2)` — the zero-source displacement allocator (scan at 649,
  `(SCRATCH_SPILL_BASE..NUM_SCRATCH).rev()`) cannot grab it. Both load-bearing neighbors intact.

## D. Paging bracket net-zero: CHECKED — self-inverse for ANY spare value, no aux=0 assumption

From `with_registers` (481–545): page-in `Xchg{spare,s2}` puts leaf4 in spare and the *old spare value*
(whatever it is) in s2; the 13 walk instrs are register-only (no scratch/mem op touches s2 in the
window — verified in the emitted ROM, indices 8–20); page-out `Xchg{spare,s2}` restores spare exactly and
lands the updated leaf in s2. Net effect on spare: identity, for any value. Spare selection:
`spares.pop()` takes the highest unused register = r7 = ctrl (confirmed in the ROM: `Xchg{rd:7,sx:2}` at
7 and 21); ctrl is not read between page-in and the post-body `Xor ctrl,tag` at 22, which follows the
page-out — so even the "ctrl happens to be 0 here" fact (true: uncomputed at 5–6) is not load-bearing.
I3 holds: `body_len` measured post-lowering (2342) = 15 (13+2 Xchg), `iter_len=25`, δu immediates in the
ROM = **231 = 256−25** — the paging is in the token count.

## E. Test teeth: CHECKED — all assertions have bite

- **T1** (`mde_spill_emission`): collects *all* Xchgs ROM-wide, asserts exactly 2 (⇒ zero spine Xchgs —
  the 4 spine slots are Xori/Addi/Subi/Xori, no relocation), both strictly inside (Ltop=2, Lend=26),
  same rd/sx, `sx≥2`, `rd≥4`; body region [7,22) Swapbr-free. Observed pagings: `[(7,7,2),(21,7,2)]` on
  a 29-instr ROM — matches the design-review shape exactly.
- **reject-cap**: matches the `CompileError::Deferred` *variant* + "payload leaves" discriminator.
  `Deferred` carries only a `String`, so variant-match + message discriminator is the maximum structure
  available — 5d satisfied as-is. `n6` (5 leaves) → `n_payload=5` hits the cap guard (2210) first.
- **reject-input**: `nat_chain(5)` has 4 leaves (`num_leaves(One)=0`, confirmed via
  `value_to_leaf_bytes(One)=[]`), so `n_input+1 = 5 > 4` — genuinely forces the 2262 guard; message
  "tag+input … live-input" matched. Passes the cap (n_payload=4) and canonical guards first, so it
  cannot be rejected for the wrong reason.
- **T3**: carries `#[ignore = "authoritative real-CPU oracle (slow)"]`; runs `run_rom_on_cpu` directly
  and asserts the **full** `scratch == [0; NUM_SCRATCH]` terminal — this has teeth because
  `run_rom_on_cpu`'s own terminal check (2897) covers only BR/s0/s1, so a dirty s2 would otherwise pass
  both oracles.
- **T4** (closure): `#[ignore]`-gated; asserts `cert.validate()` on `two_body_timing_certificate` +
  run==eval^R via `system_step` over the Bit domain.

## Findings by severity

**BLOCKER** — none.

**MAJOR** — none.

**MINOR:**
1. **(2a incomplete) `docs/scratch/mde-spill-design.md` not updated** — the diff touches only
   `crates/theseus-tisc/**` (the task's allowlist), so the design doc's F1b bullet still reads
   `Xchg spare,s_k; op; Xchg spare,s_k` (per-op shape) and the r1 body §1/§3 still describe
   per-instruction paging. The *code* comment (compile.rs:1136–1139) correctly states the whole-body
   bracket. Fix at advisor finish: one paragraph in the design doc. This is the only reason for
   SHIP-WITH-FIXES rather than SHIP.
2. **(5b deviation) closure test omits `system_ring()` + `bus_quiescent(&init)`** present in the house
   convention (`dot_reduce.rs:520–554` T7 builds the ring and asserts bus-quiescence; `mde_spill.rs`
   T4 does not). The letter of condition 3 (validate()==Ok, run==eval^R, `#[ignore]`) is met, and Xchg
   is a CPU-internal reg↔scratch op (no bus leg), but for a first-of-kind back-edge-paging ROM the
   one-line `bus_quiescent` assert would cost nothing. Suggest adding when T4 is first run via
   `--ignored`.
3. **(3b partial) `home_of` itself (compile.rs:394) carries no leaf-8/SCRATCH_SPILL_BASE comment** —
   the collision is pinned at the consumer (2303–2304) but not at the producer the r1 review named.
   Comment-only; can ride the same advisor-finish commit as (1).

**NIT:**
4. T1 computes Ltop/Lend by formula (`2`, `len−3`) without anchoring `assert!(matches!(instrs[ltop],
   Instr::Swapbr{..}))` — if the spine shape ever changes, the region checks could misalign silently
   (the `debug_assert_eq!` at 2435 mitigates). One-line hardening for a future pass.
5. The B honesty note above (guard currently shadowed by the 2381 aux check) is worth a one-line code
   comment so nobody "simplifies" one of the two away.

## Merge-blocking conditions (design-review r2 §SHIP)

| # | Condition | Status | Evidence |
|---|-----------|--------|----------|
| 1 (5a) | `#[ignore]` on `_real_cpu` | **SATISFIED** | `mde_spill.rs:216` `#[ignore = "authoritative real-CPU oracle (slow)"]` |
| 2 (3a) | `n_input+1 ≤ budget` reject test (n_input=4) | **SATISFIED** | `mde_spill.rs:334` — 4-leaf `nat_chain(5)` input, hits `compile.rs:2262`, structured match |
| 3 (5b) | SystemRing closure test, `validate()==Ok`, `#[ignore]` | **SATISFIED** (minor deviation) | `mde_spill.rs:259–304`; no `bus_quiescent` assert vs dot_reduce T7 convention (MINOR 2) |
| 4 (5c) | scratch-all-zero terminal via `run_rom_on_cpu` | **SATISFIED** | `mde_spill.rs:246–251`, full `[0; NUM_SCRATCH]` compare; teeth confirmed vs `compile.rs:2897` |
| 5 (2a/3b/4a/5d) | docs/hygiene | **PARTIAL** | 4a `#[doc(hidden)]` ✓ (compile.rs:1168); 5d structured variant match ✓; 2a code-comment ✓ / design-doc ✗ (MINOR 1, allowlist-blocked); 3b consumer-site pin ✓ / `home_of`-site ✗ (MINOR 3) |

## Observed test + clippy output (independent re-run, this review)

```
$ cargo test -p theseus-tisc --release --test mde_spill
running 6 tests
test mde_spill_real_cpu ... ignored, authoritative real-CPU oracle (slow)
test mde_spill_system_ring_closure ... ignored, ring closure test (slow, OOM-sensitive); run via --ignored
test mde_spill_emission ... ok
test mde_spill_fast_oracle ... ok
test mde_spill_reject_4leaf_input ... ok
test mde_spill_reject_5_payload ... ok
test result: ok. 4 passed; 0 failed; 2 ignored; 0 measured; 0 filtered out; finished in 0.00s

$ cargo clippy -p theseus-tisc --all-targets -- -D warnings
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.04s   (no warnings)
```

Emission dump confirmed: 29-instr ROM, δu=231, paging `Xchg{r7,s2}` at indices 7 and 21 only, walk on
(t=r1, p1=r0, p2=r2, p3=r3, p4=r7-spare) — byte-for-byte the shape the design review certified on the
real CPU. Full `theseus-tisc --release` suite already proven green by the mechanical gate
(`verify_passed`); not re-run here per review scope.

**Bottom line:** the code is correct and the test scaffolding has real teeth; ship it, with the
doc-only (2a)/(3b) patches — which the worker's allowlist made impossible to include — landed by the
advisor in the finish commit, and the `bus_quiescent` one-liner queued for the first `--ignored` run of T4.
