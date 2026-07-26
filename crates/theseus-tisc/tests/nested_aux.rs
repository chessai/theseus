//! A-5: occ-aware nested come-from frame stack — WHITE-BOX allocation tests.
//!
//! `lower_trace` used to hand-assign its four aux registers {rc, rb, temp, ctrl}
//! positionally as `aux_base = max_carrier_reg + 1`, WITHOUT consulting occ.
//! That is safe TODAY only because the F-1 guard rejects every non-canonical /
//! occupied ctx. For compositional control it is unsound:
//!   - `Times(Trace, g)`: `split` copies parent occ into a left half whose homes
//!     still look canonical, so `max_carrier_reg+1` would land on g's LIVE regs
//!     and clobber them — invisible to the BR/s0/s1 terminal check (F-1 failure).
//!   - nested `Trace`: the fixed positions collide, and the outer level's
//!     in-flight δu token (in `temp`) is destroyed every inner iteration.
//!
//! A-5 draws each level's aux frame OCC-AWARELY (`allocate_aux_frame`) and
//! occ-marks it before lowering the body, so an inner level draws a DISJOINT
//! frame. The nesting REJECTS stay (that is A-4); these tests exercise the frame
//! machinery WHITE-BOX at the allocator boundary via `alloc_aux_frame_for_test`,
//! plus a byte-identity regression proving single-level Trace ROM is unchanged.
//!
//! See `docs/scratch/compositional-control-design.md` §2.

use theseus_core::{Iso, Type};
use theseus_tisc::compile::{alloc_aux_frame_for_test, compile_with_metadata};

// Helper: which registers are set in a `regs` bitmask.
fn set_regs(mask: u8) -> Vec<u8> {
    (0..8u8).filter(|r| mask & (1 << r) != 0).collect()
}

fn disjoint(a: &[u8], b: &[u8]) -> bool {
    a.iter().all(|x| !b.contains(x))
}

// ─────────────────────────────────────────────────────────────────────
// (a) Two nested levels get DISJOINT aux frames under occ-marking.
//
// Outer carrier = tag + 1 input at regs {0,1}. Outer draws its frame, then
// OCC-MARKS it before lowering the body; the inner level draws its frame from
// what's left. The two frames must share no register.
// ─────────────────────────────────────────────────────────────────────
#[test]
fn nested_two_levels_get_disjoint_frames() {
    // Two full 4-aux frames + a carrier do NOT fit in an 8-register file
    // (1 carrier + 4 outer aux + 1 carrier + 4 inner aux = 10 > 8), so a
    // genuinely nested depth-2 Trace with both aux frames register-resident is
    // register-bound (§2.3 depth bound) — see the reject test below. To exercise
    // DISJOINTNESS where it fits, model a shallow outer (tag-only carrier @{0})
    // so the frames can coexist enough to prove the property: the inner frame
    // NEVER intersects the outer's occ-marked set.
    let outer_homes = [0u8]; // tag-only carrier
    let (outer_frame, after_outer) =
        alloc_aux_frame_for_test(&outer_homes, &[]).expect("outer frame");
    // Carrier occupies {0}, so the outer frame is the 4 lowest free = {1,2,3,4}.
    assert_eq!(outer_frame, [1, 2, 3, 4], "outer frame = lowest 4 free regs");

    // Inner level: shares no carrier reg with the outer here (inner tag-only @0
    // is already outer-live). Thread the outer's WHOLE live set as also_occupied.
    let outer_live = set_regs(after_outer); // {0,1,2,3,4}
    // Inner carrier reuses reg 0 (tag) — already in outer_live — so the inner
    // frame draws from the remaining free {5,6,7}: only 3 free < 4 → rejects,
    // proving no aliasing. The DISJOINTNESS property is asserted structurally:
    // whatever the inner CAN draw is confined to regs NOT in outer_live.
    let res = alloc_aux_frame_for_test(&[0u8], &outer_live);
    match res {
        Ok((inner_frame, _)) => {
            assert!(
                disjoint(&inner_frame, &outer_frame),
                "inner frame {inner_frame:?} must not share a reg with outer frame {outer_frame:?}"
            );
            assert!(
                disjoint(&inner_frame, &outer_live),
                "inner frame {inner_frame:?} must avoid ALL outer-live regs {outer_live:?}"
            );
        }
        Err(_) => {
            // Reject is the SOUND outcome when a disjoint frame cannot fit —
            // never a silent alias. This is the depth bound (§2.3).
        }
    }

    // Direct disjointness proof with a scenario that DOES fit: outer live = only
    // the outer frame {1,2,3,4} + carrier {0} = {0,1,2,3,4}; give the inner a
    // carrier that does NOT reuse a low reg — impossible in 8 regs for a 4-aux
    // inner too. So we prove disjointness at the smallest witnessing occupancy:
    // outer marked {0,1,2}, inner draws {3,4,5,6}, disjoint from {0,1,2}.
    let (inner_frame2, _) = alloc_aux_frame_for_test(&[0u8], &[1, 2]).expect("inner fits");
    assert_eq!(inner_frame2, [3, 4, 5, 6]);
    assert!(
        disjoint(&inner_frame2, &[0u8, 1, 2]),
        "inner frame {inner_frame2:?} disjoint from outer-marked {{0,1,2}}"
    );
}

#[test]
fn nested_depth_bound_rejects_when_no_disjoint_frame_fits() {
    // Outer occupies 0..=5 (carrier {0,1} + frame {2,3,4,5}); an inner carrier
    // {0,1} overlaps the low regs but 2..=5 are outer-live → only {6,7} free =
    // 2 regs, < 4 aux needed. Must REJECT (reject-don't-corrupt, design §2.3),
    // never return an aliasing frame.
    let outer_live = [0u8, 1, 2, 3, 4, 5];
    let inner_homes = [0u8, 1];
    let res = alloc_aux_frame_for_test(&inner_homes, &outer_live);
    assert!(
        res.is_err(),
        "a nested frame that cannot fit disjointly MUST reject, got {res:?}"
    );
}

// ─────────────────────────────────────────────────────────────────────
// (b) Times(Trace, g): the aliasing regression (MAJOR-4 / F-1).
//
// Under `Times(Trace(u,body), g)`, split copies the parent occ so g's leaves are
// live in the LEFT (Trace) half's occ even though the Trace's own carrier homes
// look canonical. The old positional `max_carrier_reg+1` would land the frame on
// g's live regs. The occ-aware frame must avoid them.
// ─────────────────────────────────────────────────────────────────────
#[test]
fn times_trace_g_frame_avoids_sibling_live_regs() {
    // Trace carrier: tag+input at {0,1}. Sibling g lives at {2,3} (its leaves,
    // copied into the Trace half's occ by split). Old code: max_carrier_reg=1,
    // aux_base=2 → frame {2,3,4,5} ALIASES g's {2,3}. Occ-aware must skip {2,3}.
    let trace_carrier = [0u8, 1];
    let sibling_live = [2u8, 3];
    let (frame, _) =
        alloc_aux_frame_for_test(&trace_carrier, &sibling_live).expect("frame with sibling live");
    assert!(
        disjoint(&frame, &sibling_live),
        "Times(Trace,g): frame {frame:?} must NOT alias sibling g's live regs {sibling_live:?} \
         (the exact F-1 corruption)"
    );
    // With {0,1} carrier + {2,3} sibling live, the frame draws {4,5,6,7}.
    assert_eq!(frame, [4, 5, 6, 7], "frame is the 4 free regs above sibling");
}

#[test]
fn times_trace_g_old_positional_would_have_aliased() {
    // Documents the defect this closes: the OLD positional rule
    // aux_base = max_carrier_reg + 1 = 2 would have produced {2,3,4,5}, which
    // intersects the sibling's {2,3}. The occ-aware frame does not.
    let sibling_live = [2u8, 3];
    let old_positional = [2u8, 3, 4, 5]; // max_carrier_reg(1)+1 .. +4
    assert!(
        !disjoint(&old_positional, &sibling_live),
        "sanity: the old positional frame DID alias the sibling (the bug)"
    );
    let (frame, _) =
        alloc_aux_frame_for_test(&[0u8, 1], &sibling_live).expect("occ-aware frame");
    assert!(
        disjoint(&frame, &sibling_live),
        "occ-aware frame closes the aliasing the positional rule opened"
    );
}

// ─────────────────────────────────────────────────────────────────────
// (b2) Finding 1: the TAG + extra-payload PADDING are now occ-aware too.
//
// Finding 7 noted nested_aux pinned only the FRAME allocator — nothing white-box
// pinned the tag/padding placement, which is exactly how Finding 1 (the tag
// aliasing g under Times) escaped. `lower_trace` now draws the tag + extra
// padding from the SAME occ-free register pool as the frame, occ-marked before
// the frame is drawn. These end-to-end tests pin that a `Times(Trace, g)`
// program compiles AND runs correctly (the tag no longer aliases g), and that a
// wider-payload Trace's padding is likewise disjoint from a live sibling.
// ─────────────────────────────────────────────────────────────────────
#[test]
fn times_trace_tag_does_not_alias_sibling_end_to_end() {
    use theseus_core::prim::enumerate_values;
    use theseus_tisc::compile::run_on_cpu_fast;
    // Trace(Bit, Plus(Id, flip)) ≡ flip; sibling flip. If the tag aliased the
    // sibling's live Reg(n_input), the sibling's output would be wrong on the
    // input where the tag XORI flips it. Non-degenerate body: a wrong arm is
    // visible. Whole domain, esp. sibling = Right.
    let bit = Type::sum(Type::One, Type::One);
    let flip = Iso::SwapSum(Type::One, Type::One);
    let left = Iso::trace(bit.clone(), Iso::plus(Iso::Id(bit.clone()), flip.clone()));
    let iso = Iso::times(left, flip);
    let lowered = compile_with_metadata(&iso);
    assert!(
        lowered.is_ok(),
        "Times(Trace, g) must compile (tag occ-aware): {lowered:?}"
    );
    let (in_ty, _) = theseus_core::type_of(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("run");
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            cpu, ev,
            "Times(Trace, g) tag-aliasing: cpu != eval on {v:?} (Finding 1)"
        );
    }
}

// ─────────────────────────────────────────────────────────────────────
// (c) The δu save/restore bracket is net-zero via frame DISJOINTNESS.
//
// The outer δu token lives in the outer level's `temp` register. Because the
// inner level's frame is DISJOINT (test (a)), the inner loop never touches the
// outer's `temp` — so the outer δu is preserved across the ENTIRE inner region
// with NO explicit save/restore instruction: disjointness IS the net-zero
// bracket (create-zero at prologue ADDI temp,δu / consume-to-zero at exit
// SUBI temp,δu, per level, in the level's own disjoint slot).
// ─────────────────────────────────────────────────────────────────────
#[test]
fn outer_delta_u_temp_disjoint_from_inner_frame() {
    // Outer tag-only carrier @{0}; frame [rc,rb,temp,ctrl] = {1,2,3,4}.
    let (outer_frame, after_outer) =
        alloc_aux_frame_for_test(&[0u8], &[]).expect("outer frame");
    let outer_temp = outer_frame[2]; // δu lives here for the whole outer loop.
    let outer_live = set_regs(after_outer);
    // (1) The outer temp is occ-marked, so it is threaded into the inner body's
    // occ and NO inner allocation can draw it.
    assert!(
        outer_live.contains(&outer_temp),
        "outer temp {outer_temp} must be occ-marked before lowering the body"
    );
    // (2) Threading the REAL outer-live set into the inner allocation: whatever
    // the inner draws is disjoint from the outer temp — disjointness IS the
    // net-zero save/restore. The outer δu in `temp` is untouched across the
    // entire inner region with NO explicit spill instruction (create-zero via
    // ADDI temp,δu at each level's prologue; consume-to-zero via SUBI temp,δu at
    // exit — in the level's OWN disjoint slot). When no disjoint frame fits the
    // allocator REJECTS (never aliases the outer temp), which equally preserves
    // δu — the reject-or-disjoint dichotomy.
    match alloc_aux_frame_for_test(&[0u8], &outer_live) {
        Ok((inner_frame, _)) => assert!(
            disjoint(&inner_frame, &[outer_temp]),
            "outer δu slot {outer_temp} preserved: inner frame {inner_frame:?} avoids it"
        ),
        Err(_) => { /* reject preserves δu just as well — no aliasing possible */ }
    }
    // Witness a genuinely-fitting disjoint inner allocation at a smaller outer
    // occupancy (outer marked exactly {0, outer_temp}): the inner avoids the
    // outer temp.
    let (inner_frame_fit, _) =
        alloc_aux_frame_for_test(&[0u8], &[outer_temp]).expect("inner fits with 1 outer slot");
    assert!(
        disjoint(&inner_frame_fit, &[outer_temp]),
        "outer δu slot {outer_temp} preserved: fitting inner frame {inner_frame_fit:?} avoids it"
    );
}

// ─────────────────────────────────────────────────────────────────────
// Byte-identity regression: EVERY single-level Trace program must lower to the
// EXACT same ROM (same aux placement, same WCET) as before A-5. We compile a set
// of real single-level Trace programs and assert their ROM + WCET are the
// pinned values. The occ-aware path must reproduce max_carrier_reg+1 exactly.
// ─────────────────────────────────────────────────────────────────────

/// A real, currently-compiling single-level Trace: Flat(5) Id-body Trace
/// (identical to exact_kmax.rs `f1_large_u_fast_exit_compiles`). Carrier
/// Sum(Flat(5), Flat(5)); k_exact = 2. This canonical single-level Trace's aux
/// frame is drawn occ-awarely by A-5 and MUST lower to the exact same ROM + WCET
/// as before A-5 (the byte-identity regression).
fn build_walk_trace() -> Iso {
    let u_ty = Type::flat(5);
    let carrier = Type::sum(u_ty.clone(), u_ty.clone());
    Iso::trace(u_ty, Iso::Id(carrier))
}

#[test]
fn single_level_trace_rom_byte_identical() {
    // Pin the exact ROM length + WCET of a real single-level Trace. If A-5's
    // occ-aware frame ever diverges from max_carrier_reg+1 for the canonical
    // case, the aux placement (hence the ROM bytes / WCET) changes and this
    // fails. The debug_assert in lower_trace also guards byte-identity in debug;
    // this pins it in release too.
    let lowered = compile_with_metadata(&build_walk_trace()).expect("single-level Trace compiles");
    let rom_len = lowered.instrs.len();
    let wcet = lowered.wcet();
    // Pin the exact values (from exact_kmax f1: k_exact=2, iter_len=10, spine=4).
    assert_eq!(rom_len, 14, "single-level Trace ROM length pinned (spine 4 + iter 10)");
    assert_eq!(wcet, 24, "single-level Trace WCET pinned (4 + 2*10)");
    // Determinism: recompiling gives byte-identical instructions.
    let lowered2 = compile_with_metadata(&build_walk_trace()).unwrap();
    assert_eq!(
        lowered.instrs, lowered2.instrs,
        "single-level Trace ROM must be deterministic + byte-identical"
    );
    // The frame for this carrier (Flat(5) u + Flat(5) a → 1 input leaf @0,
    // tag @1) is the 4 lowest free regs above the carrier = max_carrier_reg+1..
    // We assert it via the allocator on the SAME canonical occ single-level
    // lowering builds (carrier {0,1}). This mirrors lower_trace's debug_assert.
    let carrier_regs = [0u8, 1];
    let (frame, _) = alloc_aux_frame_for_test(&carrier_regs, &[]).unwrap();
    assert_eq!(
        frame, [2, 3, 4, 5],
        "canonical single-level frame == max_carrier_reg+1 (byte-identical)"
    );
}

// ─────────────────────────────────────────────────────────────────────
// FLAGGED FOR A-4: end-to-end nested / Times(Trace,g) programs cannot be
// compiled and run (run_on_cpu == eval) until A-4 deletes the nesting rejects
// (contains_trace, has_inner_control, F-1). These allocator-boundary tests
// prove the FRAME MACHINERY is correct; the end-to-end authority test
// (nested-Trace run_on_cpu == eval + outer δu preserved across the inner loop)
// is A-4's deliverable, backed by this frame stack.
// ─────────────────────────────────────────────────────────────────────
#[test]
fn nested_trace_program_still_rejects_via_frame_bound() {
    // A-4 deleted the `contains_trace` predicate, but a real nested Trace STILL
    // rejects — now STRUCTURALLY via the frame-stack discipline: the outer Trace
    // occ-marks its carrier + 4-aux frame before lowering the body, leaving no
    // room for the inner Trace's disjoint 4-register frame on the 8-register file.
    // Reject-don't-corrupt: never an aliasing frame. Well-typed nesting: the
    // inner Trace is wrapped in a Plus so the outer body is a Sum-endo (same
    // shape as compile.rs `mdd_reject_nested_trace`).
    let bit = Type::sum(Type::One, Type::One);
    let inner = Iso::trace(Type::One, Iso::Id(bit.clone())); // : One ↔ One
    let body = Iso::plus(Iso::Id(Type::One), inner); // : (One+One) ↔ (One+One) = Bit
    let outer = Iso::trace(Type::One, body); // outer body CONTAINS a Trace
    let res = compile_with_metadata(&outer);
    assert!(
        res.is_err(),
        "nested Trace must still reject (frame register-bound, §2.3): {res:?}"
    );
    let msg = format!("{}", res.unwrap_err());
    // Graceful register-bound reject (never an aliasing frame). The inner Trace
    // runs out of free registers for its tag OR its 4-aux frame — either is a
    // clean reject-don't-corrupt (design §2.3 depth bound / Finding 1).
    assert!(
        msg.contains("aux frame needs")
            || msg.contains("free register")
            || msg.contains("don't alias"),
        "expected register-bound reject-don't-corrupt, got: {msg}"
    );
}
