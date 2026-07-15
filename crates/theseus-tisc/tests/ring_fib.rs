//! M4-v2 HEADLINE DEMO: the TISC-1 CPU computes Fibonacci by
//! RECIRCULATING one compact-balanced step — not by unrolling.
//!
//! The same reversible Fibonacci ROM from `fib_cpu.rs` (N = 10 pairs,
//! 20 words: `ADD r1,r2 ; EXCH r1,r2` × 10, seed r1 = r2 = 1) is
//! assembled, lowered, and compact-balanced ONCE into a [`StepRing`].
//! The machine then runs by recirculation: `StepRing::run(&seed, R)`
//! for R = 0, 2, 4, …, 20 revolutions.
//!
//! Verified:
//!   1. After 2k revolutions the registers hold (fib(k), fib(k+1))
//!      mod 256, matching both a Rust reference sequence AND the
//!      eval-iterate oracle (`eval(step)^{2k}`).
//!   2. The ring's cell inventory is IDENTICAL for every R — one body
//!      of silicon computes all 20 cycles.
//!   3. Prints the Fibonacci sequence and a cells-per-body vs
//!      cells-if-unrolled headline.

use std::sync::OnceLock;

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::ring_run::{step_ring, StepRing};
use theseus_tisc::step::step;
use theseus_tisc::types::{MEM_WORDS, StateVals};

/// Generous per-eval fuel.
const EVAL_FUEL: u64 = 8_000_000_000;

/// Fibonacci pairs unrolled in the ROM (T = 2N machine cycles).
const N: usize = 10;

// ── Fibonacci program (identical to fib_cpu.rs) ───────────────────

fn fib_prog() -> Vec<Instr> {
    let mut prog = Vec::with_capacity(2 * N);
    for _ in 0..N {
        prog.push(Instr::Add { rd: 1, rs: 2 });
        prog.push(Instr::Exch { rd: 1, rs: 2 });
    }
    prog
}

fn seed() -> StateVals {
    let mut s = StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    s.regs[1] = 1;
    s.regs[2] = 1;
    s
}

/// Reference: consecutive-Fibonacci pairs mod 256 starting from (1, 1).
/// `fib_pairs()[k] = (fib(k), fib(k+1))` with fib(0) = fib(1) = 1.
fn fib_pairs() -> Vec<(u8, u8)> {
    let mut pairs = vec![(1u8, 1u8)];
    for k in 0..N {
        let (a, b) = pairs[k];
        pairs.push((b, a.wrapping_add(b)));
    }
    pairs
}

// ── Eval-iterate oracle ───────────────────────────────────────────

fn eval_state(name: &str, iso: &theseus_core::Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

fn eval_iterate(iso: &theseus_core::Iso, start: &StateVals, t: usize) -> StateVals {
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("eval_iterate[{k}]"), iso, &cur);
    }
    cur
}

// ── Cached ring + iso ─────────────────────────────────────────────

struct Ctx {
    ring: StepRing,
    iso: theseus_core::Iso,
    rom: Vec<u16>,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = assemble(&fib_prog());
        let ring = step_ring(&rom);
        let iso = step(&rom);
        Ctx { ring, iso, rom }
    })
}

// ══════════════════════════════════════════════════════════════════════
// 1. Recirculating Fibonacci: ring == eval^R == reference fib
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn ring_fib_matches_reference_and_oracle() {
    let c = ctx();
    let start = seed();
    let pairs = fib_pairs();

    eprintln!("== RING-FIB DEMO: recirculating CPU computes Fibonacci ==");
    eprintln!("ROM: {} words, N = {N} pairs", c.rom.len());
    eprintln!();

    let mut fib_seq = vec![start.regs[1]];

    for (k, &(ref_r1, ref_r2)) in pairs.iter().enumerate().take(N + 1) {
        let r = 2 * k; // revolutions = 2k (one fib pair = 2 machine cycles)

        // Ring recirculation result.
        let ring_result = c.ring.run(&start, r as u32);

        // Eval-iterate oracle: eval(step)^{2k}.
        let oracle = eval_iterate(&c.iso, &start, r);

        eprintln!(
            "  R={r:2}: ring (r1,r2) = ({:3},{:3})  oracle (r1,r2) = ({:3},{:3})  ref = ({:3},{:3})",
            ring_result.regs[1], ring_result.regs[2],
            oracle.regs[1], oracle.regs[2],
            ref_r1, ref_r2,
        );

        // Assert ring matches oracle.
        assert_eq!(
            ring_result, oracle,
            "ring.run(seed, {r}) != eval(step)^{r}"
        );

        // Assert registers match reference Fibonacci.
        assert_eq!(
            (ring_result.regs[1], ring_result.regs[2]),
            (ref_r1, ref_r2),
            "R={r}: registers diverged from reference Fibonacci pair {k}"
        );

        if k > 0 {
            fib_seq.push(ring_result.regs[1]);
        }
    }

    // Append final r2 to complete the sequence.
    let final_result = c.ring.run(&start, (2 * N) as u32);
    fib_seq.push(final_result.regs[2]);

    assert_eq!(
        (final_result.regs[1], final_result.regs[2]),
        (89, 144),
        "final regs must be (fib(10), fib(11)) = (89, 144)"
    );

    eprintln!();
    eprintln!("Fibonacci sequence computed by the recirculating CPU: {fib_seq:?}");
    assert_eq!(fib_seq, vec![1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]);
}

// ══════════════════════════════════════════════════════════════════════
// 2. Constant cell inventory across all revolutions
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn ring_fib_constant_area() {
    let c = ctx();
    let start = seed();
    let counts = c.ring.compact().cell_counts();
    let k0 = c.ring.compact().k0_buffer_count();

    eprintln!("== RING-FIB AREA: one body serves all revolutions ==");
    eprintln!("cell counts: {counts:?}");
    eprintln!("k0 buffer count: {k0}");

    for k in 0..=N {
        let r = (2 * k) as u32;
        let _ = c.ring.run(&start, r);
        assert_eq!(
            c.ring.compact().cell_counts(),
            counts,
            "cell_counts changed after R={r}"
        );
        assert_eq!(
            c.ring.compact().k0_buffer_count(),
            k0,
            "k0_buffer_count changed after R={r}"
        );
    }

    // Headline: cells-per-body vs cells-if-unrolled.
    let total_cells: usize = counts.values().sum();
    let total_with_k0 = total_cells + k0;
    let unrolled_cycles = 2 * N;
    let unrolled_cells = total_with_k0 * unrolled_cycles;

    eprintln!();
    eprintln!(
        "HEADLINE: recirculating body = {} cells (+ {} k0 buffers = {} total), \
         unrolled {}-step machine would be {} cells — {:.0}x area saving",
        total_cells,
        k0,
        total_with_k0,
        unrolled_cycles,
        unrolled_cells,
        unrolled_cycles as f64,
    );
}
