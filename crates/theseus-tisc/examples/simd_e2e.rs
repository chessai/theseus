//! End-to-end timings of the lane-parallel paths (ADR-0052).
//!
//! Uses only APIs that predate the explicit SIMD kernels, so the same
//! source builds against the pre-ADR-0052 tree for a before/after
//! comparison. The SIMD level is picked with `THESEUS_SIMD`.
//!
//! ```text
//! cargo run --release -p theseus-tisc --example simd_e2e -- <case> [reps]
//!   ring <revolutions>   StepRing::run_many, 256 starts
//!   synth                table_to_regtransfer on [8,8,8] (2^24 rows)
//!   admit                admit on a 65536-entry Flat(8)^2 add prim
//!   verify               bveq_verify_emission of `Add` on the same prim
//!   t6                   the T6 two-operand sweep (6 x 65536 inputs)
//! ```
//!
//! Prints min and median wall time over the repetitions. With
//! `PERF_CTL_FIFO` / `PERF_ACK_FIFO` set (`perf stat --control
//! fifo:ctl,ack -D -1`), hardware counters cover only the timed loop.

use std::collections::BTreeMap;
use std::fs::{File, OpenOptions};
use std::hint::black_box;
use std::io::{BufRead, BufReader, Write};
use std::time::Instant;

use theseus_core::prim::{admit, builtin_prim, enumerate_values, PrimDef, ADD_INTO_KEY};
use theseus_core::Type;
use theseus_tisc::asm::Instr;
use theseus_tisc::bveq;
use theseus_tisc::compile::{
    bveq_verify_emission, exec_instr, value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};
use theseus_tisc::ring_run::step_ring;
use theseus_tisc::synth::table_to_regtransfer;
use theseus_tisc::types::StateVals;

/// `perf stat --control` fifos, when the environment names them.
struct PerfCtl {
    ctl: Option<File>,
    ack: Option<BufReader<File>>,
}

impl PerfCtl {
    fn from_env() -> Self {
        let ctl = std::env::var("PERF_CTL_FIFO")
            .ok()
            .map(|p| OpenOptions::new().write(true).open(p).expect("ctl fifo"));
        let ack = std::env::var("PERF_ACK_FIFO")
            .ok()
            .map(|p| BufReader::new(File::open(p).expect("ack fifo")));
        PerfCtl { ctl, ack }
    }

    fn send(&mut self, cmd: &str) {
        if let Some(ctl) = &mut self.ctl {
            writeln!(ctl, "{cmd}").expect("perf ctl");
            ctl.flush().expect("perf ctl");
            if let Some(ack) = &mut self.ack {
                let mut line = String::new();
                ack.read_line(&mut line).expect("perf ack");
            }
        }
    }
}

/// Run `f` `reps` times (after one warm-up), counters on only around the
/// timed runs; print min / median.
fn bench(name: &str, reps: usize, mut f: impl FnMut() -> u64) {
    let check = f();
    let mut perf = PerfCtl::from_env();
    let mut times = Vec::with_capacity(reps);
    perf.send("enable");
    for _ in 0..reps {
        let t = Instant::now();
        black_box(f());
        times.push(t.elapsed().as_nanos() as f64);
    }
    perf.send("disable");
    times.sort_by(f64::total_cmp);
    let level = std::env::var("THESEUS_SIMD").unwrap_or_else(|_| "auto".into());
    println!(
        "case={name} simd={level} reps={reps} min_ms={:.3} median_ms={:.3} check={check:#x}",
        times[0] / 1e6,
        times[reps / 2] / 1e6
    );
}

fn r_ins(opcode: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    (1 << 31) | (opcode << 25) | (rd << 20) | (rs << 15) | (rt << 10)
}

fn i_ins(opc: u32, rd: u32, imm: u8) -> u32 {
    (opc << 25) | (rd << 20) | u32::from(imm)
}

/// 256 start states (xorshift registers and scratch, as `tests/ring_run.rs`).
fn starts() -> Vec<StateVals> {
    let mut x = 0x2545_F491_4F6C_DD1Du64;
    let mut byte = move || {
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        x as u8
    };
    (0..256)
        .map(|i| {
            let mut s = StateVals::zero_state();
            s.pc = i % 5;
            s.regs = std::array::from_fn(|_| byte());
            s.scratch[2..].iter_mut().for_each(|b| *b = byte());
            s
        })
        .collect()
}

/// `add_into` over `Flat(8)^2` with its full 65536-entry tables attached
/// to the symbolic spec, so `admit` checks spec == table both ways.
fn add_prim_with_table() -> PrimDef {
    let ty = Type::prod(Type::Flat(8), Type::Flat(8));
    let mut def = builtin_prim(ADD_INTO_KEY, &ty, &ty).expect("add_into");
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(&v, &ty);
        let out = value_from_leaf_bytes_pub(&[b[0], b[0].wrapping_add(b[1])], &ty).0;
        forward.insert(v.clone(), out.clone());
        backward.insert(out, v);
    }
    def.forward = forward;
    def.backward = backward;
    def
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let case = args.first().map(String::as_str).unwrap_or("ring");
    let arg = |i: usize, default: usize| {
        args.get(i)
            .map(|s| s.parse().expect("number"))
            .unwrap_or(default)
    };
    match case {
        "ring" => {
            let revolutions = arg(1, 8) as u32;
            let rom = vec![i_ins(0, 1, 0x05), r_ins(2, 3, 2, 0), i_ins(1, 6, 0x11)];
            let ring = step_ring(&rom);
            let starts = starts();
            bench(&format!("ring_r{revolutions}"), arg(2, 30), || {
                let out = ring.run_many(&starts, revolutions);
                out.iter().fold(0, |h, s| {
                    s.regs
                        .iter()
                        .fold(h, |h, &r| h.rotate_left(5) ^ u64::from(r))
                })
            });
        }
        "synth" => bench("synth_888", arg(1, 3), || {
            let t = table_to_regtransfer(
                |b| vec![b[0] ^ (b[1] & b[2]), b[1], b[2]],
                &[0, 1, 2],
                &[8, 8, 8],
            );
            format!("{:?}", t.regs[0][7]).len() as u64
        }),
        "admit" => {
            let def = add_prim_with_table();
            bench("admit_add8", arg(1, 5), || u64::from(admit(&def).is_ok()));
        }
        "verify" => {
            let def = add_prim_with_table();
            let body = [Instr::Add { rd: 1, rs: 0 }];
            bench("verify_add8", arg(1, 5), || {
                u64::from(bveq_verify_emission(&body, &def, &[0, 1]).is_ok())
            });
        }
        "t6" => {
            let two_op = [
                Instr::Add { rd: 0, rs: 1 },
                Instr::Sub { rd: 0, rs: 1 },
                Instr::Xor { rd: 0, rs: 1 },
                Instr::Exch { rd: 0, rs: 1 },
                Instr::Rotl { rd: 0, rs: 1 },
                Instr::Rotr { rd: 0, rs: 1 },
            ];
            bench("t6_two_op", arg(1, 5), || {
                let mut h = 0u64;
                for instr in &two_op {
                    let transfer = bveq::instr_semantics(instr).unwrap();
                    for a in 0..=255u8 {
                        let inputs: Vec<[u8; 8]> =
                            (0..=255u8).map(|b| [a, b, 0, 0, 0, 0, 0, 0]).collect();
                        let sym = bveq::eval_transfer_lanes(&transfer, &bveq::pack_regs(&inputs));
                        for b in 0..=255u8 {
                            let sym_out = bveq::unpack_regs(&sym, b as usize);
                            let mut state = StateVals::state_with_regs(&[a, b]);
                            exec_instr(*instr, &mut state);
                            assert_eq!(sym_out, state.regs);
                            h = h.rotate_left(3) ^ u64::from(sym_out[0]);
                        }
                    }
                }
                h
            });
        }
        other => panic!("unknown case {other:?}"),
    }
}
