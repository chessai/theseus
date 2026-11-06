//! Kernel-level benchmarks for the explicit SIMD kernels (ADR-0052):
//! each kernel's scalar reference against its SSE2 and AVX2 flavours.
//!
//! ```text
//! cargo run --release -p theseus-tisc --example simd_kernels -- <group> [variant] [param]
//!   compact <N> [kernel]   StepRing compact sim, one pass; kernel =
//!                          reference | portable | sse2 | avx2
//!   mobius <n> [level]     packed Möbius on a 2^n-row table
//!   bvprog [variant]       Add / Cswap / 4-instruction transfers on 256
//!                          lanes: recursive | scalar | sse2 | avx2
//!   pack [variant]         256 register files -> planes and back, and the
//!                          ring carrier (686 bools x 256 lanes) both ways
//!   all                    everything (the timing tables)
//! ```
//!
//! Each line is `group variant ... ns_per_op=<min> median=<median>`. With
//! `PERF_CTL_FIFO` / `PERF_ACK_FIFO` set, hardware counters cover only the
//! timed loops; `BENCH_ONLY=<substring>` restricts the run (and the counts)
//! to the labels containing it.

use std::fs::{File, OpenOptions};
use std::hint::black_box;
use std::io::{BufRead, BufReader, Write};
use std::time::Instant;

use theseus_core::simd::{self, SimdLevel};
use theseus_netlist::sim_lanes::pack_lanes_scalar;
use theseus_netlist::{
    pack_lanes, unpack_lane, unpack_lanes, CompactBalanced, CompactKernel, LaneScratch, Lanes,
};
use theseus_tisc::asm::Instr;
use theseus_tisc::bveq::{self, RegTransfer, BITS, LANES};
use theseus_tisc::ring_run::step_ring;
use theseus_tisc::types::{StateVals, NUM_REGS};

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

/// Time `inner` calls of `f` per sample, `samples` samples (min-of-N),
/// after a warm-up sample; print ns per call. `perf` counters (if
/// attached) see exactly the timed samples: `samples * inner` calls.
fn bench(
    label: &str,
    samples: usize,
    inner: usize,
    perf: &mut PerfCtl,
    mut f: impl FnMut() -> u64,
) {
    // `BENCH_ONLY=<substring>` runs (and counts) only the matching labels.
    if std::env::var("BENCH_ONLY").is_ok_and(|only| !label.contains(&only)) {
        return;
    }
    for _ in 0..inner {
        black_box(f());
    }
    let mut times = Vec::with_capacity(samples);
    perf.send("enable");
    for _ in 0..samples {
        let t = Instant::now();
        for _ in 0..inner {
            black_box(f());
        }
        times.push(t.elapsed().as_nanos() as f64 / inner as f64);
    }
    perf.send("disable");
    times.sort_by(f64::total_cmp);
    println!(
        "{label:<44} ns_per_op={:>12.1} median={:>12.1} calls={}",
        times[0],
        times[samples / 2],
        samples * inner
    );
}

fn r_ins(opcode: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    (1 << 31) | (opcode << 25) | (rd << 20) | (rs << 15) | (rt << 10)
}

fn i_ins(opc: u32, rd: u32, imm: u8) -> u32 {
    (opc << 25) | (rd << 20) | u32::from(imm)
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
}

fn level_of(s: &str) -> SimdLevel {
    SimdLevel::parse(s).unwrap_or_else(|| panic!("level {s:?}"))
}

/// Does `variant` (or "all") select `name`?
fn wants(variant: Option<&str>, name: &str) -> bool {
    variant.is_none_or(|v| v == name)
}

fn compact(n: usize, variant: Option<&str>, perf: &mut PerfCtl) {
    let rom = vec![i_ins(0, 1, 0x05), r_ins(2, 3, 2, 0), i_ins(1, 6, 0x11)];
    let ring = step_ring(&rom);
    let c = ring.compact();
    let cells = c.computing_cell_count();
    eprintln!(
        "step netlist: {cells} computing cells, {} inputs",
        c.net().inputs.len()
    );
    match n {
        2 => compact_n::<2>(c, variant, perf),
        4 => compact_n::<4>(c, variant, perf),
        8 => compact_n::<8>(c, variant, perf),
        _ => panic!("N in 2, 4, 8"),
    }
}

fn compact_n<const N: usize>(c: &CompactBalanced, variant: Option<&str>, perf: &mut PerfCtl) {
    let mut rng = Rng(0xC0FF);
    let inputs: Vec<Lanes<N>> = (0..c.net().inputs.len())
        .map(|_| Lanes(std::array::from_fn(|_| rng.next())))
        .collect();
    let mut want = None;
    let kernels = [
        ("reference", CompactKernel::Reference),
        ("portable", CompactKernel::UniformPortable),
        ("sse2", CompactKernel::Simd(SimdLevel::Sse2)),
        ("avx2", CompactKernel::Simd(SimdLevel::Avx2)),
    ];
    for (name, kernel) in kernels {
        if !wants(variant, name) {
            continue;
        }
        let mut scratch = LaneScratch::default();
        let out = c
            .simulate_lanes_kernel(&inputs, &mut scratch, kernel)
            .unwrap();
        assert_eq!(*want.get_or_insert_with(|| out.clone()), out, "{name}");
        bench(
            &format!("compact N={N} ({} lanes) {name}", 64 * N),
            30,
            200,
            perf,
            || {
                let out = c
                    .simulate_lanes_kernel(&inputs, &mut scratch, kernel)
                    .unwrap();
                out[0].0[0]
            },
        );
    }
}

fn mobius(n: usize, variant: Option<&str>, perf: &mut PerfCtl) {
    let mut rng = Rng(0x0B1);
    let table: Vec<u64> = (0..(1usize << n).div_ceil(64))
        .map(|_| rng.next())
        .collect();
    let inner = (1usize << 22 >> n).max(1);
    let mut want = None;
    for level in SimdLevel::available() {
        if !wants(variant, level.name()) {
            continue;
        }
        let mut c = table.clone();
        simd::mobius_transform_with(level, &mut c, n);
        assert_eq!(*want.get_or_insert_with(|| c.clone()), c, "{level}");
        bench(&format!("mobius n={n} {level}"), 15, inner, perf, || {
            simd::mobius_transform_with(level, &mut c, n);
            c[c.len() - 1]
        });
    }
}

fn bvprog(variant: Option<&str>, perf: &mut PerfCtl) {
    let mut rng = Rng(0xB5E9);
    let inputs: Vec<[u8; NUM_REGS]> = (0..LANES)
        .map(|_| std::array::from_fn(|_| rng.next() as u8))
        .collect();
    let vars = bveq::pack_regs(&inputs);
    let seq = [
        Instr::Add { rd: 0, rs: 1 },
        Instr::Cswap {
            rd: 2,
            rs: 3,
            rt: 4,
        },
        Instr::Rotl { rd: 5, rs: 6 },
        Instr::Sub { rd: 7, rs: 0 },
    ];
    let seq16: Vec<Instr> = (0..4).flat_map(|_| seq).collect();
    let transfers: [(&str, RegTransfer); 4] = [
        ("add", bveq::symbolic_exec(&seq[..1]).unwrap()),
        ("cswap", bveq::symbolic_exec(&seq[1..2]).unwrap()),
        ("seq4", bveq::symbolic_exec(&seq).unwrap()),
        ("seq16", bveq::symbolic_exec(&seq16).unwrap()),
    ];
    for (tname, transfer) in &transfers {
        // seq16's trees are exponential (milliseconds to walk or to
        // compile): fewer calls for the per-tree work.
        let tree_calls = if *tname == "seq16" { 20 } else { 2000 };
        let program = bveq::transfer_program(transfer);
        let want = bveq::eval_transfer_lanes_recursive(transfer, &vars);
        eprintln!("{tname}: {} program instructions", program.len());
        if wants(variant, "recursive") {
            bench(
                &format!("bvprog {tname} recursive"),
                30,
                tree_calls,
                perf,
                || bveq::eval_transfer_lanes_recursive(transfer, &vars)[0][0][0],
            );
        }
        for level in SimdLevel::available() {
            if !wants(variant, level.name()) {
                continue;
            }
            let mut out = [[[0u64; 4]; BITS]; NUM_REGS];
            program.eval_with(level, &vars, out.as_flattened_mut());
            assert_eq!(out, want, "{tname} {level}");
            bench(
                &format!("bvprog {tname} program {level}"),
                30,
                2000,
                perf,
                || {
                    program.eval_with(level, &vars, out.as_flattened_mut());
                    out[0][0][0]
                },
            );
        }
        if wants(variant, "compile") {
            bench(
                &format!("bvprog {tname} compile"),
                30,
                tree_calls,
                perf,
                || bveq::transfer_program(transfer).len() as u64,
            );
        }
    }
}

fn pack(variant: Option<&str>, perf: &mut PerfCtl) {
    let mut rng = Rng(0x9AC4);
    let regs: Vec<[u8; NUM_REGS]> = (0..LANES)
        .map(|_| std::array::from_fn(|_| rng.next() as u8))
        .collect();
    let planes = bveq::pack_regs(&regs);
    // The ring carrier: 256 encoded CPU states.
    let states: Vec<Vec<bool>> = (0..256)
        .map(|_| {
            let mut s = StateVals::zero_state();
            s.regs = std::array::from_fn(|_| rng.next() as u8);
            s.scratch = std::array::from_fn(|_| rng.next() as u8);
            s.encode_bits()
        })
        .collect();
    let words = pack_lanes::<4, _>(&states);
    eprintln!("ring carrier: {} bits per state", states[0].len());

    if wants(variant, "scalar-ref") {
        bench("pack_regs per-lane reference", 30, 20000, perf, || {
            bveq::pack_regs_scalar(&regs)[7][7][3]
        });
        bench("unpack_regs per-lane reference", 30, 20000, perf, || {
            (0..LANES).fold(0, |h, i| h ^ u64::from(bveq::unpack_regs(&planes, i)[3]))
        });
        bench(
            "carrier pack_lanes per-bit reference",
            30,
            200,
            perf,
            || pack_lanes_scalar::<4, _>(&states)[100].0[1],
        );
        bench("carrier unpack per-lane reference", 30, 200, perf, || {
            (0..256).fold(0, |h, l| h ^ unpack_lane(&words, l).len() as u64)
        });
    }
    for level in SimdLevel::available() {
        if !wants(variant, level.name()) {
            continue;
        }
        simd::set_level(Some(level));
        assert_eq!(bveq::pack_regs(&regs), bveq::pack_regs_scalar(&regs));
        assert_eq!(pack_lanes::<4, _>(&states), words);
        assert_eq!(unpack_lanes(&words, 256), states);
        bench(&format!("pack_regs {level}"), 30, 20000, perf, || {
            bveq::pack_regs(&regs)[7][7][3]
        });
        bench(&format!("unpack_regs_all {level}"), 30, 20000, perf, || {
            u64::from(bveq::unpack_regs_all(&planes, LANES)[200][3])
        });
        bench(
            &format!("carrier pack_lanes {level}"),
            30,
            200,
            perf,
            || pack_lanes::<4, _>(&states)[100].0[1],
        );
        bench(
            &format!("carrier unpack_lanes {level}"),
            30,
            200,
            perf,
            || unpack_lanes(&words, 256).len() as u64,
        );
        // The raw kernel on one 4 KiB sweep chunk (64 blocks).
        let bytes: Vec<u8> = (0..4096).map(|_| rng.next() as u8).collect();
        let mut dst = vec![0u64; 8 * 64];
        bench(
            &format!("bytes_to_planes 4KiB {level}"),
            30,
            20000,
            perf,
            || {
                simd::bytes_to_planes_with(level, &bytes, &mut dst, 64, 8);
                dst[100]
            },
        );
        let mut back = vec![0u8; 4096];
        bench(
            &format!("planes_to_bytes 4KiB {level}"),
            30,
            20000,
            perf,
            || {
                simd::planes_to_bytes_with(level, &dst, 64, 8, &mut back);
                u64::from(back[100])
            },
        );
    }
    simd::set_level(None);
    #[cfg(target_arch = "x86_64")]
    if wants(variant, "bmi2") && simd::bmi2_available() {
        let bytes: Vec<u8> = (0..4096).map(|_| rng.next() as u8).collect();
        let mut dst = vec![0u64; 8 * 64];
        bench("bytes_to_planes 4KiB bmi2-pext", 30, 20000, perf, || {
            simd::bytes_to_planes_bmi2(&bytes, &mut dst, 64, 8);
            dst[100]
        });
        let mut back = vec![0u8; 4096];
        bench("planes_to_bytes 4KiB bmi2-pdep", 30, 20000, perf, || {
            simd::planes_to_bytes_bmi2(&dst, 64, 8, &mut back);
            u64::from(back[100])
        });
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let group = args.first().map(String::as_str).unwrap_or("all");
    let mut perf = PerfCtl::from_env();
    eprintln!("detected SIMD level: {}", SimdLevel::detected());
    match group {
        "compact" => {
            let n = args.get(1).map_or(4, |s| s.parse().expect("N"));
            compact(n, args.get(2).map(String::as_str), &mut perf);
        }
        "mobius" => {
            let n = args.get(1).map_or(24, |s| s.parse().expect("n"));
            let v = args.get(2).map(String::as_str);
            if let Some(v) = v {
                level_of(v);
            }
            mobius(n, v, &mut perf);
        }
        "bvprog" => bvprog(args.get(1).map(String::as_str), &mut perf),
        "pack" => pack(args.get(1).map(String::as_str), &mut perf),
        "all" => {
            for n in [2, 4, 8] {
                compact(n, None, &mut perf);
            }
            for n in [8, 12, 16, 20, 24] {
                mobius(n, None, &mut perf);
            }
            bvprog(None, &mut perf);
            pack(None, &mut perf);
        }
        other => panic!("unknown group {other:?}"),
    }
}
