//! End-to-end demo: compile → run on the simulated CPU → view the park-tax histogram,
//! for the M-D.b2 in-memory **array map** (f2) and **XOR/parity reduce** (f3).
//!
//! Run:  `cargo run -p theseus-tisc --example array_histogram --release`
//!
//! For each program it: builds the Core-Π `Iso`, compiles it to a TISC-1 ROM
//! (`compile_with_metadata_resident`), runs it on the byte-simulated CPU
//! (`run_on_cpu_fast_resident`) and checks it against the reference interpreter
//! (`eval`), then sweeps the runtime count `k` and prints `T(k)` (productive steps)
//! and the WCET park-tax `(B−T)/B` from `rom_trace` — the histogram. (For joules,
//! see `cargo run --example rom_energy`, which prices the fib ROM in measured pJ.)

use std::collections::BTreeMap;

use theseus_core::{Iso, Type, Value};
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim, run_on_cpu_fast_resident,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};
use theseus_tisc::energy::rom_trace;

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}
fn arr_ty() -> Type {
    Type::prod(bit(), Type::prod(bit(), Type::One))
}
fn idx_arr_ty() -> Type {
    Type::prod(bit(), arr_ty())
}
fn memexch() -> Iso {
    Iso::Prim(Box::new(memexch_prim(&bit(), 2).expect("memexch_prim")))
}
fn notb() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}
fn bit_val(b: u8) -> Value {
    if b == 0 {
        Value::left(Value::Unit)
    } else {
        Value::right(Value::Unit)
    }
}
fn numeral(k: usize) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

// ── f2: array map — BODY = memexch; (g⊗id); memexch; (id⊗succ⊗id) ──
fn map_iso(d: usize) -> Iso {
    let body = Iso::seq(
        Iso::seq(
            Iso::seq(memexch(), Iso::times(notb(), Iso::Id(idx_arr_ty()))),
            memexch(),
        ),
        Iso::times(Iso::Id(bit()), Iso::times(notb(), Iso::Id(arr_ty()))),
    );
    Iso::iter(Type::numeral_ty(d), body)
}
// carrier: Nat_d × (data, idx, arr0, arr1)
fn map_input(k: usize, data: u8, idx: u8, a0: u8, a1: u8) -> Value {
    Value::pair(
        numeral(k),
        Value::pair(
            bit_val(data),
            Value::pair(
                bit_val(idx),
                Value::pair(bit_val(a0), Value::pair(bit_val(a1), Value::Unit)),
            ),
        ),
    )
}

// ── f3: XOR reduce — BODY = (id⊗memexch); assoc;(reduceStep⊗id);assoc; (id⊗memexch); succ ──
fn xor_prim() -> Iso {
    let ty = Type::prod(bit(), bit());
    let mut forward = BTreeMap::new();
    for v in theseus_core::prim::enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(&v, &ty);
        let (out, _) = value_from_leaf_bytes_pub(&[b[0], b[0] ^ b[1]], &ty);
        forward.insert(v, out);
    }
    let backward = forward.clone();
    Iso::Prim(Box::new(theseus_core::prim::PrimDef {
        forward_name: "xor".into(),
        backward_name: "xor".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }))
}
fn reduce_iso(d: usize) -> Iso {
    let reduce_step = Iso::seq(
        Iso::SwapProd(bit(), bit()),
        Iso::seq(xor_prim(), Iso::SwapProd(bit(), bit())),
    );
    let idacc_mexch = Iso::times(Iso::Id(bit()), memexch());
    let body = Iso::seq(
        Iso::seq(
            Iso::seq(
                Iso::seq(
                    Iso::seq(
                        idacc_mexch.clone(),
                        Iso::AssocLProd(bit(), bit(), idx_arr_ty()),
                    ),
                    Iso::times(reduce_step, Iso::Id(idx_arr_ty())),
                ),
                Iso::AssocRProd(bit(), bit(), idx_arr_ty()),
            ),
            idacc_mexch,
        ),
        Iso::times(
            Iso::Id(bit()),
            Iso::times(Iso::Id(bit()), Iso::times(notb(), Iso::Id(arr_ty()))),
        ),
    );
    Iso::iter(Type::numeral_ty(d), body)
}
// carrier: Nat_d × (acc, data, idx, arr0, arr1)
fn reduce_input(k: usize, acc: u8, data: u8, idx: u8, a0: u8, a1: u8) -> Value {
    Value::pair(
        numeral(k),
        Value::pair(
            bit_val(acc),
            Value::pair(
                bit_val(data),
                Value::pair(
                    bit_val(idx),
                    Value::pair(bit_val(a0), Value::pair(bit_val(a1), Value::Unit)),
                ),
            ),
        ),
    )
}

fn histogram(
    name: &str,
    iso: &Iso,
    d: usize,
    ty: &Type,
    sample: &Value,
    inputs: &dyn Fn(usize) -> Value,
) {
    let lowered = compile_with_metadata_resident(iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let b = lowered.wcet();
    let n_mexchr = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, theseus_tisc::asm::Instr::MexchReg { .. }))
        .count();

    println!("\n=== {name} (d={d}) ===");
    println!(
        "compiled → {}-instr ROM, {n_mexchr} MexchReg, WCET B={b}",
        rom.len()
    );

    // Run on the simulated CPU and check against eval.
    let mut fuel = 5_000_000u64;
    let eval_out = theseus_eval::eval(iso, sample.clone(), &mut fuel).expect("eval");
    let cpu_out = run_on_cpu_fast_resident(iso, sample, 2).expect("run_on_cpu");
    println!("sample run:  in {sample:?}");
    println!("  CPU out    {cpu_out:?}");
    println!(
        "  eval out   {eval_out:?}   [{}]",
        if cpu_out == eval_out {
            "run_on_cpu == eval ✓"
        } else {
            "MISMATCH ✗"
        }
    );

    // Park-tax histogram: sweep the runtime count k.
    println!("  k |  T(k) | park_tax=(B-T)/B");
    println!("  --+-------+-----------------");
    for k in 0..d {
        let init = marshal_resident(&inputs(k), ty, 2);
        let t = rom_trace(&rom, init, b).steps_t;
        let park = (b - t) as f64 / b as f64;
        let bar = "#".repeat((park * 40.0).round() as usize);
        println!("  {k} |  {t:>3}  | {:>5.1}%  {bar}", park * 100.0);
    }
}

fn main() {
    // f2: array MAP (flip each of the first k words), d=4.
    let map = map_iso(4);
    let map_ty = Type::prod(
        Type::numeral_ty(4),
        Type::prod(
            bit(),
            Type::prod(bit(), Type::prod(bit(), Type::prod(bit(), Type::One))),
        ),
    );
    histogram(
        "array MAP  (arr[i] ↦ ¬arr[i])",
        &map,
        4,
        &map_ty,
        &map_input(2, 0, 0, 1, 0),
        &|k| map_input(k, 0, 0, 1, 0),
    );

    // f3: XOR/parity REDUCE (acc ⊕= arr[i]), d=3.
    let reduce = reduce_iso(3);
    let red_ty = Type::prod(
        Type::numeral_ty(3),
        Type::prod(
            bit(),
            Type::prod(
                bit(),
                Type::prod(bit(), Type::prod(bit(), Type::prod(bit(), Type::One))),
            ),
        ),
    );
    histogram(
        "XOR reduce (acc ⊕= arr[i])",
        &reduce,
        3,
        &red_ty,
        &reduce_input(2, 0, 0, 0, 1, 1),
        &|k| reduce_input(k, 0, 0, 0, 1, 0),
    );

    println!("\n(joules: `cargo run -p theseus-tisc --example rom_energy` prices the fib ROM in measured pJ.)");
}
