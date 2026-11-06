//! Lane simulators ≡ scalar simulators, lane by lane (ADR-0048).
//!
//! Over a corpus of netlists -- the lowering tests' cell templates plus
//! seeded random compositions of random-permutation prims (synthesized
//! into K1/K2/R1 cells, with ancillas, Sym, Seq and skewed Times) -- and
//! random inputs, lane `l` of [`simulate_lanes`] must equal [`simulate`]
//! on lane `l`'s inputs, and lane `l` of
//! [`CompactBalanced::simulate_lanes`] must equal
//! [`CompactBalanced::simulate`]. Refusals must agree, and an injected
//! INV-TOKEN fault must be reported on the right lane with exactly the
//! scalar error.

mod common;

use common::bit;
use theseus_core::simd::SimdLevel;
use theseus_core::{type_of, Iso, Type};
use theseus_netlist::{
    balance, balance_compact, lower, lower_iso, pack_lanes, prim_from_bits, simulate,
    simulate_lanes, simulate_rails, simulate_rails_lanes, unpack_lane, unpack_lanes, CellKind,
    CompactBalanced, CompactKernel, LaneScratch, LaneSimError, Lanes, Monoid, NodeKind,
    PhasedCellNetlist, SimError, TypedNode,
};

/// splitmix64: a fixed-seed generator, so every failure reproduces.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
    fn bit(&mut self) -> bool {
        self.next() & 1 == 1
    }
}

/// `Bit × (Bit × (… × Bit))`, `n` bits.
fn bits_ty(n: usize) -> Type {
    if n == 1 {
        bit()
    } else {
        Type::prod(bit(), bits_ty(n - 1))
    }
}

fn prim(name: &str, n: usize, f: impl Fn(&[bool]) -> Vec<bool>) -> Iso {
    let ty = bits_ty(n);
    Iso::Prim(Box::new(prim_from_bits(name, ty.clone(), ty, f).unwrap()))
}

/// A random bijection on `n` bits, as a prim (lowered by synthesis).
fn random_perm(rng: &mut Rng, n: usize, id: usize) -> Iso {
    let mut perm: Vec<usize> = (0..1 << n).collect();
    for i in (1..perm.len()).rev() {
        perm.swap(i, rng.below(i + 1));
    }
    prim(&format!("rperm{id}"), n, move |b| {
        let x = b
            .iter()
            .enumerate()
            .fold(0, |x, (i, &v)| x | usize::from(v) << i);
        (0..n).map(|i| (perm[x] >> i) & 1 == 1).collect()
    })
}

/// A random type-preserving iso on `n` bits.
fn random_iso(rng: &mut Rng, n: usize, depth: u32, id: &mut usize) -> Iso {
    *id += 1;
    let leaf = depth == 0 || rng.below(3) == 0;
    if leaf {
        return match rng.below(4) {
            0 if n == 1 => Iso::SwapSum(Type::One, Type::One),
            0 => Iso::Id(bits_ty(n)),
            // Synthesis caps prims at 4 bits.
            _ if n > 4 => Iso::times(random_iso(rng, 1, 0, id), random_iso(rng, n - 1, 0, id)),
            _ => random_perm(rng, n, *id),
        };
    }
    match rng.below(4) {
        0 => Iso::sym(random_iso(rng, n, depth - 1, id)),
        1 if n >= 2 => Iso::times(
            random_iso(rng, 1, depth - 1, id),
            random_iso(rng, n - 1, depth - 1, id),
        ),
        _ => Iso::seq(
            random_iso(rng, n, depth - 1, id),
            random_iso(rng, n, depth - 1, id),
        ),
    }
}

/// Netlists under test: the lowering tests' templates (K1, K2, K3, R1,
/// K0 chains, output skew) and random compositions. Some are tick
/// imbalanced on purpose (skewed `Times`, `Reg × id`).
fn corpus() -> Vec<PhasedCellNetlist> {
    let mut nets: Vec<PhasedCellNetlist> = [
        prim("fredkin", 3, |b| CellKind::K1.eval(b)),
        prim("ccswap", 4, |b| CellKind::K2.eval(b)),
        prim("cccswap", 5, |b| CellKind::K3.eval(b)),
        prim("cnot", 2, |b| vec![b[0], b[0] ^ b[1]]),
        prim("toffoli", 3, |b| vec![b[0], b[1], b[2] ^ (b[0] & b[1])]),
        Iso::SwapSum(Type::One, Type::One),
    ]
    .iter()
    .map(|iso| lower_iso(iso).unwrap())
    .collect();

    let reg = |k| TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Reg(k),
    };
    let id_bit = TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Permute {
            monoid: Monoid::Prod,
            sigma: vec![0],
        },
    };
    nets.push(lower(&reg(3)).unwrap());
    nets.push(
        lower(&TypedNode {
            input: Type::prod(bit(), bit()),
            output: Type::prod(bit(), bit()),
            kind: NodeKind::Par(Box::new(reg(2)), Box::new(id_bit)),
        })
        .unwrap(),
    );

    let mut rng = Rng(0x007E_5E05);
    let mut id = 0;
    for _ in 0..40 {
        let n = 2 + rng.below(4);
        let iso = random_iso(&mut rng, n, 3, &mut id);
        type_of(&iso).expect("generator builds well-typed isos");
        nets.push(lower_iso(&iso).unwrap());
    }
    nets
}

/// `lanes` random input vectors for `net`.
fn random_inputs(rng: &mut Rng, net: &PhasedCellNetlist, lanes: usize) -> Vec<Vec<bool>> {
    (0..lanes)
        .map(|_| (0..net.inputs.len()).map(|_| rng.bit()).collect())
        .collect()
}

/// Timed: every lane equals the scalar sim, Ok or Err -- at 256 lanes
/// (`inputs` may be fewer: the rest idle) and at 64.
fn assert_timed_lanes_agree(net: &PhasedCellNetlist, inputs: &[Vec<bool>]) {
    let wide = simulate_lanes::<4>(net, &pack_lanes(inputs));
    let narrow = simulate_lanes::<1>(net, &pack_lanes(&inputs[..inputs.len().min(64)]));
    for (lane, input) in inputs.iter().enumerate() {
        let want = simulate(net, input);
        let results = if lane < 64 {
            vec![
                wide.clone().map(|r| r.lane(lane)),
                narrow.clone().map(|r| r.lane(lane)),
            ]
        } else {
            vec![wide.clone().map(|r| r.lane(lane))]
        };
        for got in results {
            match (got, &want) {
                (Ok(got), Ok(want)) => assert_eq!(got, *want, "lane {lane}"),
                (Err(LaneSimError::Refused(e)), Err(want)) => assert_eq!(e, *want, "lane {lane}"),
                (got, want) => panic!("lane {lane}: lanes {got:?} vs scalar {want:?}"),
            }
        }
    }
}

/// Compact: every lane equals the scalar compact sim, at 256 / 192 / 64
/// lanes.
fn assert_compact_lanes_agree(net: &PhasedCellNetlist, inputs: &[Vec<bool>]) {
    let compact = balance_compact(net).unwrap();
    let w4 = compact.simulate_lanes::<4>(&pack_lanes(inputs)).unwrap();
    let w3 = compact
        .simulate_lanes::<3>(&pack_lanes(&inputs[..192]))
        .unwrap();
    let w1 = compact
        .simulate_lanes::<1>(&pack_lanes(&inputs[..64]))
        .unwrap();
    for (lane, input) in inputs.iter().enumerate() {
        let want = compact.simulate(input).unwrap().outputs;
        assert_eq!(unpack_lane(&w4, lane), want, "lane {lane}");
        if lane < 192 {
            assert_eq!(unpack_lane(&w3, lane), want, "lane {lane} (N=3)");
        }
        if lane < 64 {
            assert_eq!(unpack_lane(&w1, lane), want, "lane {lane} (N=1)");
        }
    }
}

#[test]
fn timed_lanes_match_scalar_on_corpus() {
    let mut rng = Rng(1);
    let (mut refused, mut ran) = (0, 0);
    for net in &corpus() {
        let balanced = balance(net).unwrap();
        for net in [net, &balanced] {
            // 70 vectors: two words partly filled, idle lanes above.
            let inputs = random_inputs(&mut rng, net, 70);
            assert_timed_lanes_agree(net, &inputs);
            if net.tick_imbalances().unwrap().is_empty() {
                ran += 1;
            } else {
                refused += 1;
            }
        }
    }
    // The corpus exercises both the run and the Unbalanced refusal.
    assert!(refused >= 5 && ran >= 40, "refused {refused}, ran {ran}");
}

#[test]
fn compact_lanes_match_scalar_on_corpus() {
    let mut rng = Rng(2);
    for net in &corpus() {
        let inputs = random_inputs(&mut rng, net, Lanes::<4>::WIDTH);
        assert_compact_lanes_agree(net, &inputs);
    }
}

/// ADR-0052: every compact-sim kernel (reference, portable uniform, and
/// the SIMD kernel at each level this host has) at every block width with
/// a kernel (2, 4, 8 words), lane for lane against the scalar sim.
#[test]
fn every_compact_kernel_matches_scalar_on_corpus() {
    fn check<const N: usize>(compact: &CompactBalanced, inputs: &[Vec<bool>], want: &[Vec<bool>]) {
        let packed = pack_lanes::<N, _>(&inputs[..inputs.len().min(Lanes::<N>::WIDTH)]);
        let mut kernels = vec![CompactKernel::Reference, CompactKernel::UniformPortable];
        kernels.extend(SimdLevel::available().into_iter().map(CompactKernel::Simd));
        let mut scratch = LaneScratch::default();
        for kernel in kernels {
            let out = compact
                .simulate_lanes_kernel(&packed, &mut scratch, kernel)
                .unwrap();
            let got = unpack_lanes(&out, inputs.len().min(Lanes::<N>::WIDTH));
            for (lane, (got, want)) in got.iter().zip(want).enumerate() {
                assert_eq!(got, want, "{kernel:?}, N = {N}, lane {lane}");
            }
        }
    }
    let mut rng = Rng(0x0052);
    for net in &corpus() {
        let compact = balance_compact(net).unwrap();
        let inputs = random_inputs(&mut rng, net, Lanes::<8>::WIDTH);
        let want: Vec<Vec<bool>> = inputs
            .iter()
            .map(|i| compact.simulate(i).unwrap().outputs)
            .collect();
        check::<2>(&compact, &inputs, &want);
        check::<4>(&compact, &inputs, &want);
        check::<8>(&compact, &inputs, &want);
    }
}

#[test]
fn width_refusals_agree() {
    let net = &corpus()[0];
    let short = vec![Lanes::<4>::ZERO; net.inputs.len() - 1];
    assert_eq!(
        simulate_lanes(net, &short),
        Err(LaneSimError::Refused(
            simulate(net, &vec![false; short.len()]).unwrap_err()
        ))
    );
    let compact = balance_compact(net).unwrap();
    assert_eq!(
        compact.simulate_lanes(&short),
        Err(compact.simulate(&vec![false; short.len()]).unwrap_err())
    );
}

/// Corrupt one input pair (both lines equal) on a random subset of
/// lanes: the lane sim must stop at the earliest failing check and name
/// its lowest failing lane, with exactly that lane's scalar error, and
/// no lower lane may fail that same check; masking the faulty lanes off
/// must run clean and match the scalar sim on the good lanes.
#[test]
fn inv_token_faults_are_reported_per_lane() {
    let mut rng = Rng(3);
    let mut checked = 0;
    for net in &corpus() {
        let net = balance(net).unwrap();
        for _ in 0..2 {
            let inputs = random_inputs(&mut rng, &net, Lanes::<2>::WIDTH);
            let mut rails: Vec<Vec<(bool, bool)>> = inputs
                .iter()
                .map(|v| v.iter().map(|&t| (!t, t)).collect())
                .collect();
            let mut good = Lanes::<2>::ONES;
            for _ in 0..1 + rng.below(6) {
                let (lane, port, high) = (
                    rng.below(rails.len()),
                    rng.below(net.inputs.len()),
                    rng.bit(),
                );
                rails[lane][port] = (high, high);
                good.set(lane, false);
            }
            let packed = pack_rails::<2>(&rails);
            let scalar = |lane: usize| simulate_rails(&net, &rails[lane]);

            match simulate_rails_lanes(&net, &packed, Lanes::ONES) {
                Err(LaneSimError::Lane { lane, error }) => {
                    assert_eq!(scalar(lane), Err(error.clone()), "reported lane {lane}");
                    assert!(matches!(error, SimError::InvToken { tick: 0, .. }));
                    // Only faulted lanes can fail; good lanes stay 1-of-2.
                    for l in (0..lane).filter(|&l| !good.get(l)) {
                        assert_ne!(
                            scalar(l).err().as_ref(),
                            Some(&error),
                            "lane {l} fails the same check, yet lane {lane} was reported"
                        );
                    }
                    checked += 1;
                }
                other => panic!("expected a lane fault, got {other:?}"),
            }
            let res = simulate_rails_lanes(&net, &packed, good).expect("faulty lanes masked");
            for lane in (0..rails.len()).filter(|&l| good.get(l)).step_by(16) {
                assert_eq!(res.lane(lane), scalar(lane).unwrap(), "good lane {lane}");
            }
        }
    }
    assert!(checked >= 90, "{checked} fault trials");
}

/// Pack per-lane `(line0, line1)` input rails into lane words.
fn pack_rails<const N: usize>(rails: &[Vec<(bool, bool)>]) -> Vec<(Lanes<N>, Lanes<N>)> {
    let line0: Vec<Vec<bool>> = rails
        .iter()
        .map(|r| r.iter().map(|p| p.0).collect())
        .collect();
    let line1: Vec<Vec<bool>> = rails
        .iter()
        .map(|r| r.iter().map(|p| p.1).collect())
        .collect();
    pack_lanes(&line0)
        .into_iter()
        .zip(pack_lanes(&line1))
        .collect()
}
