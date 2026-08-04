//! Tick-level phased simulation of the cell netlist (ADR-0026 C2.7).
//!
//! # The timed token model
//!
//! Tokens advance one stage per tick (2LAL: every cell is a register).
//! A cell at `depth` d fires on tick d -- it consumes tokens *valid at* d
//! and produces tokens valid at d + 1. A token line is therefore valid
//! on exactly one tick: its arrival tick ([`PhasedCellNetlist::line_arrivals`]).
//!
//! # Dynamic INV-TOKEN, per tick
//!
//! The structural half of INV-TOKEN (rail-pair conservation, 1 driver /
//! 1 consumer per line) is [`PhasedCellNetlist::validate`]. This module
//! asserts the **dynamic** half (ADR-0031, ADR-0026 C2.6 spirit): on
//! every tick, for every rail pair valid at that tick, *exactly one* of
//! its two token lines is high. A violation is a [`SimError::InvToken`],
//! never a wrong answer.
//!
//! Consequences of the one-tick validity window:
//!
//! - a netlist with reconvergent inputs arriving on different ticks
//!   ([`PhasedCellNetlist::tick_imbalances`]) is a *wrong circuit*
//!   (C2.2): the early token is gone by the time the cell fires. The sim
//!   refuses it ([`SimError::Unbalanced`]) rather than pretending the
//!   token waited.
//! - output-port skew is fine: each output pair is decoded at its own
//!   recorded arrival tick.
//!
//! # Deferred (honest list)
//!
//! - TODO(C2.6): exact validity-*window* arithmetic (windows wider than
//!   one tick, pinned empirically in B5 against the reference decks).
//!   Until then this sim's window is exactly the arrival tick.
//! - TODO(bit-level sim): the electrical (rail, mirror) SR granularity
//!   below the token line lives inside the characterized cell
//!   (`theseus-spice`); simulating it here is `sim_bits`, deferred.

use std::collections::{BTreeMap, BTreeSet};

use crate::ir::cell::{CellKind, CellNetError, PhasedCellNetlist, RailPair};

/// Outcome of a successful timed run.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimResult {
    /// Decoded output tokens, in output-port order (each read at its
    /// own arrival tick).
    pub outputs: Vec<bool>,
    /// Total ticks simulated, `0..=ticks` inclusive (= [`PhasedCellNetlist::depth`]).
    pub ticks: u32,
    /// Per-(kind, phase) cell occupancy ([`PhasedCellNetlist::occupancy`]):
    /// which clock trapezoids drive how many cells of each kind.
    pub occupancy: BTreeMap<(CellKind, u8), usize>,
    /// Number of per-tick INV-TOKEN pair assertions that were checked
    /// (observability: every rail pair in the netlist, once, at its
    /// valid tick).
    pub inv_token_checks: usize,
}

/// Why a timed run refused to start or aborted.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum SimError {
    /// The netlist is structurally invalid.
    #[error("invalid netlist: {0}")]
    Net(#[from] CellNetError),
    /// Wrong number of input tokens for the netlist's input ports.
    #[error("netlist has {want} input ports, got {got} tokens")]
    InputWidth { want: usize, got: usize },
    /// A cell's inputs arrive on different ticks -- a wrong circuit under
    /// ADR-0026 C2.2 (the tick-exact balancer, a later slice, owes K0
    /// insertions). The timed sim refuses rather than letting the early
    /// token silently wait.
    #[error(
        "cell {cell} inputs arrive on ticks {arrivals:?} (C2.2: wrong circuit until balanced)"
    )]
    Unbalanced { cell: usize, arrivals: Vec<u32> },
    /// Dynamic INV-TOKEN violation: at `tick`, this rail pair did not
    /// have exactly one line high. `None` means the line was never
    /// driven by that tick.
    #[error(
        "INV-TOKEN violated at tick {tick}: pair (l{}, l{}) has \
         line0={line0_high:?}, line1={line1_high:?} (want exactly one high)",
        .pair.line0.0, .pair.line1.0
    )]
    InvToken {
        tick: u32,
        pair: RailPair,
        line0_high: Option<bool>,
        line1_high: Option<bool>,
    },
}

/// The per-tick INV-TOKEN assertion for one rail pair: both lines driven
/// and exactly one high.
fn check_pair_at(high: &[Option<bool>], pair: RailPair, tick: u32) -> Result<(), SimError> {
    let line0_high = high[pair.line0.0 as usize];
    let line1_high = high[pair.line1.0 as usize];
    match (line0_high, line1_high) {
        (Some(a), Some(b)) if a != b => Ok(()),
        _ => Err(SimError::InvToken {
            tick,
            pair,
            line0_high,
            line1_high,
        }),
    }
}

/// Run the netlist on one input-token vector, tick by tick.
///
/// Walks cells in tick order (topological by construction), computes
/// each output line's token via [`CellKind::eval`], asserts dynamic
/// INV-TOKEN for every rail pair at its valid tick, and decodes the
/// output ports at their recorded arrival ticks.
///
/// Refuses invalid ([`SimError::Net`]) and tick-imbalanced
/// ([`SimError::Unbalanced`]) netlists up front.
pub fn simulate(net: &PhasedCellNetlist, input_tokens: &[bool]) -> Result<SimResult, SimError> {
    net.validate().map_err(SimError::Net)?;
    if input_tokens.len() != net.inputs.len() {
        return Err(SimError::InputWidth {
            want: net.inputs.len(),
            got: input_tokens.len(),
        });
    }
    if let Some(imb) = net
        .tick_imbalances()
        .map_err(SimError::Net)?
        .into_iter()
        .next()
    {
        return Err(SimError::Unbalanced {
            cell: imb.cell,
            arrivals: imb.arrivals,
        });
    }
    let arrival = net.line_arrivals().map_err(SimError::Net)?;

    // Every rail pair in the netlist, indexed by its valid tick.
    // validate() already rejected pair skew, so a pair's valid tick is
    // its lines' (equal) arrival tick. The same two lines may be named
    // by both their driver and their consumer (possibly role-swapped --
    // NOT-in-Permute); the 1-of-2 predicate is role-symmetric, so one
    // check per unordered line pair suffices.
    let mut seen: BTreeSet<(u32, u32)> = BTreeSet::new();
    let mut pairs_at: BTreeMap<u32, Vec<RailPair>> = BTreeMap::new();
    {
        let mut note = |p: &RailPair| {
            let key = (p.line0.0.min(p.line1.0), p.line0.0.max(p.line1.0));
            if seen.insert(key) {
                pairs_at
                    .entry(arrival[p.line1.0 as usize])
                    .or_default()
                    .push(*p);
            }
        };
        for p in &net.inputs {
            note(p);
        }
        for cell in &net.cells {
            for p in cell.inputs.iter().chain(&cell.outputs) {
                note(p);
            }
        }
        for p in &net.outputs {
            note(p);
        }
    }

    // Cells grouped by firing tick (= depth).
    let mut cells_at: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    for (i, cell) in net.cells.iter().enumerate() {
        cells_at.entry(cell.depth).or_default().push(i);
    }

    // high[line] = the line's pulse at its (single) valid tick; None
    // until driven. Dual-rail encode the inputs at tick 0.
    let mut high: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    for (p, &tok) in net.inputs.iter().zip(input_tokens) {
        high[p.line1.0 as usize] = Some(tok);
        high[p.line0.0 as usize] = Some(!tok);
    }

    let ticks = net.depth();
    let mut inv_token_checks = 0usize;
    for t in 0..=ticks {
        // 1. Assert dynamic INV-TOKEN on every pair valid this tick
        //    (inputs at 0; cell/port outputs the tick after their cell
        //    fires).
        if let Some(pairs) = pairs_at.get(&t) {
            for &p in pairs {
                check_pair_at(&high, p, t)?;
                inv_token_checks += 1;
            }
        }
        // 2. Fire this tick's cells: consume tokens valid at t, produce
        //    tokens valid at t + 1.
        if let Some(cells) = cells_at.get(&t) {
            for &ci in cells {
                let cell = &net.cells[ci];
                let ins: Vec<bool> = cell
                    .inputs
                    .iter()
                    .map(|p| high[p.line1.0 as usize].expect("pair checked at this tick"))
                    .collect();
                let outs = cell.kind.eval(&ins);
                for (p, &v) in cell.outputs.iter().zip(&outs) {
                    high[p.line1.0 as usize] = Some(v);
                    high[p.line0.0 as usize] = Some(!v);
                }
            }
        }
    }

    let outputs = net
        .outputs
        .iter()
        .map(|p| high[p.line1.0 as usize].expect("output pair checked at its arrival tick"))
        .collect();

    Ok(SimResult {
        outputs,
        ticks,
        occupancy: net.occupancy(),
        inv_token_checks,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::{enumerate_values, type_of, Iso, Type};

    use crate::ir::cell::{Cell, LineId};
    use crate::ir::typed::{NodeKind, TypedNode};
    use crate::lower::{bits_of, lower, lower_iso, prim_from_bits};

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn three_bits() -> Type {
        Type::prod(bit(), Type::prod(bit(), bit()))
    }

    fn four_bits() -> Type {
        Type::prod(Type::prod(bit(), bit()), Type::prod(bit(), bit()))
    }

    fn fredkin_iso() -> Iso {
        let def = prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
            CellKind::K1.eval(b)
        })
        .unwrap();
        Iso::Prim(Box::new(def))
    }

    fn ccswap_iso() -> Iso {
        let def =
            prim_from_bits("ccswap", four_bits(), four_bits(), |b| CellKind::K2.eval(b)).unwrap();
        Iso::Prim(Box::new(def))
    }

    /// The timed differential oracle: for every input value, the
    /// tick-level sim of the lowered netlist must agree with the core
    /// interpreter (the semantic root, ADR-0008/0018) -- extending the
    /// untimed lowering oracle in `lower::tests` to the timed executor.
    fn assert_timed_sim_agrees(iso: &Iso) -> PhasedCellNetlist {
        let (in_ty, out_ty) = type_of(iso).expect("iso types");
        let net = lower_iso(iso).expect("lowering succeeds");
        for v in enumerate_values(&in_ty) {
            let bits = bits_of(&v, &in_ty).unwrap();
            let res = simulate(&net, &bits).expect("timed sim runs");
            let mut fuel = 10_000u64;
            let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
            assert_eq!(
                res.outputs,
                bits_of(&expected, &out_ty).unwrap(),
                "timed sim disagrees with core eval on {v:?}"
            );
            assert_eq!(res.ticks, net.depth());
            assert_eq!(res.occupancy, net.occupancy());
        }
        net
    }

    #[test]
    fn timed_oracle_wiring_only_isos() {
        for iso in [
            Iso::Id(bit()),
            Iso::SwapProd(bit(), bit()),
            Iso::SwapSum(Type::One, Type::One), // NOT via rail swap
            Iso::AssocRProd(bit(), bit(), bit()),
            Iso::UniteProd(bit()),
        ] {
            let net = assert_timed_sim_agrees(&iso);
            assert!(net.cells.is_empty(), "{iso:?} should be pure wiring");
        }
    }

    #[test]
    fn timed_oracle_fredkin() {
        let net = assert_timed_sim_agrees(&fredkin_iso());
        assert_eq!(net.cells.len(), 1);
        // 3 input pairs at tick 0 + 3 output pairs at tick 1.
        let res = simulate(&net, &[true, true, false]).unwrap();
        assert_eq!(res.outputs, vec![true, false, true], "control=1 swaps a,b");
        assert_eq!(res.inv_token_checks, 6);
        assert_eq!(res.ticks, 1);
    }

    #[test]
    fn timed_oracle_ccswap() {
        assert_timed_sim_agrees(&ccswap_iso());
    }

    #[test]
    fn timed_oracle_fredkin_chain_rotates_phases() {
        let fk = fredkin_iso;
        let iso = Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), fk()))));
        let net = assert_timed_sim_agrees(&iso);
        assert_eq!(net.depth(), 5);
        let res = simulate(&net, &[true, false, true]).unwrap();
        // 5 sequential cells: 3 pairs at each of ticks 0..=5.
        assert_eq!(res.inv_token_checks, 18);
        assert_eq!(
            res.occupancy,
            BTreeMap::from([
                ((CellKind::K1, 0), 2), // depths 0 and 4 both drive phase 0
                ((CellKind::K1, 1), 1),
                ((CellKind::K1, 2), 1),
                ((CellKind::K1, 3), 1),
            ])
        );
    }

    #[test]
    fn timed_oracle_adjoint_of_fredkin() {
        assert_timed_sim_agrees(&Iso::sym(fredkin_iso()));
    }

    #[test]
    fn reg3_is_a_three_tick_identity() {
        let reg3 = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Reg(3),
        };
        let net = lower(&reg3).unwrap();
        for tok in [true, false] {
            let res = simulate(&net, &[tok]).unwrap();
            assert_eq!(res.outputs, vec![tok]);
            assert_eq!(res.ticks, 3);
            // 1 pair valid at each of ticks 0, 1, 2, 3.
            assert_eq!(res.inv_token_checks, 4);
            assert_eq!(
                res.occupancy,
                BTreeMap::from([
                    ((CellKind::K0, 0), 1),
                    ((CellKind::K0, 1), 1),
                    ((CellKind::K0, 2), 1),
                ])
            );
        }
    }

    #[test]
    fn output_skew_is_simulable_each_port_at_its_own_tick() {
        // Reg(1) × id: outputs arrive at ticks [1, 0] -- fine for the
        // sim, each port decodes at its recorded arrival tick.
        let id_bit = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Permute {
                monoid: crate::ir::typed::Monoid::Prod,
                sigma: vec![0],
            },
        };
        let reg1 = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Reg(1),
        };
        let net = lower(&TypedNode {
            input: Type::prod(bit(), bit()),
            output: Type::prod(bit(), bit()),
            kind: NodeKind::Par(Box::new(reg1), Box::new(id_bit)),
        })
        .unwrap();
        assert_eq!(net.output_depths, vec![1, 0]);
        let res = simulate(&net, &[true, false]).unwrap();
        assert_eq!(res.outputs, vec![true, false]);
    }

    #[test]
    fn unbalanced_cell_inputs_are_refused() {
        // (Reg(1) × id × id) ; fredkin -- the control arrives one tick
        // late (C2.2 wrong circuit); the timed sim must refuse, not
        // pretend the early tokens waited.
        let id_bit = || TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Permute {
                monoid: crate::ir::typed::Monoid::Prod,
                sigma: vec![0],
            },
        };
        let reg1 = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Reg(1),
        };
        let par = TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Par(
                Box::new(reg1),
                Box::new(TypedNode {
                    input: Type::prod(bit(), bit()),
                    output: Type::prod(bit(), bit()),
                    kind: NodeKind::Par(Box::new(id_bit()), Box::new(id_bit())),
                }),
            ),
        };
        let fredkin = TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Prim(Box::new(
                prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
                    CellKind::K1.eval(b)
                })
                .unwrap(),
            )),
        };
        let net = lower(&TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Seq(vec![par, fredkin]),
        })
        .unwrap();
        match simulate(&net, &[true, true, false]) {
            Err(SimError::Unbalanced { arrivals, .. }) => {
                assert_eq!(arrivals, vec![1, 0, 0]);
            }
            other => panic!("expected Unbalanced, got {other:?}"),
        }
    }

    #[test]
    fn wrong_input_width_is_refused() {
        let net = lower_iso(&Iso::Id(bit())).unwrap();
        assert_eq!(
            simulate(&net, &[]),
            Err(SimError::InputWidth { want: 1, got: 0 })
        );
    }

    #[test]
    fn invalid_netlist_is_refused() {
        // A K0 whose recorded phase disagrees with depth mod P.
        let inp = RailPair {
            line0: LineId(0),
            line1: LineId(1),
        };
        let out = RailPair {
            line0: LineId(2),
            line1: LineId(3),
        };
        let net = PhasedCellNetlist {
            input_ty: bit(),
            output_ty: bit(),
            inputs: vec![inp],
            outputs: vec![out],
            output_depths: vec![1],
            cells: vec![Cell {
                kind: CellKind::K0,
                depth: 0,
                phase: 3,
                inputs: vec![inp],
                outputs: vec![out],
            }],
            num_lines: 4,
        };
        assert!(matches!(simulate(&net, &[true]), Err(SimError::Net(_))));
    }

    // The InvToken variant is defense in depth: unreachable through
    // `simulate` for a validated, balanced netlist (every write is
    // dual-rail by construction), so the check itself is unit-tested.
    #[test]
    fn inv_token_check_rejects_bad_pairs() {
        let pair = RailPair {
            line0: LineId(0),
            line1: LineId(1),
        };
        // Exactly one high: fine, either polarity.
        assert_eq!(check_pair_at(&[Some(false), Some(true)], pair, 0), Ok(()));
        assert_eq!(check_pair_at(&[Some(true), Some(false)], pair, 0), Ok(()));
        // Both high, both low, or undriven: violations.
        for high in [
            [Some(true), Some(true)],
            [Some(false), Some(false)],
            [None, Some(true)],
            [Some(true), None],
            [None, None],
        ] {
            assert!(matches!(
                check_pair_at(&high, pair, 7),
                Err(SimError::InvToken { tick: 7, .. })
            ));
        }
    }
}
