//! Analytic per-clause energy model for TISC-1 (ADR-0028 M4-v1).
//!
//! Computes the real per-instruction and per-step energy of the compiled
//! TISC-1 machine from the rung-0 characterization database, WITHOUT
//! materializing the balanced netlist (which OOMs at ~1.5M K0 buffers per
//! step).
//!
//! ## Key insight
//!
//! The balancer's K0 buffer count can be computed ANALYTICALLY from the
//! unbalanced netlist. For each cell, the number of K0 buffers the
//! balancer would insert equals the sum of per-input arrival deficits
//! `(max_arrival - arrival_i)`. This is exactly what [`balance`] does
//! (see `crates/theseus-netlist/src/balance.rs` lines 109-122), but we
//! compute only the COUNT, never the cells.
//!
//! Because `decode_execute` lowers all 14 predicated instructions every
//! step, per-step energy is approximately instruction-independent (the
//! shared decode dominates). Per-clause attribution = the shared
//! fetch/decode/unfetch overhead + each instruction's own gates.

use std::collections::BTreeMap;
use std::fmt;

use theseus_netlist::CellKind;
use theseus_netlist::PhasedCellNetlist;

use theseus_spice::annotate::{compiled_energy, KindEnergy};
use theseus_spice::chardb::{CharDb, QueryError};

use crate::compile::run_rom_fast;
use crate::types::{StateVals, PC_ADDR_SPACE};

/// Per-cell-kind energy breakdown for a balanced netlist, computed
/// analytically (without materializing the balanced netlist).
#[derive(Clone, Debug)]
pub struct EnergyBreakdown {
    /// Operating ramp time, seconds.
    pub t_ramp: f64,
    /// Total energy (active cells + analytic buffer energy), joules.
    pub total_j: f64,
    /// Active-cell energy from `compiled_energy`, joules.
    pub active_j: f64,
    /// Energy from K0 buffers the balancer WOULD insert, joules.
    pub buffer_j: f64,
    /// Number of K0 buffers the balancer would insert.
    pub buffer_count: usize,
    /// Breakdown by cell kind: active cells keyed by `"cswap-kN"`,
    /// plus `"cswap-k0-balance"` for the analytic buffer contribution.
    pub by_kind: BTreeMap<String, KindEnergy>,
}

impl fmt::Display for EnergyBreakdown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "energy breakdown @ t_ramp = {:.3e} s", self.t_ramp)?;
        for (key, k) in &self.by_kind {
            writeln!(
                f,
                "  {key}: {} cell(s) x {:.4e} J = {:.4e} J",
                k.count, k.per_cell_j, k.subtotal_j
            )?;
        }
        writeln!(f, "  active energy:  {:.4e} J", self.active_j)?;
        writeln!(
            f,
            "  buffer energy:  {:.4e} J ({} K0 buffers)",
            self.buffer_j, self.buffer_count
        )?;
        writeln!(f, "  total energy:   {:.4e} J", self.total_j)?;
        write!(
            f,
            "  [claims boundary (ADR-0028 C3.3): logic dissipation under ideal \
             power-clocks; clock-generation/resonator losses and I/O excluded]"
        )
    }
}

/// The number of K0 buffers [`balance`](theseus_netlist::balance) would
/// insert, computed analytically from [`PhasedCellNetlist::line_arrivals`]
/// without materializing the balanced netlist.
///
/// For each cell, the deficit is `sum_i (max_input_arrival - arrival_i)`.
/// The total is the sum over all cells.
///
/// # Panics
///
/// Panics if `line_arrivals` fails (structurally invalid netlist).
pub fn analytic_buffer_count(net: &PhasedCellNetlist) -> usize {
    let arrival = net
        .line_arrivals()
        .expect("line_arrivals must succeed for a valid netlist");
    let mut total: usize = 0;
    for cell in &net.cells {
        let input_arrivals: Vec<u32> = cell
            .inputs
            .iter()
            .map(|p| arrival[p.line1.0 as usize])
            .collect();
        let latest = input_arrivals.iter().copied().max().unwrap_or(0);
        for &arr in &input_arrivals {
            total += (latest - arr) as usize;
        }
    }
    total
}

/// Balanced energy of a compiled (unbalanced) netlist: active-cell energy
/// from [`compiled_energy`] plus the analytic K0 buffer energy.
///
/// The buffer energy = `analytic_buffer_count(net) * char_db.query("cswap-k0", t_ramp)`.
pub fn balanced_energy(
    net: &PhasedCellNetlist,
    char_db: &CharDb,
    t_ramp: f64,
) -> Result<EnergyBreakdown, QueryError> {
    let active_report = compiled_energy(net, char_db, t_ramp)?;
    let buf_count = analytic_buffer_count(net);
    let k0_per_cell_j = char_db.query(CellKind::K0.char_db_key(), t_ramp)?;
    let buffer_j = k0_per_cell_j * buf_count as f64;

    let mut by_kind = active_report.by_kind.clone();
    if buf_count > 0 {
        by_kind.insert(
            "cswap-k0-balance".to_string(),
            KindEnergy {
                count: buf_count,
                per_cell_j: k0_per_cell_j,
                subtotal_j: buffer_j,
            },
        );
    }

    Ok(EnergyBreakdown {
        t_ramp,
        total_j: active_report.total_j + buffer_j,
        active_j: active_report.total_j,
        buffer_j,
        buffer_count: buf_count,
        by_kind,
    })
}

/// Energy of a single lowered instruction iso (unbalanced lowering +
/// analytic buffer count).
///
/// `op_iso` should be a State-level iso (e.g. from [`crate::instr`] or
/// [`crate::decoder::instruction`]).
pub fn instruction_energy(
    op_iso: &theseus_core::Iso,
    char_db: &CharDb,
    t_ramp: f64,
) -> Result<EnergyBreakdown, QueryError> {
    let net = theseus_netlist::lower_iso(op_iso).expect("lower_iso must succeed");
    balanced_energy(&net, char_db, t_ramp)
}

/// Energy of a full step iso (fetch + decode_execute + unfetch + pc_inc),
/// from a pre-lowered unbalanced netlist with analytic buffer count.
///
/// Lowering the full step (~4846 cells) takes ~2 min and ~GBs but does
/// NOT OOM without balance. Callers should memoize the lowered net.
pub fn step_energy(
    step_net: &PhasedCellNetlist,
    char_db: &CharDb,
    t_ramp: f64,
) -> Result<EnergyBreakdown, QueryError> {
    balanced_energy(step_net, char_db, t_ramp)
}

/// Per-clause energy table entry.
#[derive(Clone, Debug)]
pub struct ClauseEnergy {
    /// Human-readable clause label.
    pub label: String,
    /// The energy breakdown for this clause.
    pub breakdown: EnergyBreakdown,
}

/// Per-clause energy report for the TISC-1 machine.
///
/// Contains entries for each of the 14 instructions (at sample register
/// indices), the step components (fetch, decode_execute, pc_inc), and
/// an optional full-step and program trace total.
#[derive(Clone, Debug)]
pub struct ClauseReport {
    /// Per-clause breakdowns.
    pub clauses: Vec<ClauseEnergy>,
    /// Total program energy for a trace, if computed.
    pub program_total_j: Option<f64>,
}

impl fmt::Display for ClauseReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "TISC-1 per-clause energy report")?;
        writeln!(f, "{:-<72}", "")?;
        writeln!(
            f,
            "{:<20} {:>10} {:>12} {:>12} {:>12}",
            "Clause", "Cells", "Active (J)", "Buffer (J)", "Total (J)"
        )?;
        writeln!(f, "{:-<72}", "")?;
        for ce in &self.clauses {
            let cell_count: usize = ce.breakdown.by_kind.values().map(|k| k.count).sum();
            writeln!(
                f,
                "{:<20} {:>10} {:>12.4e} {:>12.4e} {:>12.4e}",
                ce.label,
                cell_count,
                ce.breakdown.active_j,
                ce.breakdown.buffer_j,
                ce.breakdown.total_j,
            )?;
        }
        if let Some(total) = self.program_total_j {
            writeln!(f, "{:-<72}", "")?;
            writeln!(f, "Program total: {:.4e} J", total)?;
        }
        Ok(())
    }
}

/// Build a per-clause energy report for all 14 instructions at fixed
/// sample register indices, plus step components.
///
/// `step_net` is a pre-lowered (unbalanced) step netlist; pass `None` to
/// skip the full-step row. `trace` is an optional program trace (sequence
/// of Ops executed each step) for computing total program energy.
///
/// Because `decode_execute` lowers all 14 predicated instructions every
/// step, per-step energy is approximately instruction-independent (the
/// shared decode dominates); per-clause attribution = the shared
/// fetch/decode/unfetch overhead + each instruction's own gates.
pub fn clause_report(
    rom: &[u32],
    char_db: &CharDb,
    t_ramp: f64,
    step_net: Option<&PhasedCellNetlist>,
    trace: Option<&[crate::decoder::Op]>,
) -> Result<ClauseReport, QueryError> {
    let mut clauses = Vec::new();

    // Per-instruction energy at sample register indices.
    // Use rd=0, rs=1, rt=2, sx=0, imm=1 as representative.
    let instr_isos: Vec<(&str, theseus_core::Iso)> = vec![
        ("ADD r0,r1", crate::instr::add(0, 1)),
        ("SUB r0,r1", crate::instr::sub(0, 1)),
        ("XOR r0,r1", crate::instr::xor(0, 1)),
        ("EXCH r0,r1", crate::instr::exch(0, 1)),
        ("ROTL r0,r1", crate::instr::rotl(0, 1)),
        ("ROTR r0,r1", crate::instr::rotr(0, 1)),
        ("CSWAP r0,r1,r2", crate::instr::cswap(0, 1, 2)),
        ("XCHG r0,s0", crate::instr::xchg(0, 0)),
        ("SWAPBR r0", crate::instr::swapbr(0)),
        ("ADDI r0,#1", crate::instr::addi(0, 1)),
        ("SUBI r0,#1", crate::instr::subi(0, 1)),
        ("XORI r0,#1", crate::instr::xori(0, 1)),
        ("ROTLI r0,#1", crate::instr::rotli(0, 1)),
        ("ROTRI r0,#1", crate::instr::rotri(0, 1)),
    ];

    for (label, iso) in &instr_isos {
        let bd = instruction_energy(iso, char_db, t_ramp)?;
        clauses.push(ClauseEnergy {
            label: label.to_string(),
            breakdown: bd,
        });
    }

    // Step components.
    let fetch_iso = crate::step::fetch(rom);
    let fetch_bd = instruction_energy(&fetch_iso, char_db, t_ramp)?;
    clauses.push(ClauseEnergy {
        label: "fetch".to_string(),
        breakdown: fetch_bd,
    });

    let de_iso = crate::decoder::decode_execute();
    let de_bd = instruction_energy(&de_iso, char_db, t_ramp)?;
    clauses.push(ClauseEnergy {
        label: "decode_execute".to_string(),
        breakdown: de_bd,
    });

    let pci_iso = crate::step::pc_inc();
    let pci_bd = instruction_energy(&pci_iso, char_db, t_ramp)?;
    clauses.push(ClauseEnergy {
        label: "pc_inc".to_string(),
        breakdown: pci_bd,
    });

    // Full-step and program trace.
    let program_total_j = match (step_net, trace) {
        (Some(net), Some(trace)) => {
            let step_bd = step_energy(net, char_db, t_ramp)?;
            let total = step_bd.total_j * trace.len() as f64;
            clauses.push(ClauseEnergy {
                label: "step (full)".to_string(),
                breakdown: step_bd,
            });
            Some(total)
        }
        (Some(net), None) => {
            let step_bd = step_energy(net, char_db, t_ramp)?;
            clauses.push(ClauseEnergy {
                label: "step (full)".to_string(),
                breakdown: step_bd,
            });
            None
        }
        _ => None,
    };

    Ok(ClauseReport {
        clauses,
        program_total_j,
    })
}

// ── M-E: ROM program energy (ADR-0042) ─────────────────────────────

/// Trace result from executing a ROM for B steps.
///
/// `steps_b` = budget (WCET), the number of physical clock cycles.
/// `steps_t` = productive trace length: the first step at which PC enters
/// the dead zone `[rom.len(), 256)` (park by idle; M-D.w).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RomTrace {
    /// Total physical steps (= budget = WCET).
    pub steps_b: usize,
    /// Productive trace length: first step PC enters `[rom_len, 256)`.
    pub steps_t: usize,
}

/// Execute a ROM for `budget` steps (mirroring `run_rom_fast`) and identify
/// the productive trace length T — the first step at which PC enters the
/// dead zone `[rom.len(), 256)`.
///
/// T is defined by PC entering the dead zone, NOT by `decode() == None`
/// (idle decodes to `Some(Addi r0, #0)` — F4).
pub fn rom_trace(rom: &[u32], init: StateVals, budget: usize) -> RomTrace {
    let rom_len = rom.len();
    let mut steps_t = budget; // default: never entered dead zone

    // We need to step manually to observe PC at each step boundary.
    let mut state = init;
    for step_idx in 0..budget {
        // Check PC BEFORE this step executes: if PC is already in the
        // dead zone at the start of step `step_idx`, the first T steps (indices 0..T)
        // were productive.
        let pc = state.pc as usize;
        // A-6.4 widen-PC: dead zone is [rom.len(), 2^W_PC); PC is 16-bit
        // so `pc < PC_ADDR_SPACE` always holds (kept for documentation).
        if pc >= rom_len && pc < PC_ADDR_SPACE {
            steps_t = step_idx;
            break;
        }
        // Execute one step (same logic as run_rom_fast).
        state = run_rom_fast(rom, state, 1);
    }

    RomTrace {
        steps_b: budget,
        steps_t,
    }
}

/// Program-level energy result (ADR-0042 M-E).
///
/// The committed energy model prices STATIC CELL INVENTORY and is
/// DATA-INDEPENDENT: per-step energy is exactly constant across all steps
/// (including idle/park steps). Headline: `total_j = B × step_j` (exact
/// model value, not an approximation).
///
/// NOTE: prices the CPU-only `step(rom)` ring body; the composite
/// `system_step` additionally clocks the memory body (a bounded increment).
#[derive(Clone, Debug)]
pub struct ProgramEnergy {
    /// Total physical energy: `steps_b × step_j`, joules.
    pub total_j: f64,
    /// Productive energy: `steps_t × step_j`, joules.
    pub productive_j: f64,
    /// Physical step count B (= WCET).
    pub steps_b: usize,
    /// Productive step count T (first step PC enters dead zone).
    pub steps_t: usize,
    /// WCET park tax: `(B - T) / B` — fraction of energy spent idling.
    pub park_tax: f64,
    /// Per-step energy (from `balanced_energy` on the fib ROM's own step net).
    pub step_j: f64,
}

/// Energy-price a compiled TISC-1 ROM's execution in joules (ADR-0042 M-E).
///
/// Computes:
/// - `B` = `wcet(iso)` (physical step count, WCET park included)
/// - `T` = productive trace length (first step PC enters dead zone)
/// - `step_j` = `balanced_energy(step(rom), db, t_ramp)` — from THIS ROM's
///   own step netlist (not the 2-word money-plot ROM; F2)
/// - `total_j = B × step_j` (exact; the model is data-independent — F1)
///
/// The step net is lowered from `step(rom)` (CPU-only, consistent with
/// `cpu_money_plot`). Callers should memoize the ~2min lowering externally
/// (e.g. via `OnceLock`).
pub fn rom_program_energy(
    iso: &theseus_core::Iso,
    input: &theseus_core::Value,
    db: &CharDb,
    t_ramp: f64,
    step_net: &PhasedCellNetlist,
) -> Result<ProgramEnergy, RomProgramEnergyError> {
    // Compile the iso to get B = wcet and the ROM.
    let lowered = crate::compile::compile_with_metadata(iso)?;
    let budget = lowered.wcet();
    let rom = crate::asm::assemble(&lowered.instrs)
        .map_err(|e| RomProgramEnergyError::Asm(format!("{e}")))?;

    // Marshal input to initial state.
    let (in_ty, _out_ty) =
        theseus_core::type_of(iso).map_err(|e| RomProgramEnergyError::Type(format!("{e:?}")))?;
    let init = crate::compile::marshal(input, &in_ty);

    // Run trace to find T.
    let trace = rom_trace(&rom, init, budget);

    // Price one step via the pre-lowered step net.
    let step_bd = balanced_energy(step_net, db, t_ramp)?;
    let step_j = step_bd.total_j;

    let steps_b = trace.steps_b;
    let steps_t = trace.steps_t;
    let total_j = steps_b as f64 * step_j;
    let productive_j = steps_t as f64 * step_j;
    let park_tax = if steps_b > 0 {
        (steps_b - steps_t) as f64 / steps_b as f64
    } else {
        0.0
    };

    Ok(ProgramEnergy {
        total_j,
        productive_j,
        steps_b,
        steps_t,
        park_tax,
        step_j,
    })
}

/// Energy-price a program at its AUTO-DERIVED [`Layout`] (B-2.1c auto-derive).
///
/// Unlike [`rom_program_energy`] — which prices against a caller-supplied
/// `step_net` (lowered at whatever layout the caller chose, historically the
/// default `w_data=8`) — this derives the FULL per-program layout from the
/// compiled program ([`crate::compile::derive_full_layout_from_lowered`]:
/// `w_data`/`num_regs` off the source iso + `w_pc` off the `Lowered`'s
/// B+S_max/rom_len, B-2.2c) and lowers the step netlist at that layout
/// (`step_with(layout, rom)`). So a narrow program's PRICED `step_j` reflects
/// BOTH the reduced-`w_data` datapath AND the reduced-`w_pc` fetch/PC adder: a
/// small `Flat(4)` program is priced on the `w_data=4, w_pc=8` ring; a byte
/// (`Flat(8)`) or large program on the `w_data=8, w_pc=16` ring (identical to
/// the default `step(rom)` net).
///
/// Lowering the step net is the expensive step; callers pricing many inputs of
/// the SAME iso should lower once (via
/// [`crate::compile::derive_full_layout_from_lowered`] + `step_with` +
/// `lower_iso`) and call [`rom_program_energy`] with that net. This convenience
/// lowers per call.
pub fn rom_program_energy_auto(
    iso: &theseus_core::Iso,
    input: &theseus_core::Value,
    db: &CharDb,
    t_ramp: f64,
) -> Result<ProgramEnergy, RomProgramEnergyError> {
    let lowered = crate::compile::compile_with_metadata(iso)?;
    let rom = crate::asm::assemble(&lowered.instrs)
        .map_err(|e| RomProgramEnergyError::Asm(format!("{e}")))?;
    // B-2.2c: derive w_pc per program too, so the auto-priced step_j reflects
    // the w_pc=8 datapath for a small program (larger programs stay w_pc=16).
    let layout = crate::compile::derive_full_layout_from_lowered(iso, &lowered);
    let step_iso = crate::step::step_with(&layout, &rom);
    let step_net = theseus_netlist::lower_iso(&step_iso)
        .map_err(|e| RomProgramEnergyError::Asm(format!("lower_iso step_with: {e:?}")))?;
    rom_program_energy(iso, input, db, t_ramp, &step_net)
}

/// Energy-price a compiled TISC-1 ROM with resident-array marshalling (F2-2).
///
/// Mirrors [`rom_program_energy`] but uses `compile_with_metadata_resident`
/// and `marshal_resident` so the memexch home guard is satisfied for ROMs
/// that address resident memory words.
pub fn rom_program_energy_resident(
    iso: &theseus_core::Iso,
    input: &theseus_core::Value,
    resident: usize,
    db: &CharDb,
    t_ramp: f64,
    step_net: &PhasedCellNetlist,
) -> Result<ProgramEnergy, RomProgramEnergyError> {
    let lowered = crate::compile::compile_with_metadata_resident(iso, resident)?;
    let budget = lowered.wcet();
    let rom = crate::asm::assemble(&lowered.instrs)
        .map_err(|e| RomProgramEnergyError::Asm(format!("{e}")))?;

    let (in_ty, _out_ty) =
        theseus_core::type_of(iso).map_err(|e| RomProgramEnergyError::Type(format!("{e:?}")))?;
    let init = crate::compile::marshal_resident(input, &in_ty, resident);

    let trace = rom_trace(&rom, init, budget);

    let step_bd = balanced_energy(step_net, db, t_ramp)?;
    let step_j = step_bd.total_j;

    let steps_b = trace.steps_b;
    let steps_t = trace.steps_t;
    let total_j = steps_b as f64 * step_j;
    let productive_j = steps_t as f64 * step_j;
    let park_tax = if steps_b > 0 {
        (steps_b - steps_t) as f64 / steps_b as f64
    } else {
        0.0
    };

    Ok(ProgramEnergy {
        total_j,
        productive_j,
        steps_b,
        steps_t,
        park_tax,
        step_j,
    })
}

/// Error type for `rom_program_energy`.
#[derive(Debug)]
pub enum RomProgramEnergyError {
    Compile(crate::compile::CompileError),
    Asm(String),
    Type(String),
    CharDb(QueryError),
}

impl From<crate::compile::CompileError> for RomProgramEnergyError {
    fn from(e: crate::compile::CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<QueryError> for RomProgramEnergyError {
    fn from(e: QueryError) -> Self {
        Self::CharDb(e)
    }
}

impl fmt::Display for RomProgramEnergyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compile(e) => write!(f, "compile: {e}"),
            Self::Asm(e) => write!(f, "assemble: {e}"),
            Self::Type(e) => write!(f, "type: {e}"),
            Self::CharDb(e) => write!(f, "chardb: {e}"),
        }
    }
}

impl std::error::Error for RomProgramEnergyError {}
