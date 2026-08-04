/-
Build-time soundness gate.

`lake build` by itself does NOT fail on `sorry` (it only warns), and
`#print axioms` is a manual command whose output nobody is forced to read. This
module closes both holes: it programmatically collects the axiom dependencies of
every key theorem and FAILS ELABORATION -- hence `lake build`, hence CI -- if any
of them leaves the whitelisted classical base `{propext, Classical.choice,
Quot.sound}`. A stray `sorry` (`sorryAx`), a `native_decide` (`Lean.ofReduceBool`),
or any other axiom turns the build red.

Because this module IMPORTS the proof modules, Lake re-elaborates it whenever a
proof changes, so the check cannot go stale relative to the proofs. CI runs
`lake build` on a fresh checkout (no cached `.lake/build`), so the proofs and this
gate are always rebuilt from source. Soundness is therefore enforced by the
build, not by anyone remembering to run a command.
-/
import TheseusProofs.Reversibility
import TheseusProofs.FiniteTrace

open Lean Elab Command

namespace Theseus.AxiomGuard

/-- Fail the build unless every named declaration depends only on the classical
    base axioms. Names are given as resolved name-literals, so a typo or a removed
    theorem is itself a compile error. -/
elab "#assert_only_base_axioms " : command => do
  let allowed : Array Name := #[``propext, ``Classical.choice, ``Quot.sound]
  let targets : Array Name := #[
    -- Reversibility (Tier 1 + Tier 2, incl. the genuine accumulator fold)
    ``Theseus.Reversibility.denote_bijective,
    ``Theseus.Reversibility.denote_inv,
    ``Theseus.Reversibility.denote_inv_leftInverse,
    ``Theseus.Reversibility.denote_inv_rightInverse,
    ``Theseus.Reversibility.inv_inv,
    ``Theseus.Reversibility.seq_inv,
    ``Theseus.Reversibility.iter_succ,
    ``Theseus.Reversibility.iter_zero,
    ``Theseus.Reversibility.iter_bijective,
    ``Theseus.Reversibility.fold_unfold_inv,
    ``Theseus.Reversibility.unfold_fold_inv,
    ``Theseus.Reversibility.foldBits_bijective,
    ``Theseus.Reversibility.unfold_fold_acc,
    ``Theseus.Reversibility.fold_unfold_acc,
    ``Theseus.Reversibility.foldAcc_bijective,
    ``Theseus.Reversibility.comefrom_involution,
    ``Theseus.Reversibility.swapbrIso_involution,
    ``Theseus.Reversibility.notI_involution,
    -- FiniteTrace (TH-1 forward core + mutual inverse + per-input class-wide bound)
    ``Theseus.FiniteTrace.state_injOn,
    ``Theseus.FiniteTrace.trace_terminates,
    ``Theseus.FiniteTrace.trace_terminates_of_equiv,
    ``Theseus.FiniteTrace.hamF_underfueled,
    ``Theseus.FiniteTrace.hamF_exits,
    ``Theseus.FiniteTrace.run_some_of_steps,
    ``Theseus.FiniteTrace.steps_of_run_some,
    ``Theseus.FiniteTrace.steps_reaches,
    ``Theseus.FiniteTrace.steps_output_unique,
    ``Theseus.FiniteTrace.trace_mutual_inverse,
    ``Theseus.FiniteTrace.run_symm_entry,
    ``Theseus.FiniteTrace.traj_cross_injective,
    ``Theseus.FiniteTrace.traj_disjoint,
    ``Theseus.FiniteTrace.traj_card_bound ]
  for t in targets do
    let axs ← liftCoreM <| collectAxioms t
    let bad := axs.filter (fun a =>
      a != ``propext && a != ``Classical.choice && a != ``Quot.sound)
    unless bad.isEmpty do
      throwError
        "AXIOM GUARD failed: `{t}` depends on non-base axiom(s) {bad}. \
         Allowed base only: {allowed}. A `sorry`/`native_decide`/new axiom is not permitted."
  logInfo m!"axiom guard OK: {targets.size} theorems reduce to the classical base \
    (propext, Classical.choice, Quot.sound); no sorry/native_decide."

#assert_only_base_axioms

end Theseus.AxiomGuard
