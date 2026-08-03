//! `iter`/`fold`/`unfold` unroll lowering (ADR-0039).
//!
//! The three v0.2 bounded-loop primitives (ADR-0036/0037) lower to the
//! k-CSWAP cell family by **full unrolling**, exactly like `TraceLoop`
//! (ADR-0026 C2.4): the backend is unroll-only, and every one of these
//! loops carries its iteration bound in its *type* -- `|n| − 1` body
//! applications for `iter n f`, spine depth − 1 for `fold`/`unfold l f`.
//!
//! # The one-hot tag observation
//!
//! Under the dual-rail encoding ([`rail_width`]/[`crate::lower::bits_of`]),
//! both a numeral type `1 + (1 + (... + 0))` and a list-spine type
//! `1 + (b × (1 + (b × (... + 0))))` flatten to a sequence of **tag
//! pairs** (`Left`/nil = TRUE, `Right`/cons = FALSE), one per unroll
//! layer -- interleaved with the element payloads in the spine case.
//! A numeral `k` (or a list of length `k`) encodes as tags that are
//! FALSE at layers `0..k` and TRUE at layer `k`: **one-hot at `k`**,
//! with all-FALSE padding after. So the predicate
//!
//! > "the loop is still running at step `t`"  ⟺  `k ≥ t`  ⟺
//! > `¬tag₀ ∧ ¬tag₁ ∧ ... ∧ ¬tag_{t−1}`
//!
//! is a positive AND of rail-swapped tag reads -- computable with one
//! `K2` AND-reduction cell per step (the `emit_mcnot` borrowed-ancilla
//! trick: `K2(a, b, 0, 1)` puts `a ∧ b` on its first data output), and
//! usable directly as the control for the same tag-promotion machinery
//! `Branch`/`TraceLoop` already use ([`Builder::controlled_apply`]).
//!
//! # Constructions
//!
//! **`iter n f : n × a ↔ n × a`** -- layout `[tag₀ ... tag_{K−1} | x]`,
//! `K = |n|` = rail width of `n`. Emit the control chain
//! `c₁ = ¬tag₀`, `c_{t+1} = c_t ∧ ¬tag_t`, then `K − 1` applications of
//! `f` on `x`, application `t` controlled by `c_t`. A counter value `k`
//! makes exactly `c₁ ... c_k` true, so `f` fires `k` times -- `⟦iter⟧`.
//! The tags thread through the AND cells and control positions
//! unchanged, so the counter is preserved on output.
//!
//! **`fold/unfold l f : l × c ↔ l × c`** -- layout
//! `[tag₀ elem₀ tag₁ elem₁ ... tag_{d−2} elem_{d−2} tag_{d−1} | c]`,
//! `d` = spine depth. Same control chain over the tags; application `t`
//! runs `f` on `[elem_{t−1}, acc]`, writing the rewritten element back
//! into its slot and threading the accumulator. `fold` applies in
//! ascending slot order (head-to-tail, `c₀ → c_k`); `unfold` applies
//! the *same gated steps in descending order* -- for a length-`k` list
//! only steps `k, k−1, ..., 1` fire, i.e. tail-to-head -- which is exactly
//! `⟦unfold⟧`'s data-dependent start position without any extra logic.
//! Inactive slots hold all-FALSE padding and pass through the inactive
//! (control-off) promoted cells untouched.
//!
//! Every emitted cell is a chain `K2` or a body cell promoted by one
//! control (K0→K0, K1→K2, K2→K3, zero-cell rewiring→K1 or R1, R1→the
//! K1-R1-K1 sandwich, ADR-0040), so the result stays within the
//! K0-K3 + R1 vocabulary whenever the body itself lowers to ≤ K2/R1
//! cells -- the same promotion budget as `Branch`.
//!
//! # Verification
//!
//! Differential oracle (`tests/iter_lowering.rs`): for every value of
//! the finite input domain, lower → simulate == `theseus_eval::eval`,
//! plus `validate()`, `balance()`, and the vocabulary cap.

use std::borrow::Cow;
use std::collections::BTreeMap;

use theseus_core::{enumerate_values, PrimDef, Type};

use crate::ir::cell::{CellKind, PhasedCellNetlist, RailPair};
use crate::ir::typed::{FoldDir, NodeKind, TypedNode};
use crate::lower::{
    bits_of, lower_with_ancillas, rail_width, value_from_bits, BitAt, Builder, LowerError,
};

/// Return a *gating-safe* equivalent of `body` for use under
/// [`Builder::controlled_apply`].
///
/// `controlled_apply` promotes an arm's **cells** by one control, but
/// any zero-cell *rewiring* mixed in among cells (e.g. the `swapˣ` in
/// `cx ; swapˣ`) is pure wire routing and cannot be gated -- it would
/// fire even when the loop is done. (An arm that is *entirely*
/// zero-cell is fine: `controlled_apply` synthesizes conditional
/// permutation/NOT cells for that case.)
///
/// So a body is passed through unchanged only when it is
/// - a single `Prim` (its lowering is always either pure wiring or an
///   in-place cell network with identity residual wiring, per the
///   B4.2 templates/synthesis), or
/// - pure wiring (its standalone lowering emits no cells).
///
/// Otherwise the body is *tabulated*: its standalone lowering (already
/// per-instance verified) is evaluated over the whole finite domain and
/// wrapped as a monolithic [`PrimDef`], which then lowers in-place under
/// the gate. This is exhaustive-small by design (ADR-0031/0039); bodies
/// wider than the B4.2 synthesis cap are refused, not guessed at.
fn gate_safe_body(body: &TypedNode) -> Result<Cow<'_, TypedNode>, LowerError> {
    if matches!(body.kind, NodeKind::Prim(_)) {
        return Ok(Cow::Borrowed(body));
    }
    let (net, ancilla_values) = lower_with_ancillas(body)?;
    if net.cells.is_empty() {
        return Ok(Cow::Borrowed(body));
    }
    let w_out = rail_width(&body.output)?;
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&body.input) {
        let mut bits = bits_of(&v, &body.input)?;
        bits.extend_from_slice(&ancilla_values);
        let out_bits = eval_net_untimed(&net, &bits);
        let w = value_from_bits(&body.output, &out_bits[..w_out])?;
        forward.insert(v.clone(), w.clone());
        backward.insert(w, v);
    }
    let def = PrimDef {
        forward_name: "loop_body".into(),
        backward_name: "loop_body_inv".into(),
        input_ty: body.input.clone(),
        output_ty: body.output.clone(),
        forward,
        backward,
        symbolic: None,
    };
    Ok(Cow::Owned(TypedNode {
        input: body.input.clone(),
        output: body.output.clone(),
        kind: NodeKind::Prim(Box::new(def)),
    }))
}

/// Untimed functional evaluation of a standalone cell netlist (token
/// propagation in cell order) -- used only to tabulate loop bodies in
/// [`gate_safe_body`] over their finite domains.
fn eval_net_untimed(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        debug_assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
        v1
    }
    debug_assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        set_pair(&mut line, p, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            set_pair(&mut line, p, v);
        }
    }
    net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
}

impl Builder {
    /// Build the "still iterating" control chain over `tags` (the
    /// flattened one-hot tag pairs of a numeral or list spine):
    /// returns `ctrls`, `ctrls[i] = ¬tag₀ ∧ ... ∧ ¬tag_i` -- the gate for
    /// body application `i + 1` -- one `K2` AND cell per element after
    /// the first (`ctrls[0]` is just the rail-swapped live `tags[0]`).
    ///
    /// Handle discipline (each rail pair is consumed exactly once):
    /// - `tags[1..=len−2]` are consumed by the chain cells and replaced
    ///   with their pass-through copies (still the live tag bits).
    /// - `tags[0]`'s pair is consumed as `ctrls[0]`; **the caller must
    ///   restore `tags[0]` from the control pass-through of `ctrls[0]`'s
    ///   eventual consumer** (rail-swapped back).
    /// - `ctrls[i]` for `i ≥ 1` are ancilla-valued: their eventual
    ///   pass-throughs are garbage.
    /// - `tags[len−1]` (the last layer, never a gate) is untouched.
    fn build_step_controls(&mut self, tags: &mut [BitAt]) -> Vec<BitAt> {
        let steps = tags.len().saturating_sub(1);
        let mut ctrls: Vec<BitAt> = Vec::with_capacity(steps);
        if steps == 0 {
            return ctrls;
        }
        // c₁ = ¬tag₀: the rail-swapped view of the live tag₀ pair.
        ctrls.push(tags[0].rail_swapped());
        for i in 1..steps {
            // c_{i+1} = c_i ∧ ¬tag_i via K2(c_i, ¬tag_i, 0, 1): the
            // swap fires iff both controls are high, so the first data
            // output carries the AND (borrowed-ancilla reduction, as in
            // the B4.2 mcNOT emitter).
            let zero = self.alloc_ancilla(false);
            let one = self.alloc_ancilla(true);
            let outs = self.emit(CellKind::K2, &[ctrls[i - 1], tags[i].rail_swapped(), zero, one]);
            ctrls[i - 1] = outs[0]; // fresh handle, same c_i value
            tags[i] = outs[1].rail_swapped(); // live tag_i restored
            ctrls.push(outs[2]); // c_{i+1}
            self.mark_garbage(outs[3]); // ¬c_{i+1}
        }
        ctrls
    }

    /// Consume a step control's pass-through after its
    /// [`Builder::controlled_apply`]: the consumer of `ctrls[0]`
    /// restores the live `tags[0]` (it carried `¬tag₀`); every other
    /// control was ancilla-valued and its pass-through is garbage.
    fn retire_step_control(&mut self, step: usize, tags: &mut [BitAt], ctrl_out: BitAt) {
        if step == 0 {
            tags[0] = ctrl_out.rail_swapped();
        } else {
            self.mark_garbage(ctrl_out);
        }
    }

    /// Lower `IterLoop { counter, body }` (ADR-0039): `|counter| − 1`
    /// tag-gated applications of the body on the payload; the counter
    /// tags are preserved in place.
    pub(crate) fn lower_iter_loop(
        &mut self,
        counter: &Type,
        body: &TypedNode,
        ins: &[BitAt],
    ) -> Result<Vec<BitAt>, LowerError> {
        // One tag pair per numeral layer: rail_width(numeral_ty(K)) = K.
        let w_k = rail_width(counter)?;
        let w_a = rail_width(&body.input)?;
        // Controlled unrolling needs equal body I/O width (guaranteed
        // for a well-typed endo body; refuse rather than trust).
        if rail_width(&body.output)? != w_a {
            return Err(LowerError::UnsupportedType(body.output.clone()));
        }
        debug_assert_eq!(ins.len(), w_k + w_a);

        let mut tags: Vec<BitAt> = ins[..w_k].to_vec();
        let mut data: Vec<BitAt> = ins[w_k..].to_vec();

        let body = gate_safe_body(body)?;
        let ctrls = self.build_step_controls(&mut tags);
        for (i, ctrl) in ctrls.into_iter().enumerate() {
            let (new_data, ctrl_out) = self.controlled_apply(&body, &data, ctrl, false)?;
            data = new_data;
            self.retire_step_control(i, &mut tags, ctrl_out);
        }

        let mut out = tags;
        out.extend(data);
        Ok(out)
    }

    /// Lower `FoldLoop { spine, dir, body }` (ADR-0039): spine-depth − 1
    /// tag-gated applications of the body on `[element slot, acc]`,
    /// ascending for `fold`, descending for `unfold`; the spine (tags
    /// and rewritten elements) is preserved in place.
    pub(crate) fn lower_fold_loop(
        &mut self,
        spine: &Type,
        dir: FoldDir,
        body: &TypedNode,
        ins: &[BitAt],
    ) -> Result<Vec<BitAt>, LowerError> {
        let w_l = rail_width(spine)?;
        let (w_e, w_c) = match &body.input {
            Type::Prod(e, c) => (rail_width(e)?, rail_width(c)?),
            other => return Err(LowerError::UnsupportedType(other.clone())),
        };
        if rail_width(&body.output)? != w_e + w_c {
            return Err(LowerError::UnsupportedType(body.output.clone()));
        }
        let d = spine_tag_count(spine)?;
        // Layout cross-check: d tag pairs + (d − 1) element slots. A
        // mismatch means `spine` is not the uniform ADR-0037 shape.
        let steps = d - 1; // d ≥ 1: rail_width(spine) succeeded above
        if w_l != d + steps * w_e {
            return Err(LowerError::UnsupportedType(spine.clone()));
        }
        debug_assert_eq!(ins.len(), w_l + w_c);

        // Flattened spine layout: [tag_j at j·(1+w_e)][slot_j after it].
        let stride = 1 + w_e;
        let mut tags: Vec<BitAt> = (0..d).map(|j| ins[j * stride]).collect();
        let mut slots: Vec<Vec<BitAt>> = (0..steps)
            .map(|j| ins[j * stride + 1..j * stride + 1 + w_e].to_vec())
            .collect();
        let mut acc: Vec<BitAt> = ins[w_l..].to_vec();

        let body = gate_safe_body(body)?;
        let ctrls = self.build_step_controls(&mut tags);
        let order: Vec<usize> = match dir {
            FoldDir::Fold => (0..steps).collect(),
            FoldDir::Unfold => (0..steps).rev().collect(),
        };
        for i in order {
            let mut body_ins = slots[i].clone();
            body_ins.extend_from_slice(&acc);
            let (outs, ctrl_out) = self.controlled_apply(&body, &body_ins, ctrls[i], false)?;
            slots[i] = outs[..w_e].to_vec();
            acc = outs[w_e..].to_vec();
            self.retire_step_control(i, &mut tags, ctrl_out);
        }

        // Reassemble: interleaved tags/slots, then the accumulator.
        let mut out = Vec::with_capacity(ins.len());
        for j in 0..steps {
            out.push(tags[j]);
            out.extend_from_slice(&slots[j]);
        }
        out.push(tags[steps]);
        out.extend_from_slice(&acc);
        Ok(out)
    }
}

/// Number of `1 + (b × ...)` layers of a list-spine type -- i.e. the
/// number of tag pairs in its flattened dual-rail encoding. Refuses
/// non-spine shapes (the canonicalizer already rejects them; this is
/// the lowering's local re-check).
fn spine_tag_count(spine: &Type) -> Result<usize, LowerError> {
    let mut d = 0usize;
    let mut cur = spine;
    loop {
        match cur {
            Type::Zero => return Ok(d),
            Type::Sum(l, r) if l.as_ref() == &Type::One => match r.as_ref() {
                Type::Prod(_, rest) => {
                    d += 1;
                    cur = rest;
                }
                _ => return Err(LowerError::UnsupportedType(spine.clone())),
            },
            _ => return Err(LowerError::UnsupportedType(spine.clone())),
        }
    }
}
