//! Explicit SIMD kernels for the compact lane simulator (ADR-0052).
//!
//! [`crate::sim_lanes`]'s `CompactPlan` is a flat single-assignment slot
//! program; its portable hot loop branches per cell on the control count
//! (1-3) and on swap-vs-flip. These kernels run a *uniform*, branch-free
//! re-encoding of the same plan ([`UniformPlan`]):
//!
//! - every cell reads 3 controls + 2 targets: a missing control reads the
//!   constant all-ones slot (`ZERO | NEG`), and a flip `t ^= s` is the
//!   cswap of `t` with `!t` whose second output lands in a write-only
//!   sink slot -- so the loop body is one straight-line block;
//! - one slot is one `__m256i` (`N = 4`, 256 lanes) or two (`N = 8`); SSE2
//!   uses one `__m128i` (`N = 2`), two (`N = 4`) or four (`N = 8`);
//! - each ref's polarity bit becomes an all-ones/all-zeros XOR mask,
//!   broadcast from the cell itself (one load of the 32-byte cell,
//!   `vpsrad 31`, then `vpermd` / `pshufd` per ref);
//! - loads and stores are unaligned (`vmovdqu`), at slot offsets the plan
//!   validated once at construction;
//! - cells run in ASAP level order ([`UniformPlan::level_order`]), so
//!   neighbouring cells are independent and their store-to-load chains
//!   overlap.
//!
//! The unchecked slot accesses are the one place in the simulators that
//! skips bounds checks: [`UniformPlan::new`] checks every slot index
//! against the slot count, the fields are private, and
//! [`UniformPlan::run`] checks the slot file's length before entering a
//! kernel.

#![allow(unsafe_code)]
#![deny(clippy::undocumented_unsafe_blocks)]
#![deny(unsafe_op_in_unsafe_fn)]

use theseus_core::simd::SimdLevel;

/// A slot reference: slot index, top bit = read negated (as in
/// `sim_lanes`).
pub(crate) const NEG: u32 = 1 << 31;

/// One cell of the uniform encoding: `s = c0 & c1 & c2`, then
/// `(t0, t1) = cswap(s, t0, t1)` written to `outs`. 32 bytes, so a cell
/// never straddles a cache line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, align(32))]
pub(crate) struct UCell {
    /// `[c0, c1, c2, t0, t1]` slot refs (polarity in the top bit).
    refs: [u32; 5],
    /// Output slots for `t0'`, `t1'` (no polarity).
    outs: [u32; 2],
    _pad: u32,
}

impl UCell {
    /// A cell from its (unchecked) refs and outputs; [`UniformPlan::new`]
    /// validates them.
    pub(crate) fn new(refs: [u32; 5], outs: [u32; 2]) -> Self {
        UCell {
            refs,
            outs,
            _pad: 0,
        }
    }
}

/// A validated uniform cell program over a slot file of `n_slots` slots.
///
/// Invariant (checked in [`UniformPlan::new`], kept by privacy): every
/// ref (polarity bit masked off) and every output of every cell is
/// `< n_slots`.
#[derive(Clone, Debug)]
pub(crate) struct UniformPlan {
    cells: Vec<UCell>,
    n_slots: usize,
}

/// Largest slot file the kernels address: slot offsets are computed in
/// 32 bits (`byte_offset`), which must not drop an index bit.
pub(crate) const MAX_SLOTS: usize = 1 << 26;

impl UniformPlan {
    /// # Panics
    /// If `n_slots > MAX_SLOTS` or any cell names a slot `>= n_slots`.
    pub(crate) fn new(cells: Vec<UCell>, n_slots: usize) -> Self {
        assert!(
            n_slots <= MAX_SLOTS,
            "{n_slots} slots exceed the kernels' {MAX_SLOTS}-slot limit"
        );
        for (i, c) in cells.iter().enumerate() {
            let refs = c.refs.iter().map(|&r| r & !NEG);
            for s in refs.chain(c.outs) {
                assert!(
                    (s as usize) < n_slots,
                    "uniform cell {i}: slot {s} outside the {n_slots}-slot file"
                );
            }
        }
        UniformPlan { cells, n_slots }
    }

    /// Slots the program addresses (the slot file needs `n_slots * n` words).
    pub(crate) fn n_slots(&self) -> usize {
        self.n_slots
    }

    /// Reorder the cells by ASAP level (1 + the deepest level among the
    /// slots a cell reads; slots no cell writes are level 0), keeping
    /// program order within a level.
    ///
    /// For a single-assignment program -- every slot read is written by
    /// an earlier cell or by nobody, and only never-read slots (the sink)
    /// are written twice -- any topological order computes the same
    /// slots, and this one puts independent cells next to each other: a
    /// cell's loads then rarely wait on the store of the cell just
    /// before it, so out-of-order execution overlaps neighbouring cells.
    pub(crate) fn level_order(mut self) -> Self {
        let mut level = vec![0u32; self.n_slots];
        let mut keyed: Vec<(u32, UCell)> = self
            .cells
            .iter()
            .map(|c| {
                let l = 1 + c
                    .refs
                    .iter()
                    .map(|&r| level[(r & !NEG) as usize])
                    .max()
                    .unwrap_or(0);
                for &o in &c.outs {
                    level[o as usize] = l;
                }
                (l, *c)
            })
            .collect();
        keyed.sort_by_key(|&(l, _)| l);
        self.cells = keyed.into_iter().map(|(_, c)| c).collect();
        self
    }

    /// Is there a SIMD kernel for `n`-word blocks at `level`?
    pub(crate) fn has_kernel(level: SimdLevel, n: usize) -> bool {
        match level.supported() {
            SimdLevel::Avx2 => matches!(n, 2 | 4 | 8),
            SimdLevel::Sse2 => matches!(n, 2 | 4 | 8),
            SimdLevel::Scalar => false,
        }
    }

    /// Run every cell over `slots` (`n`-word blocks per slot) with the
    /// `level` kernel. Returns `false`, touching nothing, when there is no
    /// kernel for `(level, n)` ([`Self::has_kernel`]).
    ///
    /// # Panics
    /// If `slots` is shorter than `n_slots * n` words.
    pub(crate) fn run(&self, level: SimdLevel, n: usize, slots: &mut [u64]) -> bool {
        assert!(
            slots.len() >= self.n_slots * n,
            "slot file: {} words < {} slots x {n}",
            slots.len(),
            self.n_slots
        );
        let cells = &self.cells[..];
        match (level.supported(), n) {
            #[cfg(target_arch = "x86_64")]
            (SimdLevel::Avx2, 4) => {
                // SAFETY: `supported()` returned `Avx2`, so AVX2 was detected;
                // every slot index is `< n_slots` (type invariant) and
                // `slots` holds `n_slots * 4` words (asserted above).
                unsafe { avx2::run::<1>(cells, slots) }
            }
            #[cfg(target_arch = "x86_64")]
            (SimdLevel::Avx2, 8) => {
                // SAFETY: as above, with `n_slots * 8` words.
                unsafe { avx2::run::<2>(cells, slots) }
            }
            #[cfg(target_arch = "x86_64")]
            (SimdLevel::Sse2 | SimdLevel::Avx2, 2) => {
                // SAFETY: SSE2 was detected (the x86-64 baseline); slot
                // indices and length as above, with `n_slots * 2` words.
                unsafe { sse2::run::<1>(cells, slots) }
            }
            #[cfg(target_arch = "x86_64")]
            (SimdLevel::Sse2, 4) => {
                // SAFETY: as above, with `n_slots * 4` words.
                unsafe { sse2::run::<2>(cells, slots) }
            }
            #[cfg(target_arch = "x86_64")]
            (SimdLevel::Sse2, 8) => {
                // SAFETY: as above, with `n_slots * 8` words.
                unsafe { sse2::run::<4>(cells, slots) }
            }
            _ => return false,
        }
        true
    }

    /// The uniform program in portable Rust (checked indexing): isolates
    /// the branch-free encoding from the vector width in tests and benches.
    pub(crate) fn run_portable<const N: usize>(&self, slots: &mut [u64]) {
        for c in &self.cells {
            let mut v = [[0u64; N]; 5];
            for (vi, &r) in v.iter_mut().zip(&c.refs) {
                let neg = 0u64.wrapping_sub(u64::from(r >> 31));
                let src = &slots[(r & !NEG) as usize * N..][..N];
                for (d, &s) in vi.iter_mut().zip(src) {
                    *d = s ^ neg;
                }
            }
            let [c0, c1, c2, t0, t1] = v;
            let (mut a, mut b) = ([0u64; N], [0u64; N]);
            for k in 0..N {
                let d = (t0[k] ^ t1[k]) & c0[k] & c1[k] & c2[k];
                a[k] = t0[k] ^ d;
                b[k] = t1[k] ^ d;
            }
            slots[c.outs[0] as usize * N..][..N].copy_from_slice(&a);
            slots[c.outs[1] as usize * N..][..N].copy_from_slice(&b);
        }
    }
}

/// Byte offset of slot `r` in a file of `bytes`-byte slots (`bytes` a
/// power of two, at most 64). The 32-bit shift by 4 discards the polarity
/// bit along with every bit at or above 28, and `UniformPlan::new` keeps
/// slot indices below `MAX_SLOTS = 2^26`, so no index bit is lost; the
/// remaining scale folds into the x86 addressing mode.
#[inline(always)]
fn byte_offset(r: u32, bytes: usize) -> usize {
    ((r << 4) as usize) * (bytes / 16)
}

/// 256-bit kernels: `V` `__m256i` per slot (`N = 4 V` words).
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use super::{byte_offset, UCell};
    use std::arch::x86_64::*;

    /// Load slot `r` (`V` registers from `base`) XORed with `mask`.
    ///
    /// # Safety
    /// AVX2 must be available, and the slot (`32 V` bytes at
    /// `byte_offset(r, 32 V)`) must lie inside the allocation at `base`.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn rd<const V: usize>(base: *const u8, r: u32, mask: __m256i) -> [__m256i; V] {
        // SAFETY: the caller guarantees the slot is in bounds.
        let p = unsafe { base.add(byte_offset(r, 32 * V)) }.cast::<__m256i>();
        let mut v = [_mm256_setzero_si256(); V];
        for (i, vi) in v.iter_mut().enumerate() {
            // SAFETY: register `i` of the slot is in bounds (caller); `loadu`
            // has no alignment requirement.
            *vi = _mm256_xor_si256(unsafe { _mm256_loadu_si256(p.add(i)) }, mask);
        }
        v
    }

    /// Store `v` to slot `o`.
    ///
    /// # Safety
    /// AVX2 must be available, and the slot must lie inside the
    /// allocation at `base`.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn wr<const V: usize>(base: *mut u8, o: u32, v: [__m256i; V]) {
        // SAFETY: the caller guarantees the slot is in bounds.
        let p = unsafe { base.add(byte_offset(o, 32 * V)) }.cast::<__m256i>();
        for (i, &vi) in v.iter().enumerate() {
            // SAFETY: register `i` of the slot is in bounds (caller); `storeu`
            // has no alignment requirement.
            unsafe { _mm256_storeu_si256(p.add(i), vi) };
        }
    }

    /// The uniform cell loop. Per cell: one 32-byte load of the cell
    /// itself, whose `vpsrad 31` turns each ref's polarity bit into an
    /// all-ones/all-zeros dword, `vpermd` broadcasting each ref's mask
    /// across the register; then 5 `vpxor`-with-load reads, 2 `vpand`, 3
    /// `vpxor` and 2 stores.
    ///
    /// # Safety
    /// AVX2 must be available, and every slot named by `cells` must lie
    /// inside `slots` (`(slot + 1) * 4 V <= slots.len()`).
    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn run<const V: usize>(cells: &[UCell], slots: &mut [u64]) {
        let base = slots.as_mut_ptr().cast::<u8>();
        let pick = [
            _mm256_set1_epi32(0),
            _mm256_set1_epi32(1),
            _mm256_set1_epi32(2),
            _mm256_set1_epi32(3),
            _mm256_set1_epi32(4),
        ];
        for c in cells {
            // SAFETY: `c` is a `#[repr(C)]` 32-byte `UCell` (5 + 2 + 1 u32s),
            // so the 32-byte read stays inside it.
            let cell = unsafe { _mm256_loadu_si256((c as *const UCell).cast()) };
            let neg = _mm256_srai_epi32(cell, 31);
            let masks = [
                _mm256_permutevar8x32_epi32(neg, pick[0]),
                _mm256_permutevar8x32_epi32(neg, pick[1]),
                _mm256_permutevar8x32_epi32(neg, pick[2]),
                _mm256_permutevar8x32_epi32(neg, pick[3]),
                _mm256_permutevar8x32_epi32(neg, pick[4]),
            ];
            // SAFETY: every slot of `c` (all five reads here, both writes
            // below) is in bounds of `slots` (the caller's contract).
            let [c0, c1, c2, t0, t1] = unsafe {
                [
                    rd::<V>(base, c.refs[0], masks[0]),
                    rd::<V>(base, c.refs[1], masks[1]),
                    rd::<V>(base, c.refs[2], masks[2]),
                    rd::<V>(base, c.refs[3], masks[3]),
                    rd::<V>(base, c.refs[4], masks[4]),
                ]
            };
            let mut a = [_mm256_setzero_si256(); V];
            let mut b = [_mm256_setzero_si256(); V];
            for i in 0..V {
                let s = _mm256_and_si256(_mm256_and_si256(c0[i], c1[i]), c2[i]);
                let d = _mm256_and_si256(_mm256_xor_si256(t0[i], t1[i]), s);
                a[i] = _mm256_xor_si256(t0[i], d);
                b[i] = _mm256_xor_si256(t1[i], d);
            }
            // SAFETY: as above.
            unsafe {
                wr::<V>(base, c.outs[0], a);
                wr::<V>(base, c.outs[1], b);
            }
        }
    }
}

/// 128-bit kernels: `V` `__m128i` per slot (`N = 2 V` words).
#[cfg(target_arch = "x86_64")]
mod sse2 {
    use super::{byte_offset, UCell};
    use std::arch::x86_64::*;

    /// As the AVX2 `rd`, with `V` `__m128i`.
    ///
    /// # Safety
    /// SSE2 must be available, and the slot (`16 V` bytes at
    /// `byte_offset(r, 16 V)`) must lie inside the allocation at `base`.
    #[target_feature(enable = "sse2")]
    #[inline]
    unsafe fn rd<const V: usize>(base: *const u8, r: u32, mask: __m128i) -> [__m128i; V] {
        // SAFETY: the caller guarantees the slot is in bounds.
        let p = unsafe { base.add(byte_offset(r, 16 * V)) }.cast::<__m128i>();
        let mut v = [_mm_setzero_si128(); V];
        for (i, vi) in v.iter_mut().enumerate() {
            // SAFETY: register `i` of the slot is in bounds (caller); `loadu`
            // has no alignment requirement.
            *vi = _mm_xor_si128(unsafe { _mm_loadu_si128(p.add(i)) }, mask);
        }
        v
    }

    /// # Safety
    /// SSE2 must be available, and the slot must lie inside the
    /// allocation at `base`.
    #[target_feature(enable = "sse2")]
    #[inline]
    unsafe fn wr<const V: usize>(base: *mut u8, o: u32, v: [__m128i; V]) {
        // SAFETY: the caller guarantees the slot is in bounds.
        let p = unsafe { base.add(byte_offset(o, 16 * V)) }.cast::<__m128i>();
        for (i, &vi) in v.iter().enumerate() {
            // SAFETY: register `i` of the slot is in bounds (caller); `storeu`
            // has no alignment requirement.
            unsafe { _mm_storeu_si128(p.add(i), vi) };
        }
    }

    /// The uniform cell loop; polarity masks as in the AVX2 kernel, with
    /// `psrad` on the cell's two halves and `pshufd` broadcasts.
    ///
    /// # Safety
    /// SSE2 must be available, and every slot named by `cells` must lie
    /// inside `slots` (`(slot + 1) * 2 V <= slots.len()`).
    #[target_feature(enable = "sse2")]
    pub(super) unsafe fn run<const V: usize>(cells: &[UCell], slots: &mut [u64]) {
        let base = slots.as_mut_ptr().cast::<u8>();
        for c in cells {
            let p = (c as *const UCell).cast::<__m128i>();
            // SAFETY: `c` is a `#[repr(C)]` 32-byte `UCell`, so both 16-byte
            // reads stay inside it.
            let (lo, hi) = unsafe { (_mm_loadu_si128(p), _mm_loadu_si128(p.add(1))) };
            let (nlo, nhi) = (_mm_srai_epi32(lo, 31), _mm_srai_epi32(hi, 31));
            let masks = [
                _mm_shuffle_epi32(nlo, 0x00),
                _mm_shuffle_epi32(nlo, 0x55),
                _mm_shuffle_epi32(nlo, 0xAA),
                _mm_shuffle_epi32(nlo, 0xFF),
                _mm_shuffle_epi32(nhi, 0x00),
            ];
            // SAFETY: every slot of `c` (all five reads here, both writes
            // below) is in bounds of `slots` (the caller's contract).
            let [c0, c1, c2, t0, t1] = unsafe {
                [
                    rd::<V>(base, c.refs[0], masks[0]),
                    rd::<V>(base, c.refs[1], masks[1]),
                    rd::<V>(base, c.refs[2], masks[2]),
                    rd::<V>(base, c.refs[3], masks[3]),
                    rd::<V>(base, c.refs[4], masks[4]),
                ]
            };
            let mut a = [_mm_setzero_si128(); V];
            let mut b = [_mm_setzero_si128(); V];
            for i in 0..V {
                let s = _mm_and_si128(_mm_and_si128(c0[i], c1[i]), c2[i]);
                let d = _mm_and_si128(_mm_xor_si128(t0[i], t1[i]), s);
                a[i] = _mm_xor_si128(t0[i], d);
                b[i] = _mm_xor_si128(t1[i], d);
            }
            // SAFETY: as above.
            unsafe {
                wr::<V>(base, c.outs[0], a);
                wr::<V>(base, c.outs[1], b);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uniform_plan_rejects_out_of_range_slots() {
        let ok = UCell::new([0, 1 | NEG, 2, 3, 3 | NEG], [4, 5]);
        UniformPlan::new(vec![ok], 6);
        for bad in [
            UCell::new([0, 1, 2, 3, 6], [4, 5]),
            UCell::new([0, 1, 6 | NEG, 3, 3], [4, 5]),
            UCell::new([0, 1, 2, 3, 3], [4, 6]),
        ] {
            let r = std::panic::catch_unwind(|| UniformPlan::new(vec![bad], 6));
            assert!(r.is_err(), "{bad:?}");
        }
        let plan = UniformPlan::new(vec![ok], 6);
        let r = std::panic::catch_unwind(move || plan.run(SimdLevel::Avx2, 4, &mut [0; 23]));
        assert!(r.is_err(), "short slot file");
    }
}
