//! Kernels for flattened `BvExpr` lane programs (ADR-0052).
//!
//! [`crate::bvexpr::LaneProgram`] compiles a set of `BvExpr` trees once
//! into a single-assignment program of *uniform mux cells* over a slot
//! file of 256-lane values ([`MuxCell`]):
//!
//! ```text
//! slot[dst] = e ^ (s & (t ^ e))      -- mux(s, t, e): t where s, else e
//! ```
//!
//! Each of `s`, `t`, `e` is a slot ref whose top bit reads the slot
//! negated, so `And`, `Or`, `Xor` and `Mux` are all one cell shape and
//! `Not` is free (a flipped polarity bit). These kernels run the cells
//! branch-free: per cell one 16-byte load of the cell, `vpsrad 31` for
//! the three polarity masks, three `vpxor`-with-load reads, three vector
//! ops and one store -- no per-node dispatch (an interpreter's indirect
//! branch mispredicts on most nodes).
//!
//! Slot accesses are unchecked: [`MuxProgram::new`] validates every slot
//! index against the slot count (fields private), and [`MuxProgram::run`]
//! checks the slot file's length before entering a kernel.

use super::SimdLevel;

/// One 256-lane value (`crate::bvexpr::Lane`).
type Lane = [u64; 4];

/// Polarity bit of a slot ref: read the slot negated.
pub(crate) const NEG: u32 = 1 << 31;

/// Largest slot file the kernels address: offsets are computed in 32 bits
/// (`(r << 5)` bytes), which must not drop an index bit.
pub(crate) const MAX_SLOTS: usize = 1 << 26;

/// `slot[dst] = mux(s, t, e)` with `refs = [s, t, e]` (polarity in the top
/// bit). 16 bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, align(16))]
pub(crate) struct MuxCell {
    pub(crate) refs: [u32; 3],
    pub(crate) dst: u32,
}

/// A validated mux-cell program over `n_slots` slots.
///
/// Invariant (checked in [`MuxProgram::new`], kept by privacy): every ref
/// (polarity masked off) and every `dst` is `< n_slots <= MAX_SLOTS`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MuxProgram {
    cells: Vec<MuxCell>,
    n_slots: usize,
}

impl MuxProgram {
    /// # Panics
    /// If `n_slots > MAX_SLOTS` or a cell names a slot `>= n_slots`.
    pub(crate) fn new(cells: Vec<MuxCell>, n_slots: usize) -> Self {
        assert!(
            n_slots <= MAX_SLOTS,
            "{n_slots} slots exceed the kernels' {MAX_SLOTS}-slot limit"
        );
        for (i, c) in cells.iter().enumerate() {
            for s in c.refs.iter().map(|&r| r & !NEG).chain([c.dst]) {
                assert!(
                    (s as usize) < n_slots,
                    "mux cell {i}: slot {s} outside the {n_slots}-slot file"
                );
            }
        }
        MuxProgram { cells, n_slots }
    }

    /// Number of cells.
    pub(crate) fn len(&self) -> usize {
        self.cells.len()
    }

    /// Slots addressed (the slot file's minimum length).
    pub(crate) fn n_slots(&self) -> usize {
        self.n_slots
    }

    /// Run every cell over `slots` at `level`.
    ///
    /// # Panics
    /// If `slots` is shorter than [`Self::n_slots`].
    pub(crate) fn run(&self, level: SimdLevel, slots: &mut [Lane]) {
        assert!(
            slots.len() >= self.n_slots,
            "slot file: {} < {} slots",
            slots.len(),
            self.n_slots
        );
        match level.supported() {
            #[cfg(target_arch = "x86_64")]
            SimdLevel::Avx2 => {
                // SAFETY: `supported()` returns `Avx2` only when AVX2 was
                // detected; every slot index is `< n_slots` (type invariant)
                // and `slots` holds `n_slots` lanes (asserted above).
                unsafe { avx2::run(&self.cells, slots) }
            }
            #[cfg(target_arch = "x86_64")]
            SimdLevel::Sse2 => {
                // SAFETY: SSE2 was detected (the x86-64 baseline); slot
                // indices and length as above.
                unsafe { sse2::run(&self.cells, slots) }
            }
            _ => scalar_run(&self.cells, slots),
        }
    }
}

/// Lane `r` reads: slot `r & !NEG`, negated iff `r`'s top bit is set.
#[inline(always)]
fn read(slots: &[Lane], r: u32) -> Lane {
    let neg = 0u64.wrapping_sub(u64::from(r >> 31));
    slots[(r & !NEG) as usize].map(|w| w ^ neg)
}

/// Portable flavour: the same cells on `[u64; 4]`, checked indexing.
fn scalar_run(cells: &[MuxCell], slots: &mut [Lane]) {
    for c in cells {
        let [s, t, e] = c.refs.map(|r| read(slots, r));
        slots[c.dst as usize] = std::array::from_fn(|k| e[k] ^ (s[k] & (t[k] ^ e[k])));
    }
}

/// Byte offset of slot `r` (32-byte slots): the 32-bit shift drops the
/// polarity bit, and slots stay below `MAX_SLOTS = 2^26`.
#[cfg(target_arch = "x86_64")]
#[inline(always)]
fn byte_offset(r: u32) -> usize {
    (r << 5) as usize
}

/// 128-bit flavour: a slot is two `__m128i`.
#[cfg(target_arch = "x86_64")]
mod sse2 {
    use super::{byte_offset, Lane, MuxCell};
    use std::arch::x86_64::*;

    /// Slot `r` XORed with `mask`.
    ///
    /// # Safety
    /// SSE2 must be available and slot `r` must lie inside the slot file
    /// at `base`.
    #[target_feature(enable = "sse2")]
    #[inline]
    unsafe fn rd(base: *const u8, r: u32, mask: __m128i) -> (__m128i, __m128i) {
        // SAFETY: the caller guarantees the 32-byte slot is in bounds.
        let p = unsafe { base.add(byte_offset(r)) }.cast::<__m128i>();
        // SAFETY: both halves of the slot are in bounds; `loadu` has no
        // alignment requirement.
        let (lo, hi) = unsafe { (_mm_loadu_si128(p), _mm_loadu_si128(p.add(1))) };
        (_mm_xor_si128(lo, mask), _mm_xor_si128(hi, mask))
    }

    /// `e ^ (s & (t ^ e))` on one half.
    #[target_feature(enable = "sse2")]
    #[inline]
    fn mux(s: __m128i, t: __m128i, e: __m128i) -> __m128i {
        _mm_xor_si128(e, _mm_and_si128(s, _mm_xor_si128(t, e)))
    }

    /// # Safety
    /// SSE2 must be available and every slot named by `cells` must lie
    /// inside `slots`.
    #[target_feature(enable = "sse2")]
    pub(super) unsafe fn run(cells: &[MuxCell], slots: &mut [Lane]) {
        let base = slots.as_mut_ptr().cast::<u8>();
        for c in cells {
            // SAFETY: `c` is a 16-byte `#[repr(C)]` `MuxCell`.
            let cell = unsafe { _mm_loadu_si128((c as *const MuxCell).cast()) };
            let neg = _mm_srai_epi32(cell, 31);
            let (ms, mt, me) = (
                _mm_shuffle_epi32(neg, 0x00),
                _mm_shuffle_epi32(neg, 0x55),
                _mm_shuffle_epi32(neg, 0xAA),
            );
            // SAFETY: every slot of `c` (three reads, one write) is in
            // bounds of `slots` (the caller's contract).
            unsafe {
                let s = rd(base, c.refs[0], ms);
                let t = rd(base, c.refs[1], mt);
                let e = rd(base, c.refs[2], me);
                let p = base.add(byte_offset(c.dst)).cast::<__m128i>();
                _mm_storeu_si128(p, mux(s.0, t.0, e.0));
                _mm_storeu_si128(p.add(1), mux(s.1, t.1, e.1));
            }
        }
    }
}

/// 256-bit flavour: a slot is one `__m256i`.
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use super::{byte_offset, Lane, MuxCell};
    use std::arch::x86_64::*;

    /// Slot `r` XORed with `mask`.
    ///
    /// # Safety
    /// AVX2 must be available and slot `r` must lie inside the slot file
    /// at `base`.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn rd(base: *const u8, r: u32, mask: __m256i) -> __m256i {
        // SAFETY: the caller guarantees the 32-byte slot is in bounds;
        // `loadu` has no alignment requirement.
        let v = unsafe { _mm256_loadu_si256(base.add(byte_offset(r)).cast()) };
        _mm256_xor_si256(v, mask)
    }

    /// The mux-cell loop: per cell a 16-byte cell load, `vpsrad` +
    /// broadcasts for the three polarity masks, three masked loads,
    /// `vpxor`/`vpand`/`vpxor`, one store.
    ///
    /// # Safety
    /// AVX2 must be available and every slot named by `cells` must lie
    /// inside `slots`.
    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn run(cells: &[MuxCell], slots: &mut [Lane]) {
        let base = slots.as_mut_ptr().cast::<u8>();
        for c in cells {
            // SAFETY: `c` is a 16-byte `#[repr(C)]` `MuxCell`.
            let cell = unsafe { _mm_loadu_si128((c as *const MuxCell).cast()) };
            let neg = _mm_srai_epi32(cell, 31);
            let ms = _mm256_broadcastd_epi32(neg);
            let mt = _mm256_broadcastd_epi32(_mm_shuffle_epi32(neg, 0x55));
            let me = _mm256_broadcastd_epi32(_mm_shuffle_epi32(neg, 0xAA));
            // SAFETY: every slot of `c` (three reads, one write) is in
            // bounds of `slots` (the caller's contract).
            unsafe {
                let s = rd(base, c.refs[0], ms);
                let t = rd(base, c.refs[1], mt);
                let e = rd(base, c.refs[2], me);
                let r = _mm256_xor_si256(e, _mm256_and_si256(s, _mm256_xor_si256(t, e)));
                _mm256_storeu_si256(base.add(byte_offset(c.dst)).cast(), r);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mux_program_rejects_out_of_range_slots() {
        let ok = MuxCell {
            refs: [1, 2 | NEG, 0],
            dst: 3,
        };
        MuxProgram::new(vec![ok], 4);
        for bad in [
            MuxCell {
                refs: [4, 2, 0],
                dst: 3,
            },
            MuxCell {
                refs: [1, 4 | NEG, 0],
                dst: 3,
            },
            MuxCell {
                refs: [1, 2, 0],
                dst: 4,
            },
        ] {
            assert!(std::panic::catch_unwind(|| MuxProgram::new(vec![bad], 4)).is_err());
        }
        let p = MuxProgram::new(vec![ok], 4);
        for level in SimdLevel::available() {
            let r = std::panic::catch_unwind(|| p.run(level, &mut [[0; 4]; 3]));
            assert!(r.is_err(), "{level}: short slot file");
        }
    }

    /// Every level computes `e ^ (s & (t ^ e))` with every polarity.
    #[test]
    fn mux_cell_every_polarity_every_level() {
        let mut rng = super::super::SplitMix64(0x30C5);
        for pol in 0..8u32 {
            let refs = [1u32, 2, 3].map(|r| r | if (pol >> (r - 1)) & 1 == 1 { NEG } else { 0 });
            let p = MuxProgram::new(vec![MuxCell { refs, dst: 4 }], 5);
            let init: Vec<Lane> = (0..5)
                .map(|_| std::array::from_fn(|_| rng.next_u64()))
                .collect();
            let mut want = init.clone();
            scalar_run(&p.cells, &mut want);
            let [s, t, e] = refs.map(|r| read(&init, r));
            let expect: Lane = std::array::from_fn(|k| (s[k] & t[k]) | (!s[k] & e[k]));
            assert_eq!(want[4], expect);
            for level in SimdLevel::available() {
                let mut got = init.clone();
                p.run(level, &mut got);
                assert_eq!(got, want, "{level}, polarity {pol:03b}");
            }
        }
    }
}
