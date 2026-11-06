//! Packed Möbius (Reed–Muller) transform kernels (ADR-0052).
//!
//! A truth table over `n` variables is packed 64 rows per word (row `x`
//! is bit `x & 63` of word `x >> 6`). Stage `k` XORs the low half of
//! every stride-`2^k` butterfly into the high half:
//!
//! - `k < 6` -- inside a word: `w ^= (w & MASK[k]) << 2^k`;
//! - `k >= 6` -- across words, `half = 2^(k-6)` words apart.
//!
//! The scalar reference (theseus-tisc's original loop) makes one pass over
//! the table per stage. The SIMD kernels make far fewer:
//!
//! 1. one pass applies, per register, the six in-word stages (`vpand`,
//!    `vpsllq`, `vpxor`), the `half = 1` and `half = 2` stages by in-register
//!    shuffles (`vpslldq` by 8 bytes, `vperm2i128` lane move; SSE2 only has
//!    the `half = 1` one in-register), and then -- holding up to 8
//!    registers -- the next three cross-register stages;
//! 2. the remaining `half >= 4` stages go three at a time (a radix-8
//!    butterfly on 8 registers per column), so a 2^24-row table takes 6
//!    passes instead of 24.
//!
//! The pass fusion is part of the speedup; the benches report it against
//! the one-pass-per-stage scalar loop.

use super::SimdLevel;

/// `MASKS[k]` selects the rows of a word whose index has bit `k` clear --
/// the low halves of the stride-`2^k` butterfly.
const MASKS: [u64; 6] = [
    0x5555_5555_5555_5555,
    0x3333_3333_3333_3333,
    0x0F0F_0F0F_0F0F_0F0F,
    0x00FF_00FF_00FF_00FF,
    0x0000_FFFF_0000_FFFF,
    0x0000_0000_FFFF_FFFF,
];

/// In-place Möbius transform of a packed truth table over `n` variables:
/// on return row `S` holds `XOR_{T ⊆ S} f[T]`. `c` is `2^n` rows packed 64
/// per word (for `n < 6`, one word whose rows `2^n..` are zero and stay
/// zero).
///
/// # Panics
/// If `c.len() != (2^n).div_ceil(64)`.
pub fn mobius_transform(c: &mut [u64], n: usize) {
    mobius_transform_with(super::level(), c, n);
}

/// [`mobius_transform`] at a given level (capped at what the CPU has).
pub fn mobius_transform_with(level: SimdLevel, c: &mut [u64], n: usize) {
    assert!(n < 58, "mobius_transform: 2^{n} rows");
    assert_eq!(
        c.len(),
        (1usize << n).div_ceil(64),
        "mobius_transform: {} words for {n} variables",
        c.len()
    );
    // From here `c.len()` is a power of two, and for `c.len() >= 2` (n >= 7)
    // every in-word stage applies, so the SIMD kernels need not know `n`.
    match level.supported() {
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Avx2 if c.len() >= 4 => {
            // SAFETY: `supported()` returns `Avx2` only when AVX2 was detected.
            unsafe { avx2::mobius(c) }
        }
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Sse2 | SimdLevel::Avx2 if c.len() >= 2 => {
            // SAFETY: `supported()` returns `Sse2` or above only when SSE2 was
            // detected (always, on x86-64).
            unsafe { sse2::mobius(c) }
        }
        _ => scalar(c, n),
    }
}

/// The portable reference: one pass per stage (moved verbatim from
/// theseus-tisc `synth`).
fn scalar(c: &mut [u64], n: usize) {
    for (k, &mask) in MASKS.iter().enumerate().take(n) {
        for w in c.iter_mut() {
            *w ^= (*w & mask) << (1u32 << k);
        }
    }
    let mut half = 1;
    while half < c.len() {
        for block in c.chunks_exact_mut(2 * half) {
            let (lo, hi) = block.split_at_mut(half);
            for (h, &l) in hi.iter_mut().zip(lo.iter()) {
                *h ^= l;
            }
        }
        half *= 2;
    }
}

/// Radix-`R` butterfly (`R` = 1, 2, 4 or 8) on `R` registers: `log2 R`
/// cross-register stages, `v[i] ^= v[i ^ h]` for every `i` with bit `h`.
macro_rules! butterfly {
    ($v:expr, $R:expr, $xor:ident) => {{
        let mut h = 1;
        while h < $R {
            for i in 0..$R {
                if i & h != 0 {
                    $v[i] = $xor($v[i], $v[i ^ h]);
                }
            }
            h *= 2;
        }
    }};
}

/// The two pass drivers, shared by both flavours: `$first::<G>` runs the
/// in-register stages over groups of `G` registers, `$pass::<R>` a
/// radix-`R` cross-word pass. `c.len()` is a power of two, at least `$W`.
macro_rules! drive {
    ($c:expr, $W:expr, $first:ident, $pass:ident) => {{
        let c = $c;
        let g = (c.len() / $W).min(8);
        match g {
            8 => $first::<8>(c),
            4 => $first::<4>(c),
            2 => $first::<2>(c),
            _ => $first::<1>(c),
        }
        let mut half = $W * g;
        while half < c.len() {
            let r = (c.len() / half).min(8);
            match r {
                8 => $pass::<8>(c, half),
                4 => $pass::<4>(c, half),
                _ => $pass::<2>(c, half),
            }
            half *= r;
        }
    }};
}

/// 128-bit flavour (2 words per register). SSE2 is the x86-64 baseline,
/// but the functions still carry `#[target_feature(enable = "sse2")]` (the
/// intrinsics require it), so they are entered through `unsafe` too.
#[cfg(target_arch = "x86_64")]
mod sse2 {
    use super::MASKS;
    use std::arch::x86_64::*;

    /// Words per register.
    const W: usize = 2;

    #[inline]
    #[target_feature(enable = "sse2")]
    fn ld(c: &[u64]) -> __m128i {
        let a: &[u64; W] = c[..W].try_into().expect("2 words");
        // SAFETY: `a` is 16 readable bytes; `loadu` has no alignment requirement.
        unsafe { _mm_loadu_si128(a.as_ptr().cast()) }
    }

    #[inline]
    #[target_feature(enable = "sse2")]
    fn st(c: &mut [u64], v: __m128i) {
        let a: &mut [u64; W] = (&mut c[..W]).try_into().expect("2 words");
        // SAFETY: `a` is 16 writable bytes; `storeu` has no alignment requirement.
        unsafe { _mm_storeu_si128(a.as_mut_ptr().cast(), v) }
    }

    #[inline]
    #[target_feature(enable = "sse2")]
    fn xor(a: __m128i, b: __m128i) -> __m128i {
        _mm_xor_si128(a, b)
    }

    /// Stages 0..=6 on one register: the six in-word stages, then
    /// `half = 1` (`[w0, w1] ^= [0, w0]`, a byte shift by 8).
    #[inline]
    #[target_feature(enable = "sse2")]
    fn low(mut v: __m128i) -> __m128i {
        macro_rules! inword {
            ($k:literal, $s:literal) => {
                let m = _mm_and_si128(v, _mm_set1_epi64x(MASKS[$k] as i64));
                v = _mm_xor_si128(v, _mm_slli_epi64(m, $s));
            };
        }
        inword!(0, 1);
        inword!(1, 2);
        inword!(2, 4);
        inword!(3, 8);
        inword!(4, 16);
        inword!(5, 32);
        _mm_xor_si128(v, _mm_slli_si128(v, 8))
    }

    /// Pass 1 over groups of `G` registers: [`low`] on each, then a
    /// radix-`G` butterfly across them.
    #[target_feature(enable = "sse2")]
    fn first<const G: usize>(c: &mut [u64]) {
        // `G` registers of `W` words each.
        let groups = c.as_chunks_mut::<W>().0.as_chunks_mut::<G>().0;
        for group in groups {
            let mut v = [_mm_setzero_si128(); G];
            for (vi, words) in v.iter_mut().zip(group.iter()) {
                *vi = low(ld(words));
            }
            butterfly!(v, G, xor);
            for (words, &vi) in group.iter_mut().zip(v.iter()) {
                st(words, vi);
            }
        }
    }

    /// `log2 R` cross-word stages from `half` (in words) up, in one pass.
    #[target_feature(enable = "sse2")]
    fn pass<const R: usize>(c: &mut [u64], half: usize) {
        for block in c.chunks_exact_mut(R * half) {
            for j in (0..half).step_by(W) {
                let mut v = [_mm_setzero_si128(); R];
                for (i, vi) in v.iter_mut().enumerate() {
                    *vi = ld(&block[i * half + j..]);
                }
                butterfly!(v, R, xor);
                for (i, &vi) in v.iter().enumerate().skip(1) {
                    st(&mut block[i * half + j..], vi);
                }
            }
        }
    }

    /// `c.len()` is a power of two, at least 2.
    #[target_feature(enable = "sse2")]
    pub(super) fn mobius(c: &mut [u64]) {
        drive!(c, W, first, pass);
    }
}

/// 256-bit flavour (4 words per register). Every function is
/// `#[target_feature(enable = "avx2")]`, reached only through
/// `SimdLevel::supported()`.
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use super::MASKS;
    use std::arch::x86_64::*;

    /// Words per register.
    const W: usize = 4;

    #[target_feature(enable = "avx2")]
    #[inline]
    fn ld(c: &[u64]) -> __m256i {
        let a: &[u64; W] = c[..W].try_into().expect("4 words");
        // SAFETY: `a` is 32 readable bytes; `loadu` has no alignment requirement.
        unsafe { _mm256_loadu_si256(a.as_ptr().cast()) }
    }

    #[target_feature(enable = "avx2")]
    #[inline]
    fn st(c: &mut [u64], v: __m256i) {
        let a: &mut [u64; W] = (&mut c[..W]).try_into().expect("4 words");
        // SAFETY: `a` is 32 writable bytes; `storeu` has no alignment requirement.
        unsafe { _mm256_storeu_si256(a.as_mut_ptr().cast(), v) }
    }

    #[target_feature(enable = "avx2")]
    #[inline]
    fn xor(a: __m256i, b: __m256i) -> __m256i {
        _mm256_xor_si256(a, b)
    }

    /// Stages 0..=7 on one register `[w0, w1, w2, w3]`: the six in-word
    /// stages (`vpand` + `vpsllq` + `vpxor`), then `half = 1`
    /// (`^= [0, w0, 0, w2]`: `vpslldq` by 8 within each 128-bit lane) and
    /// `half = 2` (`^= [0, 0, w0, w1]`: `vperm2i128` moving the low lane up
    /// and zeroing the low one).
    #[target_feature(enable = "avx2")]
    #[inline]
    fn low(mut v: __m256i) -> __m256i {
        macro_rules! inword {
            ($k:literal, $s:literal) => {
                let m = _mm256_and_si256(v, _mm256_set1_epi64x(MASKS[$k] as i64));
                v = _mm256_xor_si256(v, _mm256_slli_epi64(m, $s));
            };
        }
        inword!(0, 1);
        inword!(1, 2);
        inword!(2, 4);
        inword!(3, 8);
        inword!(4, 16);
        inword!(5, 32);
        v = _mm256_xor_si256(v, _mm256_slli_si256(v, 8));
        _mm256_xor_si256(v, _mm256_permute2x128_si256(v, v, 0x08))
    }

    /// Pass 1 over groups of `G` registers: [`low`] on each, then a
    /// radix-`G` butterfly across them.
    #[target_feature(enable = "avx2")]
    fn first<const G: usize>(c: &mut [u64]) {
        // `G` registers of `W` words each.
        let groups = c.as_chunks_mut::<W>().0.as_chunks_mut::<G>().0;
        for group in groups {
            let mut v = [_mm256_setzero_si256(); G];
            for (vi, words) in v.iter_mut().zip(group.iter()) {
                *vi = low(ld(words));
            }
            butterfly!(v, G, xor);
            for (words, &vi) in group.iter_mut().zip(v.iter()) {
                st(words, vi);
            }
        }
    }

    /// `log2 R` cross-word stages from `half` (in words) up, in one pass.
    #[target_feature(enable = "avx2")]
    fn pass<const R: usize>(c: &mut [u64], half: usize) {
        for block in c.chunks_exact_mut(R * half) {
            for j in (0..half).step_by(W) {
                let mut v = [_mm256_setzero_si256(); R];
                for (i, vi) in v.iter_mut().enumerate() {
                    *vi = ld(&block[i * half + j..]);
                }
                butterfly!(v, R, xor);
                for (i, &vi) in v.iter().enumerate().skip(1) {
                    st(&mut block[i * half + j..], vi);
                }
            }
        }
    }

    /// `c.len()` is a power of two, at least 4.
    #[target_feature(enable = "avx2")]
    pub(super) fn mobius(c: &mut [u64]) {
        drive!(c, W, first, pass);
    }
}

#[cfg(test)]
mod tests {
    use super::super::SplitMix64;
    use super::*;

    /// Bool-per-row reference: the textbook butterfly.
    fn mobius_bools(f: &mut [bool]) {
        let mut h = 1;
        while h < f.len() {
            for x in 0..f.len() {
                if x & h != 0 {
                    f[x] ^= f[x ^ h];
                }
            }
            h *= 2;
        }
    }

    fn random_table(rng: &mut SplitMix64, n: usize) -> Vec<u64> {
        let mut c: Vec<u64> = (0..(1usize << n).div_ceil(64))
            .map(|_| rng.next_u64())
            .collect();
        if n < 6 {
            c[0] &= (1u64 << (1 << n)) - 1;
        }
        c
    }

    /// Every level against the scalar loop, across the sub-word, one-,
    /// two- and four-word boundaries and every SIMD pass shape (group
    /// sizes 1..8, radix 2/4/8 tails).
    #[test]
    fn mobius_every_level_matches_scalar() {
        let mut rng = SplitMix64(0x0B1_0001);
        let max_n = if cfg!(miri) { 10 } else { 18 };
        for n in 0..=max_n {
            for _ in 0..3 {
                let c = random_table(&mut rng, n);
                let mut want = c.clone();
                scalar(&mut want, n);
                for level in SimdLevel::available() {
                    let mut got = c.clone();
                    mobius_transform_with(level, &mut got, n);
                    assert_eq!(got, want, "{level}, n = {n}");
                }
            }
        }
    }

    #[test]
    fn scalar_matches_bool_butterfly() {
        let mut rng = SplitMix64(0x0B1_0002);
        for n in 0..=10 {
            let c = random_table(&mut rng, n);
            let mut f: Vec<bool> = (0..1usize << n)
                .map(|x| (c[x >> 6] >> (x & 63)) & 1 == 1)
                .collect();
            mobius_bools(&mut f);
            let mut got = c.clone();
            scalar(&mut got, n);
            for (x, &want) in f.iter().enumerate() {
                assert_eq!((got[x >> 6] >> (x & 63)) & 1 == 1, want, "n = {n}, row {x}");
            }
            // Rows past 2^n stay zero.
            if n < 6 {
                assert_eq!(got[0] >> (1 << n), 0);
            }
            // An involution over GF(2).
            scalar(&mut got, n);
            assert_eq!(got, c);
        }
    }

    #[test]
    fn mobius_rejects_bad_lengths() {
        assert!(std::panic::catch_unwind(|| mobius_transform(&mut [0; 3], 8)).is_err());
        assert!(std::panic::catch_unwind(|| mobius_transform(&mut [0; 2], 6)).is_err());
    }
}
