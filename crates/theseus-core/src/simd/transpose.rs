//! Bit-matrix transposes between bytes and bit planes (ADR-0052).
//!
//! Bit-sliced evaluation keeps one `u64` word per *bit position*, holding
//! that bit for 64 inputs ("bit planes"); everything else in the compiler
//! keeps one byte per input. Converting is an 8 x 64 bit-matrix transpose
//! per 64 inputs:
//!
//! - [`bytes_to_planes`]: 64 bytes -> 8 words (`vpmovmskb` reads bit 7 of
//!   every byte at once; `vpaddb x, x` shifts every byte left one bit, so
//!   eight `movemask`s peel the planes off from bit 7 down);
//! - [`planes_to_bytes`]: the inverse (broadcast a plane's 32 lane bits,
//!   `vpshufb` byte `j` to lane `j`, test bit `j % 8`, accumulate);
//! - [`bools_to_bits`] / [`bits_to_bools`]: the same for `bool` slices (the
//!   netlist's per-lane token vectors), 32 bools per `movemask`.
//!
//! The scalar flavour is a SWAR 8 x 8 transpose (three masked delta swaps
//! on a `u64`); a BMI2 `pext`/`pdep` flavour is kept for comparison only.

use super::SimdLevel;

/// Shape check shared by both directions: `n_bytes` in whole 64-byte
/// blocks, at most 8 planes, and every plane word `b * stride + w`
/// inside a buffer of `buf_len` words. Returns the block count.
fn plane_shape(what: &str, n_bytes: usize, buf_len: usize, stride: usize, planes: usize) -> usize {
    assert!(
        n_bytes.is_multiple_of(64),
        "{what}: {n_bytes} bytes is not a whole number of 64-byte blocks"
    );
    assert!(planes <= 8, "{what}: {planes} planes > 8");
    let words = n_bytes / 64;
    if planes > 0 && words > 0 {
        let need = (planes - 1)
            .checked_mul(stride)
            .and_then(|x| x.checked_add(words))
            .unwrap_or(usize::MAX);
        assert!(
            need <= buf_len,
            "{what}: {planes} planes of {words} words at stride {stride} exceed {buf_len} words"
        );
    }
    words
}

/// Bytes to bit planes: for every 64-byte block `w` of `src` and every
/// plane `b < planes`, bit `i` of `dst[b * stride + w]` becomes bit `b`
/// of `src[64 * w + i]`. Other words of `dst` are untouched.
///
/// # Panics
/// If `src.len()` is not a multiple of 64, `planes > 8`, or a plane word
/// falls outside `dst`.
pub fn bytes_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
    bytes_to_planes_with(super::level(), src, dst, stride, planes);
}

/// [`bytes_to_planes`] at a given level (capped at what the CPU has).
pub fn bytes_to_planes_with(
    level: SimdLevel,
    src: &[u8],
    dst: &mut [u64],
    stride: usize,
    planes: usize,
) {
    let words = plane_shape("bytes_to_planes", src.len(), dst.len(), stride, planes);
    if planes == 0 || words == 0 {
        return;
    }
    match level.supported() {
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Avx2 => {
            // SAFETY: `supported()` returns `Avx2` only when AVX2 was detected
            // at runtime, which is the one requirement of this
            // `#[target_feature(enable = "avx2")]` function.
            unsafe { avx2::bytes_to_planes(src, dst, stride, planes) }
        }
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Sse2 => {
            // SAFETY: `supported()` returns `Sse2` or above only when SSE2 was
            // detected (always, on x86-64).
            unsafe { sse2::bytes_to_planes(src, dst, stride, planes) }
        }
        _ => scalar::bytes_to_planes(src, dst, stride, planes),
    }
}

/// Bit planes to bytes, the inverse of [`bytes_to_planes`]: for every
/// 64-byte block `w` of `dst`, byte `i` gets bit `b` from bit `i` of
/// `src[b * stride + w]` for `b < planes`; bits at and above `planes` are 0.
///
/// # Panics
/// If `dst.len()` is not a multiple of 64, `planes > 8`, or a plane word
/// falls outside `src`.
pub fn planes_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
    planes_to_bytes_with(super::level(), src, stride, planes, dst);
}

/// [`planes_to_bytes`] at a given level (capped at what the CPU has).
pub fn planes_to_bytes_with(
    level: SimdLevel,
    src: &[u64],
    stride: usize,
    planes: usize,
    dst: &mut [u8],
) {
    plane_shape("planes_to_bytes", dst.len(), src.len(), stride, planes);
    if planes == 0 {
        dst.fill(0);
        return;
    }
    match level.supported() {
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Avx2 => {
            // SAFETY: `supported()` returns `Avx2` only when AVX2 was detected.
            unsafe { avx2::planes_to_bytes(src, stride, planes, dst) }
        }
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Sse2 => {
            // SAFETY: `supported()` returns `Sse2` or above only when SSE2 was
            // detected (always, on x86-64).
            unsafe { sse2::planes_to_bytes(src, stride, planes, dst) }
        }
        _ => scalar::planes_to_bytes(src, stride, planes, dst),
    }
}

/// Pack bools 8 per byte, LSB first: bit `j` of `dst[k]` is `src[8k + j]`;
/// bits past `src.len()` are 0.
///
/// # Panics
/// If `dst.len() != src.len().div_ceil(8)`.
pub fn bools_to_bits(src: &[bool], dst: &mut [u8]) {
    bools_to_bits_with(super::level(), src, dst);
}

/// [`bools_to_bits`] at a given level (capped at what the CPU has).
pub fn bools_to_bits_with(level: SimdLevel, src: &[bool], dst: &mut [u8]) {
    assert_eq!(
        dst.len(),
        src.len().div_ceil(8),
        "bools_to_bits: {} bools need {} bytes",
        src.len(),
        src.len().div_ceil(8)
    );
    match level.supported() {
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Avx2 => {
            // SAFETY: `supported()` returns `Avx2` only when AVX2 was detected.
            unsafe { avx2::bools_to_bits(src, dst) }
        }
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Sse2 => {
            // SAFETY: `supported()` returns `Sse2` or above only when SSE2 was
            // detected (always, on x86-64).
            unsafe { sse2::bools_to_bits(src, dst) }
        }
        _ => scalar::bools_to_bits(src, dst),
    }
}

/// Unpack bits to bools, the inverse of [`bools_to_bits`]: `dst[i]` is
/// bit `i % 8` of `src[i / 8]`.
///
/// # Panics
/// If `src.len() != dst.len().div_ceil(8)`.
pub fn bits_to_bools(src: &[u8], dst: &mut [bool]) {
    bits_to_bools_with(super::level(), src, dst);
}

/// [`bits_to_bools`] at a given level (capped at what the CPU has).
pub fn bits_to_bools_with(level: SimdLevel, src: &[u8], dst: &mut [bool]) {
    assert_eq!(
        src.len(),
        dst.len().div_ceil(8),
        "bits_to_bools: {} bools need {} bytes",
        dst.len(),
        dst.len().div_ceil(8)
    );
    match level.supported() {
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Avx2 => {
            // SAFETY: `supported()` returns `Avx2` only when AVX2 was detected.
            unsafe { avx2::bits_to_bools(src, dst) }
        }
        #[cfg(target_arch = "x86_64")]
        SimdLevel::Sse2 => {
            // SAFETY: `supported()` returns `Sse2` or above only when SSE2 was
            // detected (always, on x86-64).
            unsafe { sse2::bits_to_bools(src, dst) }
        }
        _ => scalar::bits_to_bools(src, dst),
    }
}

/// Is BMI2 (`pext`/`pdep`) available? Gates the comparison-only BMI2
/// transposes.
#[cfg(target_arch = "x86_64")]
pub fn bmi2_available() -> bool {
    is_x86_feature_detected!("bmi2")
}

/// [`bytes_to_planes`] via BMI2 `pext` (one `pext` per plane per 8 bytes).
/// Kept for comparison (the benches); falls back to the scalar flavour
/// when BMI2 is absent.
#[cfg(target_arch = "x86_64")]
pub fn bytes_to_planes_bmi2(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
    let words = plane_shape("bytes_to_planes", src.len(), dst.len(), stride, planes);
    if planes == 0 || words == 0 {
        return;
    }
    if bmi2_available() {
        // SAFETY: BMI2 was detected just above.
        unsafe { bmi2::bytes_to_planes(src, dst, stride, planes) }
    } else {
        scalar::bytes_to_planes(src, dst, stride, planes);
    }
}

/// [`planes_to_bytes`] via BMI2 `pdep`, for comparison; scalar fallback.
#[cfg(target_arch = "x86_64")]
pub fn planes_to_bytes_bmi2(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
    plane_shape("planes_to_bytes", dst.len(), src.len(), stride, planes);
    if planes == 0 {
        dst.fill(0);
    } else if bmi2_available() {
        // SAFETY: BMI2 was detected just above.
        unsafe { bmi2::planes_to_bytes(src, stride, planes, dst) }
    } else {
        scalar::planes_to_bytes(src, stride, planes, dst);
    }
}

/// Transpose an 8 x 8 bit matrix held row-major in a `u64` (bit `8r + c`
/// is row `r`, column `c`): three masked delta swaps (Hacker's Delight
/// 7-3). For 8 little-endian bytes, byte `c` of the result collects bit
/// `c` of every input byte.
#[inline]
fn transpose8x8(mut x: u64) -> u64 {
    let t = (x ^ (x >> 7)) & 0x00AA_00AA_00AA_00AA;
    x ^= t ^ (t << 7);
    let t = (x ^ (x >> 14)) & 0x0000_CCCC_0000_CCCC;
    x ^= t ^ (t << 14);
    let t = (x ^ (x >> 28)) & 0x0000_0000_F0F0_F0F0;
    x ^= t ^ (t << 28);
    x
}

/// Portable flavour: SWAR 8 x 8 transposes.
mod scalar {
    use super::transpose8x8;

    pub(super) fn bytes_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
        for (w, block) in src.as_chunks::<64>().0.iter().enumerate() {
            let mut p = [0u64; 8];
            for (g, group) in block.as_chunks::<8>().0.iter().enumerate() {
                let y = transpose8x8(u64::from_le_bytes(*group));
                for (b, pb) in p.iter_mut().enumerate() {
                    *pb |= ((y >> (8 * b)) & 0xFF) << (8 * g);
                }
            }
            for (b, &pb) in p.iter().enumerate().take(planes) {
                dst[b * stride + w] = pb;
            }
        }
    }

    pub(super) fn planes_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
        for (w, block) in dst.as_chunks_mut::<64>().0.iter_mut().enumerate() {
            let mut p = [0u64; 8];
            for (b, pb) in p.iter_mut().enumerate().take(planes) {
                *pb = src[b * stride + w];
            }
            for (g, group) in block.as_chunks_mut::<8>().0.iter_mut().enumerate() {
                let x = p.iter().enumerate().fold(0u64, |x, (b, &pb)| {
                    x | (((pb >> (8 * g)) & 0xFF) << (8 * b))
                });
                *group = transpose8x8(x).to_le_bytes();
            }
        }
    }

    pub(super) fn bools_to_bits(src: &[bool], dst: &mut [u8]) {
        for (byte, chunk) in dst.iter_mut().zip(src.chunks(8)) {
            *byte = chunk
                .iter()
                .enumerate()
                .fold(0u8, |acc, (j, &b)| acc | (u8::from(b) << j));
        }
    }

    pub(super) fn bits_to_bools(src: &[u8], dst: &mut [bool]) {
        for (chunk, &byte) in dst.chunks_mut(8).zip(src) {
            for (j, b) in chunk.iter_mut().enumerate() {
                *b = (byte >> j) & 1 == 1;
            }
        }
    }
}

/// 128-bit flavour. SSE2 is part of the x86-64 baseline, but the
/// intrinsics still require `#[target_feature(enable = "sse2")]` on their
/// caller, so these are entered through `unsafe` like the AVX2 ones.
#[cfg(target_arch = "x86_64")]
mod sse2 {
    use std::arch::x86_64::*;

    #[inline]
    #[target_feature(enable = "sse2")]
    fn load(b: &[u8; 16]) -> __m128i {
        // SAFETY: `b` is 16 readable bytes; `loadu` has no alignment requirement.
        unsafe { _mm_loadu_si128(b.as_ptr().cast()) }
    }

    #[inline]
    #[target_feature(enable = "sse2")]
    fn store(b: &mut [u8; 16], v: __m128i) {
        // SAFETY: `b` is 16 writable bytes; `storeu` has no alignment requirement.
        unsafe { _mm_storeu_si128(b.as_mut_ptr().cast(), v) }
    }

    /// 16 one-byte lanes, byte `j` = byte `j / 8` of `m` (low 16 bits),
    /// then `0xFF` where bit `j % 8` of that byte is set.
    #[inline]
    #[target_feature(enable = "sse2")]
    fn expand16(m: u64, sel: __m128i) -> __m128i {
        const SPLAT: u64 = 0x0101_0101_0101_0101;
        let lo = (m & 0xFF).wrapping_mul(SPLAT);
        let hi = ((m >> 8) & 0xFF).wrapping_mul(SPLAT);
        let v = _mm_set_epi64x(hi as i64, lo as i64);
        _mm_cmpeq_epi8(_mm_and_si128(v, sel), sel)
    }

    #[target_feature(enable = "sse2")]
    pub(super) fn bytes_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
        for (w, block) in src.as_chunks::<64>().0.iter().enumerate() {
            let mut v = [_mm_setzero_si128(); 4];
            for (vq, quarter) in v.iter_mut().zip(block.as_chunks::<16>().0) {
                *vq = load(quarter);
            }
            let mut p = [0u64; 8];
            // Plane 7 first: movemask reads bit 7 of every byte, then
            // `x + x` shifts each byte left one bit for the next plane.
            for pb in p.iter_mut().rev() {
                for (q, vq) in v.iter_mut().enumerate() {
                    *pb |= u64::from(_mm_movemask_epi8(*vq) as u16) << (16 * q);
                    *vq = _mm_add_epi8(*vq, *vq);
                }
            }
            for (b, &pb) in p.iter().enumerate().take(planes) {
                dst[b * stride + w] = pb;
            }
        }
    }

    #[target_feature(enable = "sse2")]
    pub(super) fn planes_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
        let sel = _mm_set1_epi64x(super::BIT_SELECT);
        for (w, block) in dst.as_chunks_mut::<64>().0.iter_mut().enumerate() {
            for (q, out) in block.as_chunks_mut::<16>().0.iter_mut().enumerate() {
                // Highest plane first; `acc + acc - eq` shifts the bytes
                // left and adds 1 where the plane bit is set (eq = -1).
                let mut acc = _mm_setzero_si128();
                for b in (0..planes).rev() {
                    let eq = expand16(src[b * stride + w] >> (16 * q), sel);
                    acc = _mm_sub_epi8(_mm_add_epi8(acc, acc), eq);
                }
                store(out, acc);
            }
        }
    }

    #[target_feature(enable = "sse2")]
    pub(super) fn bools_to_bits(src: &[bool], dst: &mut [u8]) {
        let (chunks, tail) = src.as_chunks::<16>();
        let (out, out_tail) = dst.split_at_mut(2 * chunks.len());
        for (chunk, o) in chunks.iter().zip(out.as_chunks_mut::<2>().0) {
            // SAFETY: `chunk` is 16 readable bytes (`bool` is one byte).
            let v = unsafe { _mm_loadu_si128(chunk.as_ptr().cast()) };
            // A bool byte is 0 or 1: shifting its 64-bit lane left by 7
            // moves bit 0 of every byte to bit 7 of the same byte.
            *o = (_mm_movemask_epi8(_mm_slli_epi64(v, 7)) as u16).to_le_bytes();
        }
        super::scalar::bools_to_bits(tail, out_tail);
    }

    #[target_feature(enable = "sse2")]
    pub(super) fn bits_to_bools(src: &[u8], dst: &mut [bool]) {
        let sel = _mm_set1_epi64x(super::BIT_SELECT);
        let one = _mm_set1_epi8(1);
        let (chunks, tail) = dst.as_chunks_mut::<16>();
        let (bits, bits_tail) = src.split_at(2 * chunks.len());
        for (chunk, m) in chunks.iter_mut().zip(bits.as_chunks::<2>().0) {
            let v = _mm_and_si128(expand16(u64::from(u16::from_le_bytes(*m)), sel), one);
            // SAFETY: `chunk` is 16 writable bytes, and every stored byte is
            // 0 or 1 (a 0x00/0xFF mask ANDed with 1): valid `bool`s.
            unsafe { _mm_storeu_si128(chunk.as_mut_ptr().cast(), v) };
        }
        super::scalar::bits_to_bools(bits_tail, tail);
    }
}

/// 256-bit flavour. Every function is `#[target_feature(enable = "avx2")]`
/// and reached only through `SimdLevel::supported()`.
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use std::arch::x86_64::*;

    #[target_feature(enable = "avx2")]
    #[inline]
    fn load(b: &[u8; 32]) -> __m256i {
        // SAFETY: `b` is 32 readable bytes; `loadu` has no alignment requirement.
        unsafe { _mm256_loadu_si256(b.as_ptr().cast()) }
    }

    #[target_feature(enable = "avx2")]
    #[inline]
    fn store(b: &mut [u8; 32], v: __m256i) {
        // SAFETY: `b` is 32 writable bytes; `storeu` has no alignment requirement.
        unsafe { _mm256_storeu_si256(b.as_mut_ptr().cast(), v) }
    }

    /// 32 one-byte lanes from the 32-bit mask `m`: byte `j` is `0xFF` iff
    /// bit `j` of `m` is set. `vpshufb` copies mask byte `j / 8` to lane
    /// `j` (within each 128-bit half, which both hold the broadcast mask),
    /// then `vpand`/`vpcmpeqb` test bit `j % 8`.
    #[target_feature(enable = "avx2")]
    #[inline]
    fn expand32(m: u32) -> __m256i {
        let shuf = _mm256_setr_epi8(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, //
            2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        );
        let sel = _mm256_set1_epi64x(super::BIT_SELECT);
        let v = _mm256_shuffle_epi8(_mm256_set1_epi32(m as i32), shuf);
        _mm256_cmpeq_epi8(_mm256_and_si256(v, sel), sel)
    }

    #[target_feature(enable = "avx2")]
    pub(super) fn bytes_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
        for (w, block) in src.as_chunks::<64>().0.iter().enumerate() {
            let [lo, hi] = block.as_chunks::<32>().0 else {
                unreachable!("64 = 2 x 32")
            };
            let (mut v0, mut v1) = (load(lo), load(hi));
            let mut p = [0u64; 8];
            // Plane 7 first: movemask reads bit 7 of every byte, then
            // `x + x` shifts each byte left one bit for the next plane.
            for pb in p.iter_mut().rev() {
                let m0 = _mm256_movemask_epi8(v0) as u32;
                let m1 = _mm256_movemask_epi8(v1) as u32;
                *pb = u64::from(m0) | (u64::from(m1) << 32);
                v0 = _mm256_add_epi8(v0, v0);
                v1 = _mm256_add_epi8(v1, v1);
            }
            for (b, &pb) in p.iter().enumerate().take(planes) {
                dst[b * stride + w] = pb;
            }
        }
    }

    #[target_feature(enable = "avx2")]
    pub(super) fn planes_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
        for (w, block) in dst.as_chunks_mut::<64>().0.iter_mut().enumerate() {
            for (half, out) in block.as_chunks_mut::<32>().0.iter_mut().enumerate() {
                // Highest plane first; `acc + acc - eq` shifts the bytes
                // left and adds 1 where the plane bit is set (eq = -1).
                let mut acc = _mm256_setzero_si256();
                for b in (0..planes).rev() {
                    let eq = expand32((src[b * stride + w] >> (32 * half)) as u32);
                    acc = _mm256_sub_epi8(_mm256_add_epi8(acc, acc), eq);
                }
                store(out, acc);
            }
        }
    }

    #[target_feature(enable = "avx2")]
    pub(super) fn bools_to_bits(src: &[bool], dst: &mut [u8]) {
        let (chunks, tail) = src.as_chunks::<32>();
        let (out, out_tail) = dst.split_at_mut(4 * chunks.len());
        for (chunk, o) in chunks.iter().zip(out.as_chunks_mut::<4>().0) {
            // SAFETY: `chunk` is 32 readable bytes (`bool` is one byte).
            let v = unsafe { _mm256_loadu_si256(chunk.as_ptr().cast()) };
            // Bool bytes are 0 or 1: a 64-bit shift by 7 moves bit 0 of
            // every byte to bit 7 of the same byte.
            *o = (_mm256_movemask_epi8(_mm256_slli_epi64(v, 7)) as u32).to_le_bytes();
        }
        super::scalar::bools_to_bits(tail, out_tail);
    }

    #[target_feature(enable = "avx2")]
    pub(super) fn bits_to_bools(src: &[u8], dst: &mut [bool]) {
        let one = _mm256_set1_epi8(1);
        let (chunks, tail) = dst.as_chunks_mut::<32>();
        let (bits, bits_tail) = src.split_at(4 * chunks.len());
        for (chunk, m) in chunks.iter_mut().zip(bits.as_chunks::<4>().0) {
            let v = _mm256_and_si256(expand32(u32::from_le_bytes(*m)), one);
            // SAFETY: `chunk` is 32 writable bytes, and every stored byte is
            // 0 or 1 (a 0x00/0xFF mask ANDed with 1): valid `bool`s.
            unsafe { _mm256_storeu_si256(chunk.as_mut_ptr().cast(), v) };
        }
        super::scalar::bits_to_bools(bits_tail, tail);
    }
}

/// The per-byte bit selector `0x80 40 20 10 08 04 02 01` (byte `j` selects
/// bit `j % 8`), shared by the SSE2 and AVX2 expanders.
#[cfg(target_arch = "x86_64")]
const BIT_SELECT: i64 = 0x8040_2010_0804_0201u64 as i64;

/// BMI2 flavour (comparison only): `pext` gathers one plane's 8 bits from
/// 8 bytes, `pdep` scatters them back.
#[cfg(target_arch = "x86_64")]
mod bmi2 {
    use std::arch::x86_64::{_pdep_u64, _pext_u64};

    const LOW_BITS: u64 = 0x0101_0101_0101_0101;

    #[target_feature(enable = "bmi2")]
    pub(super) fn bytes_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
        for (w, block) in src.as_chunks::<64>().0.iter().enumerate() {
            let mut p = [0u64; 8];
            for (g, group) in block.as_chunks::<8>().0.iter().enumerate() {
                let x = u64::from_le_bytes(*group);
                for (b, pb) in p.iter_mut().enumerate().take(planes) {
                    *pb |= _pext_u64(x, LOW_BITS << b) << (8 * g);
                }
            }
            for (b, &pb) in p.iter().enumerate().take(planes) {
                dst[b * stride + w] = pb;
            }
        }
    }

    #[target_feature(enable = "bmi2")]
    pub(super) fn planes_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
        for (w, block) in dst.as_chunks_mut::<64>().0.iter_mut().enumerate() {
            for (g, group) in block.as_chunks_mut::<8>().0.iter_mut().enumerate() {
                let mut x = 0u64;
                for b in 0..planes {
                    x |= _pdep_u64((src[b * stride + w] >> (8 * g)) & 0xFF, LOW_BITS << b);
                }
                *group = x.to_le_bytes();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::SplitMix64;
    use super::*;

    /// Bit-at-a-time reference for [`bytes_to_planes`].
    fn naive_to_planes(src: &[u8], dst: &mut [u64], stride: usize, planes: usize) {
        for (w, block) in src.as_chunks::<64>().0.iter().enumerate() {
            for b in 0..planes {
                dst[b * stride + w] = block
                    .iter()
                    .enumerate()
                    .fold(0, |p, (i, &x)| p | (u64::from((x >> b) & 1) << i));
            }
        }
    }

    /// Bit-at-a-time reference for [`planes_to_bytes`].
    fn naive_to_bytes(src: &[u64], stride: usize, planes: usize, dst: &mut [u8]) {
        for (w, block) in dst.as_chunks_mut::<64>().0.iter_mut().enumerate() {
            for (i, byte) in block.iter_mut().enumerate() {
                *byte = (0..planes).fold(0, |x, b| {
                    x | ((((src[b * stride + w] >> i) & 1) as u8) << b)
                });
            }
        }
    }

    fn random_bytes(rng: &mut SplitMix64, n: usize) -> Vec<u8> {
        (0..n).map(|_| rng.next_u64() as u8).collect()
    }

    #[test]
    fn transpose8x8_is_the_transpose() {
        let mut rng = SplitMix64(7);
        for _ in 0..1000 {
            let x = rng.next_u64();
            let y = transpose8x8(x);
            for r in 0..8 {
                for c in 0..8 {
                    assert_eq!((y >> (8 * r + c)) & 1, (x >> (8 * c + r)) & 1);
                }
            }
            assert_eq!(transpose8x8(y), x);
        }
    }

    #[test]
    fn bytes_planes_round_trip_every_level() {
        let mut rng = SplitMix64(0x7A5_0001);
        for words in [1usize, 2, 3, 4, 5, 8] {
            for planes in 0..=8 {
                for stride in [words, words + 3] {
                    let src = random_bytes(&mut rng, 64 * words);
                    let len = 8 * stride + 2;
                    let mut want = vec![0xDEAD_BEEF_u64; len];
                    naive_to_planes(&src, &mut want, stride, planes);
                    for level in SimdLevel::available() {
                        let mut got = vec![0xDEAD_BEEF_u64; len];
                        bytes_to_planes_with(level, &src, &mut got, stride, planes);
                        assert_eq!(got, want, "{level}: {words} words, {planes} planes");
                    }
                    #[cfg(target_arch = "x86_64")]
                    {
                        let mut got = vec![0xDEAD_BEEF_u64; len];
                        bytes_to_planes_bmi2(&src, &mut got, stride, planes);
                        assert_eq!(got, want, "bmi2: {words} words, {planes} planes");
                    }

                    // And back: the low `planes` bits of every byte.
                    let mask = ((1u16 << planes) - 1) as u8;
                    let back_want: Vec<u8> = src.iter().map(|&b| b & mask).collect();
                    let mut back = vec![0u8; src.len()];
                    naive_to_bytes(&want, stride, planes, &mut back);
                    assert_eq!(back, back_want);
                    for level in SimdLevel::available() {
                        let mut back = vec![0xA5u8; src.len()];
                        planes_to_bytes_with(level, &want, stride, planes, &mut back);
                        assert_eq!(back, back_want, "{level}: {words} words, {planes} planes");
                    }
                    #[cfg(target_arch = "x86_64")]
                    {
                        let mut back = vec![0xA5u8; src.len()];
                        planes_to_bytes_bmi2(&want, stride, planes, &mut back);
                        assert_eq!(back, back_want, "bmi2: {words} words, {planes} planes");
                    }
                }
            }
        }
    }

    #[test]
    fn empty_and_rejected_shapes() {
        for level in SimdLevel::available() {
            let mut dst = [7u64; 4];
            bytes_to_planes_with(level, &[], &mut dst, 1, 8);
            assert_eq!(dst, [7; 4]);
            let mut out: [u8; 0] = [];
            planes_to_bytes_with(level, &[], 0, 8, &mut out);
        }
        let partial = std::panic::catch_unwind(|| bytes_to_planes(&[0u8; 63], &mut [0; 8], 1, 8));
        assert!(partial.is_err(), "a partial block must be rejected");
        let short = std::panic::catch_unwind(|| bytes_to_planes(&[0u8; 128], &mut [0; 15], 2, 8));
        assert!(
            short.is_err(),
            "plane 7, word 1 (index 15) is out of bounds"
        );
    }

    #[test]
    fn bools_bits_round_trip_every_level() {
        let mut rng = SplitMix64(0xB001);
        for n in [
            0usize, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 100, 686, 1000,
        ] {
            let bools: Vec<bool> = (0..n).map(|_| rng.next_u64() & 1 == 1).collect();
            let mut want = vec![0u8; n.div_ceil(8)];
            for (i, &b) in bools.iter().enumerate() {
                want[i / 8] |= u8::from(b) << (i % 8);
            }
            for level in SimdLevel::available() {
                let mut bits = vec![0xFFu8; n.div_ceil(8)];
                bools_to_bits_with(level, &bools, &mut bits);
                assert_eq!(bits, want, "{level}: {n} bools");
                let mut back = vec![true; n];
                bits_to_bools_with(level, &bits, &mut back);
                assert_eq!(back, bools, "{level}: {n} bools back");
            }
        }
    }
}
