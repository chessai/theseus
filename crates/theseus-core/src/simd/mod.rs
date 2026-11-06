//! Explicit SIMD kernels with runtime dispatch (ADR-0052).
//!
//! The lane-parallel paths (ADR-0048, ADR-0049) and the packed Möbius
//! transform are plain `u64` loops; the baseline x86-64 target only lets
//! LLVM auto-vectorize them to SSE2. This module holds hand-written
//! `core::arch` kernels for them, each in three flavours:
//!
//! - [`SimdLevel::Scalar`]: portable Rust, the reference (and the only
//!   flavour off x86-64);
//! - [`SimdLevel::Sse2`]: 128-bit kernels (SSE2 is the x86-64 baseline, so
//!   these need no detection);
//! - [`SimdLevel::Avx2`]: 256-bit `#[target_feature(enable = "avx2")]`
//!   kernels, entered only after runtime detection.
//!
//! # Dispatch
//!
//! The active level is detected once ([`SimdLevel::detected`]) and cached.
//! `THESEUS_SIMD=scalar|sse2|avx2|auto` lowers it (never above what the CPU
//! has), and [`set_level`] overrides it programmatically (benches). Every
//! kernel has a `*_with(level, ..)` twin that tests use to force a level
//! per call, so parallel tests never race on the global. Dispatch happens
//! once per kernel call -- a whole loop -- never per lane op.
//!
//! # Safety design
//!
//! `unsafe` is allowed in this module tree only (the workspace denies it);
//! every block carries a `// SAFETY:` comment, and the public API is safe:
//!
//! - an AVX2 kernel is only ever called through [`SimdLevel::supported`],
//!   which caps any requested level at the detected one, so a forced or
//!   bogus `Avx2` on a CPU without AVX2 runs the SSE2 or scalar kernel;
//! - loads and stores are unaligned (`loadu`/`storeu`) and go through
//!   references to fixed-size arrays (`&[u64; 4]`, `&[u8; 32]`, ...) obtained
//!   by checked slicing, so every pointer is in bounds by construction;
//! - shapes (lengths, strides, plane counts, variable indices) are checked
//!   once on entry, before any kernel runs.

#![allow(unsafe_code)]
#![deny(clippy::undocumented_unsafe_blocks)]
#![deny(unsafe_op_in_unsafe_fn)]

pub(crate) mod bvprog;
mod mobius;
mod transpose;

pub use mobius::{mobius_transform, mobius_transform_with};
pub use transpose::{
    bits_to_bools, bits_to_bools_with, bools_to_bits, bools_to_bits_with, bytes_to_planes,
    bytes_to_planes_with, planes_to_bytes, planes_to_bytes_with,
};
#[cfg(target_arch = "x86_64")]
pub use transpose::{bmi2_available, bytes_to_planes_bmi2, planes_to_bytes_bmi2};

use std::sync::atomic::{AtomicU8, Ordering};

/// An instruction-set tier for the kernels in this module, ordered by
/// capability (`Scalar < Sse2 < Avx2`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimdLevel {
    /// Portable Rust: the reference, and the only tier off x86-64.
    Scalar = 0,
    /// 128-bit SSE2 kernels (x86-64 baseline).
    Sse2 = 1,
    /// 256-bit AVX2 kernels (runtime-detected).
    Avx2 = 2,
}

/// Sentinel for "not yet computed" in the level caches.
const UNSET: u8 = u8::MAX;
/// [`SimdLevel::detected`], once computed.
static DETECTED: AtomicU8 = AtomicU8::new(UNSET);
/// [`level`], once computed (or set by [`set_level`]).
static ACTIVE: AtomicU8 = AtomicU8::new(UNSET);

impl SimdLevel {
    /// Every level, lowest first.
    pub const ALL: [SimdLevel; 3] = [SimdLevel::Scalar, SimdLevel::Sse2, SimdLevel::Avx2];

    /// The level's `THESEUS_SIMD` spelling.
    pub fn name(self) -> &'static str {
        match self {
            SimdLevel::Scalar => "scalar",
            SimdLevel::Sse2 => "sse2",
            SimdLevel::Avx2 => "avx2",
        }
    }

    /// Parse a `THESEUS_SIMD` spelling (case-insensitive); `None` for
    /// anything else, including `auto`.
    pub fn parse(s: &str) -> Option<SimdLevel> {
        SimdLevel::ALL
            .into_iter()
            .find(|l| l.name().eq_ignore_ascii_case(s.trim()))
    }

    fn from_u8(v: u8) -> SimdLevel {
        match v {
            0 => SimdLevel::Scalar,
            1 => SimdLevel::Sse2,
            _ => SimdLevel::Avx2,
        }
    }

    /// The highest level this CPU supports, detected once and cached.
    pub fn detected() -> SimdLevel {
        match DETECTED.load(Ordering::Relaxed) {
            UNSET => {
                let l = detect();
                DETECTED.store(l as u8, Ordering::Relaxed);
                l
            }
            v => SimdLevel::from_u8(v),
        }
    }

    /// `self` capped at [`SimdLevel::detected`]: the level a kernel may
    /// actually run at. Every dispatch goes through this, which is what
    /// makes the `#[target_feature]` calls sound.
    pub fn supported(self) -> SimdLevel {
        self.min(SimdLevel::detected())
    }

    /// The levels this host can run, lowest first (tests iterate these).
    pub fn available() -> Vec<SimdLevel> {
        SimdLevel::ALL
            .into_iter()
            .filter(|&l| l <= SimdLevel::detected())
            .collect()
    }
}

impl std::fmt::Display for SimdLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

#[cfg(target_arch = "x86_64")]
fn detect() -> SimdLevel {
    if is_x86_feature_detected!("avx2") {
        SimdLevel::Avx2
    } else if is_x86_feature_detected!("sse2") {
        SimdLevel::Sse2
    } else {
        SimdLevel::Scalar
    }
}

#[cfg(not(target_arch = "x86_64"))]
fn detect() -> SimdLevel {
    SimdLevel::Scalar
}

/// The level requested by `THESEUS_SIMD`, capped at the detected one;
/// unset, empty or `auto` mean the detected level. An unrecognized value
/// also means auto, with a one-time warning on stderr (this runs once,
/// when the level is first needed).
fn from_env() -> SimdLevel {
    let detected = SimdLevel::detected();
    match std::env::var("THESEUS_SIMD") {
        Ok(s) if s.trim().is_empty() || s.trim().eq_ignore_ascii_case("auto") => detected,
        Ok(s) => match SimdLevel::parse(&s) {
            Some(l) => l.supported(),
            None => {
                eprintln!(
                    "theseus: ignoring THESEUS_SIMD={s:?} (want scalar|sse2|avx2|auto); \
                     using {detected}"
                );
                detected
            }
        },
        Err(_) => detected,
    }
}

/// The active level: the last [`set_level`] override, else
/// `THESEUS_SIMD`, else [`SimdLevel::detected`]. Cached after the first
/// call; one relaxed atomic load afterwards.
pub fn level() -> SimdLevel {
    match ACTIVE.load(Ordering::Relaxed) {
        UNSET => {
            let l = from_env();
            ACTIVE.store(l as u8, Ordering::Relaxed);
            l
        }
        v => SimdLevel::from_u8(v),
    }
}

/// Override the active level for every later kernel call in the process
/// (capped at what the CPU supports); `None` returns to `THESEUS_SIMD` /
/// auto. Returns the level now in effect. Meant for benches and binaries:
/// tests should pass a level per call (`*_with`) instead, since the test
/// harness runs tests concurrently.
pub fn set_level(level: Option<SimdLevel>) -> SimdLevel {
    let l = match level {
        Some(l) => l.supported(),
        None => from_env(),
    };
    ACTIVE.store(l as u8, Ordering::Relaxed);
    l
}

/// Fixed-seed splitmix64 for the kernel property tests (no `rand`).
#[cfg(test)]
pub(crate) struct SplitMix64(pub(crate) u64);

#[cfg(test)]
impl SplitMix64 {
    pub(crate) fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels_are_ordered_and_capped() {
        assert!(SimdLevel::Scalar < SimdLevel::Sse2 && SimdLevel::Sse2 < SimdLevel::Avx2);
        let detected = SimdLevel::detected();
        for l in SimdLevel::ALL {
            assert_eq!(l.supported(), l.min(detected));
            assert_eq!(SimdLevel::parse(l.name()), Some(l));
            assert_eq!(SimdLevel::parse(&l.name().to_uppercase()), Some(l));
        }
        assert_eq!(SimdLevel::parse("auto"), None);
        assert_eq!(SimdLevel::parse("avx512"), None);
        assert_eq!(SimdLevel::available().last(), Some(&detected));
        assert_eq!(SimdLevel::available()[0], SimdLevel::Scalar);
        #[cfg(target_arch = "x86_64")]
        assert!(detected >= SimdLevel::Sse2, "SSE2 is the x86-64 baseline");
        #[cfg(not(target_arch = "x86_64"))]
        assert_eq!(detected, SimdLevel::Scalar);
    }
}
