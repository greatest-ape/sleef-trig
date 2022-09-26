#![cfg_attr(target_arch = "x86_64", feature(simd_ffi))]
#![cfg_attr(target_arch = "x86_64", allow(improper_ctypes))]

extern "C" {
    pub fn Sleef_cosf1_u35purec(d: f32) -> f32;
    pub fn Sleef_sinf1_u35purec(d: f32) -> f32;
    pub fn Sleef_sind1_u35purec(d: f64) -> f64;
}

#[cfg(target_arch = "x86_64")]
use core::arch::x86_64::{__m128d, __m256d};

#[cfg(target_arch = "x86_64")]
extern "C" {
    pub fn Sleef_sind2_u35sse2(d: __m128d) -> __m128d;
    pub fn Sleef_sind4_u35avx(d: __m256d) -> __m256d;
}
