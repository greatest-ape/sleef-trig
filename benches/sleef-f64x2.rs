use core::arch::x86_64::*;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

#[target_feature(enable = "sse2")]
unsafe fn sse2(a: f64) -> __m128d {
    ::sleef_sys::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0))
}

#[target_feature(enable = "sse2")]
unsafe fn sse2_port(a: f64) -> __m128d {
    ::sleef_trig::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0))
}

#[target_feature(enable = "sse4.2")]
unsafe fn sse4(a: f64) -> __m128d {
    ::sleef_sys::Sleef_sind2_u35sse4(_mm_set_pd(a, a + 1.0))
}

unsafe fn scalar(a: f64) -> [f64; 2] {
    [
        ::sleef_sys::Sleef_sind1_u35purec(a),
        ::sleef_sys::Sleef_sind1_u35purec(a + 1.0),
    ]
}

unsafe fn scalar_port(a: f64) -> [f64; 2] {
    [
        ::sleef_trig::Sleef_sind1_u35purec(a),
        ::sleef_trig::Sleef_sind1_u35purec(a + 1.0),
    ]
}

fn bench(c: &mut Criterion) {
    if !is_x86_feature_detected!("sse4.2") {
        panic!("CPU does not support avx");
    }

    let mut group = c.benchmark_group("sleef f64x2");

    for a in [1.2, 40.8, 1e+14 + 2.0].iter() {
        group.bench_with_input(BenchmarkId::new("scalar (reference)", a), a, |b, a| {
            b.iter(|| unsafe { scalar(*a) })
        });
        group.bench_with_input(BenchmarkId::new("scalar (port)", a), a, |b, a| {
            b.iter(|| unsafe { scalar_port(*a) })
        });
        group.bench_with_input(BenchmarkId::new("sse2 (reference)", a), a, |b, a| {
            b.iter(|| unsafe { sse2(*a) })
        });
        group.bench_with_input(BenchmarkId::new("sse2 (port)", a), a, |b, a| {
            b.iter(|| unsafe { sse2_port(*a) })
        });
        group.bench_with_input(BenchmarkId::new("sse4 (reference)", a), a, |b, a| {
            b.iter(|| unsafe { sse4(*a) })
        });
    }

    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
