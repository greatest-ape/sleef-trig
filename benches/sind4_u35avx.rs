use core::arch::x86_64::*;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

#[target_feature(enable = "avx")]
unsafe fn reference(a: f64) -> __m256d {
    ::sleef_sys::Sleef_sind4_u35avx(_mm256_set_pd(a, a + 1.0, a + 2.0, a + 3.0))
}

#[target_feature(enable = "avx")]
unsafe fn port(a: f64) -> __m256d {
    ::sleef_trig::Sleef_sind4_u35avx(_mm256_set_pd(a, a + 1.0, a + 2.0, a + 3.0))
}

fn bench_sind4_u35avx(c: &mut Criterion) {
    if !is_x86_feature_detected!("avx") {
        panic!("CPU does not support avx");
    }

    let mut group = c.benchmark_group("Sleef_sind4_u35avx");

    for a in [1.2, 40.8, 1e+14 + 2.0].iter() {
        group.bench_with_input(BenchmarkId::new("Reference", a), a, |b, a| {
            b.iter(|| unsafe { reference(*a) })
        });
        group.bench_with_input(BenchmarkId::new("Ported", a), a, |b, a| {
            b.iter(|| unsafe { port(*a) })
        });
    }

    group.finish();
}

criterion_group!(benches, bench_sind4_u35avx);
criterion_main!(benches);
