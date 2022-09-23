use core::arch::x86_64::*;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

unsafe fn reference(a: f64) -> __m128d {
    ::sleef_sys::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0))
}

unsafe fn port(a: f64) -> __m128d {
    ::sleef_trig::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0))
}

fn bench_sind4_u35avx(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sleef_sind2_u35sse2");

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
