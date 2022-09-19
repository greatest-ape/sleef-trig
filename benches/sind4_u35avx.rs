use core::arch::x86_64::*;
use criterion::{criterion_group, criterion_main, Criterion, black_box};

fn bench_custom(c: &mut Criterion) {
    if !is_x86_feature_detected!("avx") {
        panic!("CPU does not support avx");
    }

    c.bench_function("ported Sleef_sind4_u35avx", |b| b.iter(||
        unsafe {
            ::sleef_port::Sleef_sind4_u35avx(black_box(_mm256_set1_pd(40.0)))
        }
    ));
}

fn bench_reference(c: &mut Criterion) {
    if !is_x86_feature_detected!("avx") {
        panic!("CPU does not support avx");
    }

    c.bench_function("reference Sleef_sind4_u35avx", |b| b.iter(||
        unsafe {
            ::sleef_sys::Sleef_sind4_u35avx(black_box(_mm256_set1_pd(40.0)))
        }
    ));
}

criterion_group!(benches, bench_reference, bench_custom);
criterion_main!(benches);