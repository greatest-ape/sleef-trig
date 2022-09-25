use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sleef_sinf1_u35purec_range125");

    for a in [1.2, 40.8].iter() {
        group.bench_with_input(BenchmarkId::new("Reference", a), a, |b, a| {
            b.iter(|| unsafe { ::sleef_trig_sys::Sleef_sinf1_u35purec(*a) })
        });
        group.bench_with_input(BenchmarkId::new("Ported", a), a, |b, a| {
            b.iter(|| ::sleef_trig::Sleef_sinf1_u35purec_range125(*a))
        });
    }

    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
