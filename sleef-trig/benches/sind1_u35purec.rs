use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn bench_sind1_u35purec(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sleef_sind1_u35purec");

    for a in [1.2, 40.8, 1e+14 + 2.0].iter() {
        group.bench_with_input(BenchmarkId::new("Reference", a), a, |b, a| {
            b.iter(|| unsafe { ::sleef_trig_sys::Sleef_sind1_u35purec(*a) })
        });
        group.bench_with_input(BenchmarkId::new("Ported", a), a, |b, a| {
            b.iter(|| ::sleef_trig::Sleef_sind1_u35purec(*a))
        });
    }

    group.finish();
}

criterion_group!(benches, bench_sind1_u35purec);
criterion_main!(benches);
