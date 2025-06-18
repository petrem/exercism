use collatz_conjecture::{collatz, run_of_the_mill_collatz};
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_collatz(c: &mut Criterion) {
    let input = std::hint::black_box(27211324);

    c.bench_function("optimized collatz", |b| b.iter(|| collatz(input)));

    c.bench_function("naive collatz", |b| {
        b.iter(|| run_of_the_mill_collatz(input))
    });
}

criterion_group!(benches, bench_collatz);
criterion_main!(benches);
