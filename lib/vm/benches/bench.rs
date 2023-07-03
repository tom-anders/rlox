use criterion::{criterion_group, criterion_main, Criterion};
use vm::Vm;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib", |b| b.iter(|| {
        let source = r#"
            fun fib(n) {
                if (n <= 1) return n;
                return fib(n - 2) + fib(n - 1);
            }

            print fib(10);
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
    }));

    c.bench_function("basic class stuff", |b| b.iter(|| {
        let source = r#"
            class Pair { }

            var pair = Pair();
            pair.first = 1;
            pair.second = 2;
            print pair;
            print pair.first + pair.second;
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
    }));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
