use criterion::{criterion_group, criterion_main, Criterion};
use vm::Vm;

pub fn book_repo_benchmarks(c: &mut Criterion) {
    for benchmark in std::fs::read_dir(format!(
        "{}/../../ext/craftinginterpreters/test/benchmark/",
        env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap()
    .flatten()
    .filter(|entry| entry.path().extension() == Some("lox".as_ref()))
    {
        let source = std::fs::read_to_string(benchmark.path()).unwrap();
        c.bench_function(benchmark.file_name().to_str().unwrap(), |b| {
            b.iter(|| {
                Vm::new().run_source(&source, &mut Vec::new()).unwrap();
            });
        });
    }
}

criterion_group! {
    name = benches;
    // Running the book benchmarks takes a few seconds, so use a low sample size
    config = Criterion::default().sample_size(10);
    targets = book_repo_benchmarks
}
criterion_main!(benches);
