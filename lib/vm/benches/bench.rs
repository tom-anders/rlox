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

    c.bench_function("zoo example from the book", |b| b.iter(|| {
        let source = r#"
            class Zoo {
              init() {
                this.aardvark = 1;
                this.baboon   = 1;
                this.cat      = 1;
                this.donkey   = 1;
                this.elephant = 1;
                this.fox      = 1;
              }
              ant()    { return this.aardvark; }
              banana() { return this.baboon; }
              tuna()   { return this.cat; }
              hay()    { return this.donkey; }
              grass()  { return this.elephant; }
              mouse()  { return this.fox; }
            }

            var zoo = Zoo();
            var sum = 0;
            var start = clock();
            while (sum < 1000) {
              sum = sum + zoo.ant()
                        + zoo.banana()
                        + zoo.tuna()
                        + zoo.hay()
                        + zoo.grass()
                        + zoo.mouse();
            }
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
    }));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
