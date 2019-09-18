#[macro_use]
extern crate criterion;

use criterion::Criterion;
use nom_etf;

fn small_map(c: &mut Criterion) {
    // let mut edn = std::fs::File::open("./fixtures/deps.edn").unwrap();
    let small_map = [131, 116, 0, 0, 0, 5, 100, 0, 1, 97, 97, 73, 100, 0, 1, 98, 98, 0, 0, 32, 56, 100, 0, 1, 99, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 100, 0, 1, 100, 100, 0, 2, 111, 107, 100, 0, 1, 101, 97, 99];

    c.bench_function("decode_small_map", move |b| b.iter(|| nom_etf::parse(&small_map)));
}

criterion_group!(benches, small_map);
criterion_main!(benches);
