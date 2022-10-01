# sleef-trig

[![CI](https://github.com/greatest-ape/sleef-trig/actions/workflows/ci.yml/badge.svg)](https://github.com/greatest-ape/sleef-trig/actions/workflows/ci.yml)

Rust port of a few [SLEEF](https://sleef.org) trigonometric functions:

* `Sleef_sind1_u35purec` (approximately 15-30% slower than original)
* `Sleef_sind2_u35sse2` (approximately 15-20% slower than original)
* `Sleef_sind4_u35avx` (approximately 5-10% slower than original)

The performance differences are the largest for inputs with high absolute
values, notably equal to or greater than 1e14.

Additionally, a couple of functions only valid for inputs between -125.0 and
125.0 are provided:

* `Sleef_sinf1_u35purec_range125`
* `Sleef_cosf1_u35purec_range125`

Both are around 30x faster than the equivalent (unlimited range) sleef
functions.

## Usage

To use this crate, add `sleef-trig` as a dependency to your `Cargo.toml` file.

For optimal performance, compile sleef-trig with `codegen-units = 1` by adding
the following to `Cargo.toml` (once for each performance sensitive profile):

```toml
[profile.release.package.sleef-trig]
codegen-units = 1
```

### Running tests and benchmarks

Run:

```sh
cargo test
cargo bench
```

Running tests and benchmarks requires nightly Rust and relies on
`sleef-trig-sys` for SLEEF bindings.

## Why not just link to SLEEF?

SIMD FFI is [unstable and relies on undefined behaviour](https://github.com/rust-lang/rust/issues/63068).
