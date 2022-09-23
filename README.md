# sleef-trig

Rust port of a few [SLEEF](https://sleef.org) trigonometric functions:

* `Sleef_sind1_u35purec`
* `Sleef_sind2_u35sse2`
* `Sleef_sind4_u35avx`

Additionally, a couple of functions only valid for inputs between -125.0 and
125.0 are provided:

* `Sleef_sinf1_u35purec_range125`
* `Sleef_cosf1_u35purec_range125`

## Usage

To use this crate, add it as a dependency in your `Cargo.toml` file.

For optimal performance, compile sleef-trig with `codegen-units = 1` by adding
the following to `Cargo.toml` (one statement for each performance sensitive
profile):

```toml
[profile.release.package.sleef-trig]
codegen-units = 1
```

## Why not just link to sleef?

SIMD FFI is [unstable and relies on undefined behaviour](https://github.com/rust-lang/rust/issues/63068).
