# sleef-trig

Rust port of a few [SLEEF](https://sleef.org) trigonometric functions:

* `Sleef_sind4_u35avx`
* `Sleef_sind1_u35purec`

Additionally, a couple of functions only valid for inputs between -125.0 and
125.0 are provided:

* `Sleef_sinf1_u35purec_range125`
* `Sleef_cosf1_u35purec_range125`

## Why not just link to sleef?

SIMD FFI is [unstable and possibly unsafe](https://github.com/rust-lang/rust/issues/63068).
