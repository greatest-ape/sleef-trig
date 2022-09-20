# sleef-sind

Rust port of a few [SLEEF](https://sleef.org) sine functions:

* `Sleef_sind4_u35avx`
* `Sleef_sind1_u35purec`
* `Sleef_sinf1_u35purec_range125`, a modified version of the original
  `Sleef_sinf1_u35purec` only valid for inputs between -125.0 and 125.0

## Why not just link to sleef?

SIMD FFI is [unstable and possibly unsafe](https://github.com/rust-lang/rust/issues/63068).
