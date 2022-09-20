# sleef-sind

Rust port of a few [SLEEF](https://sleef.org) sine functions:

* `Sleef_sind4_u35avx`
* `Sleef_sind1_u35purec`

## Why not just link to sleef?

SIMD FFI is [unstable and possibly unsafe](https://github.com/rust-lang/rust/issues/63068).
