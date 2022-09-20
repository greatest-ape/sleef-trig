# sleef-sind

Rust port of a few [sleef](https://sleef.org) sine functions:

* Sleef_sind4_u35avx
* Sleef_sind1_u35purec

## Why not just link to sleef?

SIMD FFI is [unstable, possibly unsafe and only supported on nightly Rust](https://github.com/rust-lang/rust/issues/63068).
