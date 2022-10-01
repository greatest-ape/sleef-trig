# sleef-trig-sys

Bindings to some [SLEEF](https://sleef.org) trigonometric functions:

* `Sleef_sinf1_u35purec`
* `Sleef_cosf1_u35purec`
* `Sleef_sind1_u35purec`
* `Sleef_sind2_u35sse2`
* `Sleef_sind4_u35avx`

Please note that SIMD FFI is [unstable and relies on undefined behaviour](https://github.com/rust-lang/rust/issues/63068).

The purpose of this crate is to enable correctness testing of `sleef-trig`, a
Rust port of some of the above functions. I recommend using it instead of this
crate due to the aformentioned soundness issues.
