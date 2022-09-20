mod f32_purec;
#[cfg(target_arch = "x86_64")]
mod f64_avx;
mod f64_purec;
mod f64_tables;

pub use f32_purec::{Sleef_cosf1_u35purec_range125, Sleef_sinf1_u35purec_range125};
#[cfg(target_arch = "x86_64")]
pub use f64_avx::Sleef_sind4_u35avx;
pub use f64_purec::Sleef_sind1_u35purec;
