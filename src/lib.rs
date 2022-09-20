#[cfg(target_arch = "x86_64")]
mod avx;
mod fallback;
mod sinf1_u35purec_range125;
mod tables;

#[cfg(target_arch = "x86_64")]
pub use avx::Sleef_sind4_u35avx;
pub use fallback::Sleef_sind1_u35purec;
pub use sinf1_u35purec_range125::Sleef_sinf1_u35purec_range125;
