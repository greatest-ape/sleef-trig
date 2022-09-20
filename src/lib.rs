mod sind1_u35purec;
#[cfg(target_arch = "x86_64")]
mod sind4_u35avx;
mod sinf1_u35purec_range125;
mod tables;

pub use sind1_u35purec::Sleef_sind1_u35purec;
#[cfg(target_arch = "x86_64")]
pub use sind4_u35avx::Sleef_sind4_u35avx;
pub use sinf1_u35purec_range125::Sleef_sinf1_u35purec_range125;
