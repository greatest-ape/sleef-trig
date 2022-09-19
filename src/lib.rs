mod avx;
mod fallback;
mod tables;

pub use avx::Sleef_sind4_u35avx;
pub use fallback::Sleef_sind1_u35purec;
