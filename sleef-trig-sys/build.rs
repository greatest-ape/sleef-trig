use cc::Build;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=c");

    Build::new()
        .file("./c/purec.c")
        .warnings(false)
        .compile("sleef_trig_purec");

    #[cfg(target_arch = "x86_64")]
    Build::new()
        .file("./c/sse2.c")
        .flag("-msse2")
        .warnings(false)
        .compile("sleef_trig_sse2");

    #[cfg(target_arch = "x86_64")]
    Build::new()
        .file("./c/avx.c")
        .flag("-mavx")
        .warnings(false)
        .compile("sleef_trig_avx");
}
