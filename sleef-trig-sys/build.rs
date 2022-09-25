use cc::Build;

const FILE: &str = "./c/x86_64.c";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=c");

    Build::new()
        .file(FILE)
        .flag("-march=native")
        .compile("sleef_trig_sys_cc")
}
