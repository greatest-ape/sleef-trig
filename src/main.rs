/// Useful for debugging
fn main() {
    let a = -2.604817296578813e228;

    let result = unsafe {
        let mut arr = [0.0; 2];

        core::arch::x86_64::_mm_storeu_pd(arr.as_mut_ptr(), sleef_trig::Sleef_sind2_u35sse2(core::arch::x86_64::_mm_set1_pd(a)));

        arr
    };

    println!("{:?}", result);
}
