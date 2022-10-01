//   Copyright Naoki Shibata and contributors 2010 - 2021.
// Distributed under the Boost Software License, Version 1.0.
//        (See http://www.boost.org/LICENSE_1_0.txt)

#![allow(non_snake_case)]

/// Evaluate the sine function with an error bound of 3.5 ULP. Only valid for
/// inputs within (-125.0, 125.0).
pub fn Sleef_sinf1_u35purec_range125(mut d: f32) -> f32 {
    let r = d;

    let q = (d * 0.318309886183790671537767526745028724 as f32).round() as i32;
    let u = (q) as f32;
    d = u * -3.1414794921875f32 + d;
    d = u * -0.00011315941810607910156f32 + d;
    d = u * -1.9841872589410058936e-09f32 + d;

    let s = d * d;

    d = f32::from_bits(((if (q & 1) == 1 { !0 } else { 0 }) & (-0.0f32).to_bits()) ^ d.to_bits());

    let mut u = 2.6083159809786593541503e-06f32;
    u = u * s + -0.0001981069071916863322258f32;
    u = u * s + 0.00833307858556509017944336f32;
    u = u * s + -0.166666597127914428710938f32;

    u = (s * (u * d)) + d;

    u = if r == -0.0 { r } else { u };

    return u;
}

/// Evaluate the cosine function with an error bound of 3.5 ULP. Only valid for
/// inputs within (-125.0, 125.0).
pub fn Sleef_cosf1_u35purec_range125(mut d: f32) -> f32 {
    let mut q = ((d * 0.318309886183790671537767526745028724 as f32) - 0.5f32).round() as i32;
    q = (q + q) + 1;

    let mut u = q as f32;
    d = u * -3.1414794921875f32 * 0.5f32 + d;
    d = u * -0.00011315941810607910156f32 * 0.5f32 + d;
    d = u * -1.9841872589410058936e-09f32 * 0.5f32 + d;

    let s = d * d;

    d = f32::from_bits(((if (q & 2) == 0 { !0 } else { 0 }) & (-0.0f32).to_bits()) ^ d.to_bits());

    u = 2.6083159809786593541503e-06f32;
    u = u * s + -0.0001981069071916863322258f32;
    u = u * s + 0.00833307858556509017944336f32;
    u = u * s + -0.166666597127914428710938f32;

    (s * (u * d)) + d
}

#[cfg(test)]
mod tests {
    use quickcheck::*;

    #[test]
    fn test_sinf1_u35purec_range125() {
        fn prop(a: f32) -> TestResult {
            if !(a.abs() < 125.0) {
                return TestResult::discard();
            }

            let result = super::Sleef_sinf1_u35purec_range125(a);
            let reference = unsafe { sleef_trig_sys::Sleef_sinf1_u35purec(a) };

            let success = result == reference;

            if !success {
                println!();
                println!("input:     {}", a);
                println!("result:    {}", result);
                println!("reference: {}", reference);
            }

            TestResult::from_bool(success)
        }

        quickcheck(prop as fn(f32) -> TestResult);
    }

    #[test]
    fn test_cosf1_u35purec_range125() {
        fn prop(a: f32) -> TestResult {
            if !(a.abs() < 125.0) {
                return TestResult::discard();
            }

            let result = super::Sleef_cosf1_u35purec_range125(a);
            let reference = unsafe { sleef_trig_sys::Sleef_cosf1_u35purec(a) };

            let success = result == reference;

            if !success {
                println!();
                println!("input:     {}", a);
                println!("result:    {}", result);
                println!("reference: {}", reference);
            }

            TestResult::from_bool(success)
        }

        quickcheck(prop as fn(f32) -> TestResult);
    }
}
