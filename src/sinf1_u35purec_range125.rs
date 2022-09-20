/// Like Sleef_sinf1_u35purec but only valid for d where d.abs() < 125.0
pub fn Sleef_sinf1_u35purec_range125(mut d: f32) -> f32 {
    let r = d;

    let q = (d * 0.318309886183790671537767526745028724 as f32).round() as i32;
    let u = (q) as f32;
    d = u * -3.1414794921875f32 + d;
    d = u * -0.00011315941810607910156f32 + d;
    d = u * -1.9841872589410058936e-09f32 + d;

    let s = d * d;

    d = vreinterpret_vf_vm_purec_scalar_sleef(
        vand_vm_vo32_vm_purec_scalar_sleef(
            if (q & 1) == 1 { !0 } else { 0 },
            vreinterpret_vm_vf_purec_scalar_sleef(-0.0f32),
        ) ^ vreinterpret_vm_vf_purec_scalar_sleef(d),
    );

    let mut u = 2.6083159809786593541503e-06f32;
    u = u * s + -0.0001981069071916863322258f32;
    u = u * s + 0.00833307858556509017944336f32;
    u = u * s + -0.166666597127914428710938f32;

    u = (s * (u * d)) + d;

    u = if r == -0.0 { r } else { u };

    return u;
}

#[inline(always)]
fn vreinterpret_vm_vf_purec_scalar_sleef(f: f32) -> u64 {
    (f.to_bits() as u64) << 32
}

#[inline(always)]
fn vreinterpret_vf_vm_purec_scalar_sleef(vm: u64) -> f32 {
    f32::from_bits((vm >> 32) as u32)
}

#[inline(always)]
fn vand_vm_vo32_vm_purec_scalar_sleef(x: i32, y: u64) -> u64 {
    vcast_vm_vo_purec_scalar_sleef(x) & y
}

#[inline(always)]
fn vcast_vm_vo_purec_scalar_sleef(o: i32) -> u64 {
    return o as u64 | ((o as u64) << 32);
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
            let reference = unsafe { sleef_sys::Sleef_sinf1_u35purec(a) };

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
