#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::tables::Sleef_rempitabdp;

type vmask_purec_scalar_sleef = u64;
type vopmask_purec_scalar_sleef = u32;
type vdouble_purec_scalar_sleef = f64;
type vint_purec_scalar_sleef = i32;

#[derive(Clone, Copy)]
struct vdouble2_purec_scalar_sleef {
    x: vdouble_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
}

#[derive(Clone, Copy)]
struct ddi_t_purec_scalar_sleef {
    dd_purec_scalar_sleef: vdouble2_purec_scalar_sleef,
    i: vint_purec_scalar_sleef,
}

#[derive(Clone, Copy)]
struct di_t_purec_scalar_sleef {
    d: vdouble_purec_scalar_sleef,
    i: vint_purec_scalar_sleef,
}

pub fn Sleef_sind1_u35purec(mut d: f64) -> f64 {
    let r = d;

    let ql = if d.abs() < 15.0 {
        let dql = (d * 0.318309886183790671537767526745028724).round();
        let ql = dql.round() as i32; // FIXME: cast ok?
        d = dql * -3.141592653589793116 + d;
        d = dql * -1.2246467991473532072e-16 + d;

        ql
    } else if d.abs() < 1e+14 {
        let mut dqh = (d * 0.318309886183790671537767526745028724 / f64::from(1 << 24)).trunc();
        dqh = dqh * f64::from(1 << 24);
        let dql = (d * 0.318309886183790671537767526745028724 - dqh).round();
        let ql = dql.round() as i32; // FIXME: cast ok?

        d = dqh * -3.1415926218032836914 + d;
        d = dql * -3.1415926218032836914 + d;
        d = dqh * -3.1786509424591713469e-08 + d;
        d = dql * -3.1786509424591713469e-08 + d;
        d = dqh * -1.2246467864107188502e-16 + d;
        d = dql * -1.2246467864107188502e-16 + d;
        d = (dqh + dql) * -1.2736634327021899816e-24 + d;

        ql
    } else {
        let mut ddi_purec_scalar_sleef = rempi_purec_scalar_sleef(d);
        let mut ql = ddi_purec_scalar_sleef.i & 3;
        ql = (ql + ql)
            + if ddi_purec_scalar_sleef.dd_purec_scalar_sleef.x > 0.0 {
                2
            } else {
                1
            };
        ql = ql >> 2;
        let o = (ddi_purec_scalar_sleef.i & 1) == 1;
        let mut x = vdouble2_purec_scalar_sleef {
            x: vmulsign_vd_vd_vd_purec_scalar_sleef(
                -3.141592653589793116 * 0.5,
                ddi_purec_scalar_sleef.dd_purec_scalar_sleef.x,
            ),
            y: vmulsign_vd_vd_vd_purec_scalar_sleef(
                -1.2246467991473532072e-16 * 0.5,
                ddi_purec_scalar_sleef.dd_purec_scalar_sleef.x,
            ),
        };
        x = ddadd2_vd2_vd2_vd2_purec_scalar_sleef(ddi_purec_scalar_sleef.dd_purec_scalar_sleef, x);
        ddi_purec_scalar_sleef.dd_purec_scalar_sleef = if o {
            x
        } else {
            ddi_purec_scalar_sleef.dd_purec_scalar_sleef
        };
        d = ddi_purec_scalar_sleef.dd_purec_scalar_sleef.x
            + ddi_purec_scalar_sleef.dd_purec_scalar_sleef.y;
        d = f64::from_bits(vor_vm_vo64_vm_purec_scalar_sleef(
            r.is_infinite() as u32 | r.is_nan() as u32, // FIXME: cast ok?
            d.to_bits(),
        ));

        ql
    };

    let s = d * d;

    d = f64::from_bits(
        vand_vm_vo64_vm_purec_scalar_sleef(
            veq_vo_vi_vi_purec_scalar_sleef(ql & 1i32, 1),
            (-0.0f64).to_bits(),
        ) ^ (d).to_bits(),
    );

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = (s4)
        * ((s2) * ((s) * (-7.97255955009037868891952e-18) + (2.81009972710863200091251e-15))
            + ((s) * (-7.64712219118158833288484e-13) + (1.60590430605664501629054e-10)))
        + ((s2) * ((s) * (-2.50521083763502045810755e-08) + (2.75573192239198747630416e-06))
            + ((s) * (-0.000198412698412696162806809) + (0.00833333333333332974823815)));

    u = u * s + -0.166666666666666657414808;

    u = s * (u * d) + d;

    u = if r == -0.0 { r } else { u };

    return u;
}

#[inline(always)]
fn rempi_purec_scalar_sleef(mut a: vdouble_purec_scalar_sleef) -> ddi_t_purec_scalar_sleef {
    let mut ex: vint_purec_scalar_sleef = vilogb2k_vi_vd_purec_scalar_sleef(a);

    ex = ex - 55;
    let mut q = (ex > (700 - 55)) as i32 & (-64);
    a = vldexp3_vd_vd_vi_purec_scalar_sleef(a, q);
    ex = (ex >> 31) & (!ex);
    ex = ((ex as u32) << 2) as i32; // FIXME: cast ok? vsll_vi_vi_i_purec_scalar_sleef;
    let mut x = ddmul_vd2_vd_vd_purec_scalar_sleef(a, Sleef_rempitabdp[ex as usize]);
    let mut di: di_t_purec_scalar_sleef = rempisub_purec_scalar_sleef(x.x);
    q = di.i;
    x.x = di.d;
    x = ddnormalize_vd2_vd2_purec_scalar_sleef(x);
    let mut y = ddmul_vd2_vd_vd_purec_scalar_sleef(a, Sleef_rempitabdp[1 + ex as usize]);
    x = ddadd2_vd2_vd2_vd2_purec_scalar_sleef(x, y);
    di = rempisub_purec_scalar_sleef(x.x);
    q = q + di.i;
    x.x = di.d;
    x = ddnormalize_vd2_vd2_purec_scalar_sleef(x);
    y = vdouble2_purec_scalar_sleef {
        x: Sleef_rempitabdp[2 + ex as usize],
        y: Sleef_rempitabdp[3 + ex as usize],
    };
    y = ddmul_vd2_vd2_vd_purec_scalar_sleef(y, a);
    x = ddadd2_vd2_vd2_vd2_purec_scalar_sleef(x, y);
    x = ddnormalize_vd2_vd2_purec_scalar_sleef(x);
    x = ddmul_vd2_vd2_vd2_purec_scalar_sleef(
        x,
        vdouble2_purec_scalar_sleef {
            x: 3.141592653589793116 * 2.0,
            y: 1.2246467991473532072e-16 * 2.0,
        },
    );
    let o: vopmask_purec_scalar_sleef = (a.abs() < 0.7) as u32;
    x.x = if o != 0 { a } else { x.x };
    x.y = f64::from_bits((o as u64) & !(x.y.to_bits()));
    ddi_t_purec_scalar_sleef {
        dd_purec_scalar_sleef: x,
        i: q,
    }
}

#[inline(always)]
fn vmulsign_vd_vd_vd_purec_scalar_sleef(
    x: vdouble_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
) -> vdouble_purec_scalar_sleef {
    f64::from_bits(x.to_bits() ^ vsignbit_vm_vd_purec_scalar_sleef(y))
}

#[inline(always)]
fn vsignbit_vm_vd_purec_scalar_sleef(d: vdouble_purec_scalar_sleef) -> vmask_purec_scalar_sleef {
    d.to_bits() & (-0.0f64).to_bits()
}

#[inline(always)]
fn ddadd2_vd2_vd2_vd2_purec_scalar_sleef(
    x: vdouble2_purec_scalar_sleef,
    y: vdouble2_purec_scalar_sleef,
) -> vdouble2_purec_scalar_sleef {
    let s: vdouble_purec_scalar_sleef = x.x + y.x;
    let v: vdouble_purec_scalar_sleef = s - x.x;
    let t: vdouble_purec_scalar_sleef = (x.x - (s - v)) + (y.x - v);

    vdouble2_purec_scalar_sleef {
        x: s,
        y: (t + (x.y + y.y)),
    }
}

#[inline(always)]
fn ddmul_vd2_vd2_vd_purec_scalar_sleef(
    x: vdouble2_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
) -> vdouble2_purec_scalar_sleef {
    let xh: vdouble_purec_scalar_sleef = vupper_vd_vd_purec_scalar_sleef(x.x);
    let xl = x.x - xh;
    let yh: vdouble_purec_scalar_sleef = vupper_vd_vd_purec_scalar_sleef(y);
    let yl = y - yh;

    let s: vdouble_purec_scalar_sleef = x.x * y;

    vdouble2_purec_scalar_sleef {
        x: s,
        y: (xh * yh + -s + xl * yh + xh * yl + xl * yl + x.y * y),
    }
}

// FIXME: correct?
#[inline(always)]
fn vupper_vd_vd_purec_scalar_sleef(d: vdouble_purec_scalar_sleef) -> vdouble_purec_scalar_sleef {
    f64::from_bits(d.to_bits() & ((0xffffffffu64 << 32) | 0xf8000000u64))
}

#[inline(always)]
fn ddmul_vd2_vd_vd_purec_scalar_sleef(
    x: vdouble_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
) -> vdouble2_purec_scalar_sleef {
    let xh = vupper_vd_vd_purec_scalar_sleef(x);
    let xl = x - xh;
    let yh = vupper_vd_vd_purec_scalar_sleef(y);
    let yl = y - yh;

    let s = x * y;

    vdouble2_purec_scalar_sleef {
        x: s,
        y: (xh * yh + -s + xl * yh + xh * yl + xl * yl),
    }
}

#[inline(always)]
fn ddmul_vd2_vd2_vd2_purec_scalar_sleef(
    x: vdouble2_purec_scalar_sleef,
    y: vdouble2_purec_scalar_sleef,
) -> vdouble2_purec_scalar_sleef {
    let xh = vupper_vd_vd_purec_scalar_sleef(x.x);
    let xl = x.x - xh;
    let yh = vupper_vd_vd_purec_scalar_sleef(y.x);
    let yl = y.x - yh;

    let s = x.x * y.x;

    vdouble2_purec_scalar_sleef {
        x: s,
        y: (xh * yh + -s + xl * yh + xh * yl + xl * yl + x.x * y.y + x.y * y.x),
    }
}

#[inline(always)]
fn ddnormalize_vd2_vd2_purec_scalar_sleef(
    t: vdouble2_purec_scalar_sleef,
) -> vdouble2_purec_scalar_sleef {
    let s = t.x + t.y;

    vdouble2_purec_scalar_sleef {
        x: s,
        y: t.x - s + t.y,
    }
}

#[inline(always)]
fn rempisub_purec_scalar_sleef(x: vdouble_purec_scalar_sleef) -> di_t_purec_scalar_sleef {
    let y = (x * 4.0).round();
    let vi = (y - x.round() * 4.0).trunc() as i32; // FIXME: ok to just cast here?

    di_t_purec_scalar_sleef {
        d: x - y * 0.25,
        i: vi,
    }
}

#[inline(always)]
fn vilogb2k_vi_vd_purec_scalar_sleef(d: vdouble_purec_scalar_sleef) -> vint_purec_scalar_sleef {
    let mut q = vcastu_vi_vm_purec_scalar_sleef(d.to_bits());
    q = ((q as u32) >> 20) as i32; // FIXME: cast ok?
    q = q & 0x7ff;
    q = q - 0x3ff;

    q
}

// FIXME: cast ok?
#[inline(always)]
fn vcastu_vi_vm_purec_scalar_sleef(vm: vmask_purec_scalar_sleef) -> vint_purec_scalar_sleef {
    (vm >> 32) as i32
}

#[inline(always)]
fn vldexp3_vd_vd_vi_purec_scalar_sleef(
    d: vdouble_purec_scalar_sleef,
    q: vint_purec_scalar_sleef,
) -> vdouble_purec_scalar_sleef {
    f64::from_bits(
        d.to_bits() + vcastu_vm_vi_purec_scalar_sleef(((q as u32) << 20) as i32), // FIXME: cast ok?
    )
}

#[inline(always)]
fn vcastu_vm_vi_purec_scalar_sleef(vi: vint_purec_scalar_sleef) -> vmask_purec_scalar_sleef {
    (vi as u64) << 32 // FIXME: cast ok?
}

#[inline(always)]
fn vor_vm_vo64_vm_purec_scalar_sleef(
    x: vopmask_purec_scalar_sleef,
    y: vmask_purec_scalar_sleef,
) -> vmask_purec_scalar_sleef {
    x as u64 | y
}

#[cfg(test)]
mod tests {
    use quickcheck::*;

    #[test]
    fn test_sind1_u35purec() {
        fn prop(a: f64) -> TestResult {
            if a.is_infinite() || a.is_nan() {
                return TestResult::discard();
            }

            let result = super::Sleef_sind1_u35purec(a);
            let reference = unsafe { sleef_sys::Sleef_sind1_u35purec(a) };

            let success = result == reference;

            if !success {
                println!();
                println!("input: {}", a);
                println!("result: {}", result);
                println!("reference: {}", reference);
            }

            TestResult::from_bool(success)
        }

        quickcheck(prop as fn(f64) -> TestResult);
    }
}

fn vand_vm_vo64_vm_purec_scalar_sleef(
    x: vopmask_purec_scalar_sleef,
    y: vmask_purec_scalar_sleef,
) -> vmask_purec_scalar_sleef {
    return vcast_vm_vo_purec_scalar_sleef(x) & y;
}

fn vcast_vm_vo_purec_scalar_sleef(o: vopmask_purec_scalar_sleef) -> vmask_purec_scalar_sleef {
    o as u64 | ((o as u64) << 32)
}

fn veq_vo_vi_vi_purec_scalar_sleef(
    x: vint_purec_scalar_sleef,
    y: vint_purec_scalar_sleef,
) -> vopmask_purec_scalar_sleef {
    if x == y {
        !0u32
    } else {
        0
    }
}
