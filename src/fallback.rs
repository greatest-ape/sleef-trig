use crate::tables::Sleef_rempitabdp;

type vmask_purec_scalar_sleef = u64;
type vopmask_purec_scalar_sleef = u32;
type vdouble_purec_scalar_sleef = f64;
type vint_purec_scalar_sleef = i32;
type vfloat_purec_scalar_sleef = f32;
type vint2_purec_scalar_sleef = i32;

type vint64_purec_scalar_sleef = i64;
type vuint64_purec_scalar_sleef = u64;

struct vdouble2_purec_scalar_sleef {
    x: vdouble_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
}

struct ddi_t_purec_scalar_sleef {
    dd_purec_scalar_sleef: vdouble2_purec_scalar_sleef,
    i: vint_purec_scalar_sleef,
}

struct di_t_purec_scalar_sleef {
    d: vdouble_purec_scalar_sleef,
    i: vint_purec_scalar_sleef,
}

fn Sleef_sind1_u35purec(mut d: vdouble_purec_scalar_sleef) -> vdouble_purec_scalar_sleef {
    let r = d;

    let ql = if d.abs() < 15.0 {
        let dql = (d * 0.318309886183790671537767526745028724).round();
        let ql = dql.round().into();
        d = (dql * -3.141592653589793116 + d);
        d = (dql * -1.2246467991473532072e-16 + d);

        ql
    } else if d.abs() < 1e+14 {
        let mut dqh = (d * 0.318309886183790671537767526745028724 / f64::from(1 << 24)).trunc();
        dqh = dqh * f64::from(1 << 24);
        let dql = (d * 0.318309886183790671537767526745028724 - dqh).round();
        let ql = dql.round().into();

        d = dqh * -3.1415926218032836914 + d;
        d = dql * -3.1415926218032836914 + d;
        d = dqh * -3.1786509424591713469e-08 + d;
        d = dql * -3.1786509424591713469e-08 + d;
        d = dqh * -1.2246467864107188502e-16 + d;
        d = dql * -1.2246467864107188502e-16 + d;
        d = (dqh + dql) * -1.2736634327021899816e-24 + d;

        ql
    } else {
        let ddi_purec_scalar_sleef = rempi_purec_scalar_sleef(d);
        let mut ql = ddi_purec_scalar_sleef.i & 3;
        ql = (ql + ql)
            + if ddi_purec_scalar_sleef.dd_purec_scalar_sleef.x > 0.0 {
                2
            } else {
                1
            };
        ql = (ql >> 2);
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
            r.is_infinite() as i32 | r.is_nan() as i32,
            d.to_bits(),
        ));

        ql
    };

    let s = d * d;

    d = f64::from_bits((((ql & 1) == 1) as u64) & (-0.0f64).to_bits() ^ d.to_bits());

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

fn rempi_purec_scalar_sleef(a: vdouble_purec_scalar_sleef) -> ddi_t_purec_scalar_sleef {
    let mut ex: vint_purec_scalar_sleef = vilogb2k_vi_vd_purec_scalar_sleef(a);

    ex = ex - 55;
    let q = (ex > (700 - 55)) as i32 & (-64);
    a = vldexp3_vd_vd_vi_purec_scalar_sleef(a, q);
    ex = (ex >> 31) & (!ex);
    ex = vsll_vi_vi_i_purec_scalar_sleef(ex, 2);
    let x = ddmul_vd2_vd_vd_purec_scalar_sleef(a, Sleef_rempitabdp[ex.into()]);
    let di: di_t_purec_scalar_sleef = rempisub_purec_scalar_sleef(x.x);
    q = di.i;
    x.x = di.d;
    x = ddnormalize_vd2_vd2_purec_scalar_sleef(x);
    let mut y = ddmul_vd2_vd_vd_purec_scalar_sleef(a, Sleef_rempitabdp[1 + ex.into()]);
    x = ddadd2_vd2_vd2_vd2_purec_scalar_sleef(x, y);
    di = rempisub_purec_scalar_sleef(x.x);
    q = q + di.i;
    x.x = di.d;
    x = ddnormalize_vd2_vd2_purec_scalar_sleef(x);
    y = vdouble2_purec_scalar_sleef {
        x: Sleef_rempitabdp[2 + ex.into()],
        y: Sleef_rempitabdp[3 + ex.into()],
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

fn vmulsign_vd_vd_vd_purec_scalar_sleef(
    x: vdouble_purec_scalar_sleef,
    y: vdouble_purec_scalar_sleef,
) -> vdouble_purec_scalar_sleef {
    f64::from_bits(x.to_bits() ^ vsignbit_vm_vd_purec_scalar_sleef(y))
}

fn vsignbit_vm_vd_purec_scalar_sleef(d: vdouble_purec_scalar_sleef) -> vmask_purec_scalar_sleef {
    d.to_bits() & (-0.0f64).to_bits()
}

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
        y: vadd_vd_6vd_purec_scalar_sleef(xh * yh, -s, xl * yh, xh * yl, xl * yl, x.y * y),
    }
}

// FIXME: correct?
fn vupper_vd_vd_purec_scalar_sleef(d: vdouble_purec_scalar_sleef) -> vdouble_purec_scalar_sleef {
    f64::from_bits(d.to_bits() & ((0xffffffffu64 << 32) | 0xf8000000u64))
}
