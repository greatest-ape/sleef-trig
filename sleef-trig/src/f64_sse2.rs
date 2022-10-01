//   Copyright Naoki Shibata and contributors 2010 - 2021.
// Distributed under the Boost Software License, Version 1.0.
//        (See http://www.boost.org/LICENSE_1_0.txt)

#![allow(non_camel_case_types, non_snake_case)]

#[cfg(not(target_arch = "x86_64"))]
compile_error!("Attempted to compile the SSE2 implementation on a target arch other than x86_64");

use core::arch::x86_64::*;

use super::f64_tables::*;

type vmask_sse2_sleef = __m128i;
type vopmask_sse2_sleef = __m128i;

type vdouble_sse2_sleef = __m128d;
type vint_sse2_sleef = __m128i;

macro_rules! vsll_vi_vi_i_sse2_sleef {
    ($x:expr, $c:expr) => {
        _mm_slli_epi32($x, $c)
    };
}

macro_rules! vsra_vi_vi_i_sse2_sleef {
    ($x:expr, $c:expr) => {
        _mm_srai_epi32($x, $c)
    };
}

macro_rules! vsrl_vi_vi_i_sse2_sleef {
    ($x:expr, $c:expr) => {
        _mm_srli_epi32($x, $c)
    };
}

#[derive(Clone, Copy, Debug)]
struct vdouble2_sse2_sleef {
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
}

#[derive(Clone, Copy, Debug)]
struct di_t_sse2_sleef {
    d: vdouble_sse2_sleef,
    i: vint_sse2_sleef,
}

#[derive(Clone, Copy, Debug)]
struct ddi_t_sse2_sleef {
    dd_sse2_sleef: vdouble2_sse2_sleef,
    i: vint_sse2_sleef,
}

/// Evaluate the sine function with an error bound of 3.5 ULP.
pub fn Sleef_sind2_u35sse2(d: __m128d) -> __m128d {
    // Safe because SSE2 is always supported on x86_64, which is the only
    // platform on which this module is compiled
    unsafe { Sleef_sind2_u35sse2_inner(d) }
}

#[inline(always)]
unsafe fn Sleef_sind2_u35sse2_inner(mut d: __m128d) -> __m128d {
    let r = d;

    let ql = if (vtestallones_i_vo64_sse2_sleef(vlt_vo_vd_vd_sse2_sleef(
        vabs_vd_vd_sse2_sleef(d),
        vcast_vd_d_sse2_sleef(15.0),
    ))) != 0
    {
        let dql: vdouble_sse2_sleef = vrint_vd_vd_sse2_sleef(vmul_vd_vd_vd_sse2_sleef(
            d,
            vcast_vd_d_sse2_sleef(0.318309886183790671537767526745028724),
        ));
        let ql = vrint_vi_vd_sse2_sleef(dql);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dql, vcast_vd_d_sse2_sleef(-3.141592653589793116), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dql, vcast_vd_d_sse2_sleef(-1.2246467991473532072e-16), d);

        ql
    } else if vtestallones_i_vo64_sse2_sleef(vlt_vo_vd_vd_sse2_sleef(
        vabs_vd_vd_sse2_sleef(d),
        vcast_vd_d_sse2_sleef(1e+14),
    )) != 0
    {
        let mut dqh: vdouble_sse2_sleef = vtruncate_vd_vd_sse2_sleef(vmul_vd_vd_vd_sse2_sleef(
            d,
            vcast_vd_d_sse2_sleef(0.318309886183790671537767526745028724 / f64::from(1 << 24)),
        ));
        dqh = vmul_vd_vd_vd_sse2_sleef(dqh, vcast_vd_d_sse2_sleef(f64::from(1 << 24)));
        let dql: vdouble_sse2_sleef = vrint_vd_vd_sse2_sleef(vmlapn_vd_vd_vd_vd_sse2_sleef(
            d,
            vcast_vd_d_sse2_sleef(0.318309886183790671537767526745028724),
            dqh,
        ));
        let ql = vrint_vi_vd_sse2_sleef(dql);

        d = vmla_vd_vd_vd_vd_sse2_sleef(dqh, vcast_vd_d_sse2_sleef(-3.1415926218032836914), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dql, vcast_vd_d_sse2_sleef(-3.1415926218032836914), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dqh, vcast_vd_d_sse2_sleef(-3.1786509424591713469e-08), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dql, vcast_vd_d_sse2_sleef(-3.1786509424591713469e-08), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dqh, vcast_vd_d_sse2_sleef(-1.2246467864107188502e-16), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(dql, vcast_vd_d_sse2_sleef(-1.2246467864107188502e-16), d);
        d = vmla_vd_vd_vd_vd_sse2_sleef(
            vadd_vd_vd_vd_sse2_sleef(dqh, dql),
            vcast_vd_d_sse2_sleef(-1.2736634327021899816e-24),
            d,
        );

        ql
    } else {
        let mut ddi_sse2_sleef: ddi_t_sse2_sleef = rempi_sse2_sleef(d);
        let mut ql = vand_vi_vi_vi_sse2_sleef(ddi_sse2_sleef.i, vcast_vi_i_sse2_sleef(3));
        ql = vadd_vi_vi_vi_sse2_sleef(
            vadd_vi_vi_vi_sse2_sleef(ql, ql),
            vsel_vi_vo_vi_vi_sse2_sleef(
                vcast_vo32_vo64_sse2_sleef(vgt_vo_vd_vd_sse2_sleef(
                    ddi_sse2_sleef.dd_sse2_sleef.x,
                    vcast_vd_d_sse2_sleef(0.0),
                )),
                vcast_vi_i_sse2_sleef(2),
                vcast_vi_i_sse2_sleef(1),
            ),
        );
        ql = vsra_vi_vi_i_sse2_sleef!(ql, 2);
        let o: vopmask_sse2_sleef = veq_vo_vi_vi_sse2_sleef(
            vand_vi_vi_vi_sse2_sleef(ddi_sse2_sleef.i, vcast_vi_i_sse2_sleef(1)),
            vcast_vi_i_sse2_sleef(1),
        );
        let mut x: vdouble2_sse2_sleef = vdouble2_sse2_sleef {
            x: vmulsign_vd_vd_vd_sse2_sleef(
                vcast_vd_d_sse2_sleef(-3.141592653589793116 * 0.5),
                ddi_sse2_sleef.dd_sse2_sleef.x,
            ),
            y: vmulsign_vd_vd_vd_sse2_sleef(
                vcast_vd_d_sse2_sleef(-1.2246467991473532072e-16 * 0.5),
                ddi_sse2_sleef.dd_sse2_sleef.x,
            ),
        };
        x = ddadd2_vd2_vd2_vd2_sse2_sleef(ddi_sse2_sleef.dd_sse2_sleef, x);
        ddi_sse2_sleef.dd_sse2_sleef = vsel_vd2_vo_vd2_vd2_sse2_sleef(
            vcast_vo64_vo32_sse2_sleef(o),
            x,
            ddi_sse2_sleef.dd_sse2_sleef,
        );
        d = vadd_vd_vd_vd_sse2_sleef(
            ddi_sse2_sleef.dd_sse2_sleef.x,
            ddi_sse2_sleef.dd_sse2_sleef.y,
        );
        d = vreinterpret_vd_vm_sse2_sleef(vor_vm_vo64_vm_sse2_sleef(
            vor_vo_vo_vo_sse2_sleef(visinf_vo_vd_sse2_sleef(r), visnan_vo_vd_sse2_sleef(r)),
            vreinterpret_vm_vd_sse2_sleef(d),
        ));

        ql
    };

    let s = vmul_vd_vd_vd_sse2_sleef(d, d);

    d = vreinterpret_vd_vm_sse2_sleef(vxor_vm_vm_vm_sse2_sleef(
        vand_vm_vo64_vm_sse2_sleef(
            vcast_vo64_vo32_sse2_sleef(veq_vo_vi_vi_sse2_sleef(
                vand_vi_vi_vi_sse2_sleef(ql, vcast_vi_i_sse2_sleef(1)),
                vcast_vi_i_sse2_sleef(1),
            )),
            vreinterpret_vm_vd_sse2_sleef(vcast_vd_d_sse2_sleef(-0.0)),
        ),
        vreinterpret_vm_vd_sse2_sleef(d),
    ));

    let s2: vdouble_sse2_sleef = vmul_vd_vd_vd_sse2_sleef(s, s);
    let s4: vdouble_sse2_sleef = vmul_vd_vd_vd_sse2_sleef(s2, s2);

    let mut u = vmla_vd_vd_vd_vd_sse2_sleef(
        s4,
        vmla_vd_vd_vd_vd_sse2_sleef(
            s2,
            vmla_vd_vd_vd_vd_sse2_sleef(
                s,
                vcast_vd_d_sse2_sleef(-7.97255955009037868891952e-18),
                vcast_vd_d_sse2_sleef(2.81009972710863200091251e-15),
            ),
            vmla_vd_vd_vd_vd_sse2_sleef(
                s,
                vcast_vd_d_sse2_sleef(-7.64712219118158833288484e-13),
                vcast_vd_d_sse2_sleef(1.60590430605664501629054e-10),
            ),
        ),
        vmla_vd_vd_vd_vd_sse2_sleef(
            s2,
            vmla_vd_vd_vd_vd_sse2_sleef(
                s,
                vcast_vd_d_sse2_sleef(-2.50521083763502045810755e-08),
                vcast_vd_d_sse2_sleef(2.75573192239198747630416e-06),
            ),
            vmla_vd_vd_vd_vd_sse2_sleef(
                s,
                vcast_vd_d_sse2_sleef(-0.000198412698412696162806809),
                vcast_vd_d_sse2_sleef(0.00833333333333332974823815),
            ),
        ),
    );

    u = vmla_vd_vd_vd_vd_sse2_sleef(u, s, vcast_vd_d_sse2_sleef(-0.166666666666666657414808));

    u = vadd_vd_vd_vd_sse2_sleef(
        vmul_vd_vd_vd_sse2_sleef(s, vmul_vd_vd_vd_sse2_sleef(u, d)),
        d,
    );

    u = vsel_vd_vo_vd_vd_sse2_sleef(visnegzero_vo_vd_sse2_sleef(r), r, u);

    u
}

#[inline]
#[cold]
unsafe fn rempi_sse2_sleef(mut a: vdouble_sse2_sleef) -> ddi_t_sse2_sleef {
    let mut ex: vint_sse2_sleef = vilogb2k_vi_vd_sse2_sleef(a);

    ex = vsub_vi_vi_vi_sse2_sleef(ex, vcast_vi_i_sse2_sleef(55));
    let mut q: vint_sse2_sleef = vand_vi_vo_vi_sse2_sleef(
        vgt_vo_vi_vi_sse2_sleef(ex, vcast_vi_i_sse2_sleef(700 - 55)),
        vcast_vi_i_sse2_sleef(-64),
    );
    a = vldexp3_vd_vd_vi_sse2_sleef(a, q);
    ex = vandnot_vi_vi_vi_sse2_sleef(vsra_vi_vi_i_sse2_sleef!(ex, 31), ex);
    ex = vsll_vi_vi_i_sse2_sleef!(ex, 2);
    let mut x = ddmul_vd2_vd_vd_sse2_sleef(a, vgather_vd_p_vi_sse2_sleef(Sleef_rempitabdp, ex));
    let mut di: di_t_sse2_sleef = rempisub_sse2_sleef(x.x);
    q = di.i;
    x.x = di.d;
    x = ddnormalize_vd2_vd2_sse2_sleef(x);
    let mut y =
        ddmul_vd2_vd_vd_sse2_sleef(a, vgather_vd_p_vi_sse2_sleef(&Sleef_rempitabdp[1..], ex));
    x = ddadd2_vd2_vd2_vd2_sse2_sleef(x, y);
    di = rempisub_sse2_sleef(x.x);
    q = vadd_vi_vi_vi_sse2_sleef(q, di.i);
    x.x = di.d;
    x = ddnormalize_vd2_vd2_sse2_sleef(x);
    y = vdouble2_sse2_sleef {
        x: vgather_vd_p_vi_sse2_sleef(&Sleef_rempitabdp[2..], ex),
        y: vgather_vd_p_vi_sse2_sleef(&Sleef_rempitabdp[3..], ex),
    };
    y = ddmul_vd2_vd2_vd_sse2_sleef(y, a);
    x = ddadd2_vd2_vd2_vd2_sse2_sleef(x, y);
    x = ddnormalize_vd2_vd2_sse2_sleef(x);
    x = ddmul_vd2_vd2_vd2_sse2_sleef(
        x,
        vcast_vd2_d_d_sse2_sleef(3.141592653589793116 * 2.0, 1.2246467991473532072e-16 * 2.0),
    );
    let o: vopmask_sse2_sleef =
        vlt_vo_vd_vd_sse2_sleef(vabs_vd_vd_sse2_sleef(a), vcast_vd_d_sse2_sleef(0.7));
    x.x = vsel_vd_vo_vd_vd_sse2_sleef(o, a, x.x);
    x.y = vreinterpret_vd_vm_sse2_sleef(vandnot_vm_vo64_vm_sse2_sleef(
        o,
        vreinterpret_vm_vd_sse2_sleef(x.y),
    ));

    ddi_t_sse2_sleef {
        dd_sse2_sleef: x,
        i: q,
    }
}

#[inline(always)]
unsafe fn vsel_vd2_vo_vd2_vd2_sse2_sleef(
    m: vopmask_sse2_sleef,
    x: vdouble2_sse2_sleef,
    y: vdouble2_sse2_sleef,
) -> vdouble2_sse2_sleef {
    vdouble2_sse2_sleef {
        x: vsel_vd_vo_vd_vd_sse2_sleef(m, x.x, y.x),
        y: vsel_vd_vo_vd_vd_sse2_sleef(m, x.y, y.y),
    }
}

#[inline(always)]
unsafe fn vsel_vd_vo_vd_vd_sse2_sleef(
    opmask: vopmask_sse2_sleef,
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return _mm_or_pd(
        _mm_and_pd(_mm_castsi128_pd(opmask), x),
        _mm_andnot_pd(_mm_castsi128_pd(opmask), y),
    );
}

#[inline(always)]
unsafe fn vtestallones_i_vo64_sse2_sleef(g: vopmask_sse2_sleef) -> i32 {
    (_mm_movemask_epi8(g) == 0xFFFF) as i32
}

#[inline(always)]
unsafe fn ddadd2_vd2_vd2_vd2_sse2_sleef(
    x: vdouble2_sse2_sleef,
    y: vdouble2_sse2_sleef,
) -> vdouble2_sse2_sleef {
    let s: vdouble_sse2_sleef = vadd_vd_vd_vd_sse2_sleef(x.x, y.x);
    let v: vdouble_sse2_sleef = vsub_vd_vd_vd_sse2_sleef(s, x.x);
    let t: vdouble_sse2_sleef = vadd_vd_vd_vd_sse2_sleef(
        vsub_vd_vd_vd_sse2_sleef(x.x, vsub_vd_vd_vd_sse2_sleef(s, v)),
        vsub_vd_vd_vd_sse2_sleef(y.x, v),
    );
    return vdouble2_sse2_sleef {
        x: s,
        y: vadd_vd_vd_vd_sse2_sleef(t, vadd_vd_vd_vd_sse2_sleef(x.y, y.y)),
    };
}

#[inline(always)]
unsafe fn vlt_vo_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vopmask_sse2_sleef {
    _mm_castpd_si128(_mm_cmplt_pd(x, y))
}

#[inline(always)]
unsafe fn vreinterpret_vm_vd_sse2_sleef(vd_sse2_sleef: vdouble_sse2_sleef) -> vmask_sse2_sleef {
    _mm_castpd_si128(vd_sse2_sleef)
}

#[inline(always)]
unsafe fn vabs_vd_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vdouble_sse2_sleef {
    _mm_andnot_pd(_mm_set1_pd(-0.0), d)
}

#[inline(always)]
unsafe fn vcast_vd_d_sse2_sleef(d: f64) -> vdouble_sse2_sleef {
    _mm_set1_pd(d)
}

#[inline(always)]
unsafe fn vrint_vd_vd_sse2_sleef(vd_sse2_sleef: vdouble_sse2_sleef) -> vdouble_sse2_sleef {
    _mm_cvtepi32_pd(vrint_vi_vd_sse2_sleef(vd_sse2_sleef))
}

#[inline(always)]
unsafe fn vmul_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    _mm_mul_pd(x, y)
}

#[inline(always)]
unsafe fn vadd_vi_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vint_sse2_sleef {
    _mm_add_epi32(x, y)
}

#[inline(always)]
unsafe fn vand_vi_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vint_sse2_sleef {
    _mm_and_si128(x, y)
}

#[inline(always)]
unsafe fn vmla_vd_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
    z: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    vadd_vd_vd_vd_sse2_sleef(vmul_vd_vd_vd_sse2_sleef(x, y), z)
}

#[inline(always)]
unsafe fn vadd_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    _mm_add_pd(x, y)
}

#[inline(always)]
unsafe fn vrint_vi_vd_sse2_sleef(vd_sse2_sleef: vdouble_sse2_sleef) -> vint_sse2_sleef {
    _mm_cvtpd_epi32(vd_sse2_sleef)
}

#[inline(always)]
unsafe fn vtruncate_vd_vd_sse2_sleef(vd_sse2_sleef: vdouble_sse2_sleef) -> vdouble_sse2_sleef {
    _mm_cvtepi32_pd(_mm_cvttpd_epi32(vd_sse2_sleef))
}

#[inline(always)]
unsafe fn vmlapn_vd_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
    z: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vsub_vd_vd_vd_sse2_sleef(vmul_vd_vd_vd_sse2_sleef(x, y), z);
}

#[inline(always)]
unsafe fn vsub_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    _mm_sub_pd(x, y)
}

#[inline(always)]
unsafe fn vcast_vi_i_sse2_sleef(i: i32) -> vint_sse2_sleef {
    _mm_set_epi32(0, 0, i, i)
}

#[inline(always)]
unsafe fn vsel_vi_vo_vi_vi_sse2_sleef(
    o: vopmask_sse2_sleef,
    x: vint_sse2_sleef,
    y: vint_sse2_sleef,
) -> vint_sse2_sleef {
    _mm_or_si128(vand_vm_vm_vm_sse2_sleef(o, x), _mm_andnot_si128(o, y))
}

#[inline(always)]
unsafe fn vcast_vo32_vo64_sse2_sleef(o: vopmask_sse2_sleef) -> vopmask_sse2_sleef {
    _mm_shuffle_epi32(o, 0x08)
}

#[inline(always)]
unsafe fn vgt_vo_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vopmask_sse2_sleef {
    _mm_castpd_si128(_mm_cmpgt_pd(x, y))
}

#[inline(always)]
unsafe fn vreinterpret_vd_vm_sse2_sleef(vm: vmask_sse2_sleef) -> vdouble_sse2_sleef {
    _mm_castsi128_pd(vm)
}

#[inline(always)]
unsafe fn veq_vo_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vopmask_sse2_sleef {
    return _mm_cmpeq_epi32(x, y);
}

#[inline(always)]
unsafe fn vmulsign_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    vreinterpret_vd_vm_sse2_sleef(vxor_vm_vm_vm_sse2_sleef(
        vreinterpret_vm_vd_sse2_sleef(x),
        vsignbit_vm_vd_sse2_sleef(y),
    ))
}

#[inline(always)]
unsafe fn vsignbit_vm_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vmask_sse2_sleef {
    return vand_vm_vm_vm_sse2_sleef(
        vreinterpret_vm_vd_sse2_sleef(d),
        vreinterpret_vm_vd_sse2_sleef(vcast_vd_d_sse2_sleef(-0.0)),
    );
}

#[inline(always)]
unsafe fn vand_vm_vm_vm_sse2_sleef(x: vmask_sse2_sleef, y: vmask_sse2_sleef) -> vmask_sse2_sleef {
    _mm_and_si128(x, y)
}
#[inline(always)]
unsafe fn vxor_vm_vm_vm_sse2_sleef(x: vmask_sse2_sleef, y: vmask_sse2_sleef) -> vmask_sse2_sleef {
    _mm_xor_si128(x, y)
}

#[inline(always)]
unsafe fn vor_vm_vo64_vm_sse2_sleef(
    x: vopmask_sse2_sleef,
    y: vmask_sse2_sleef,
) -> vmask_sse2_sleef {
    _mm_or_si128(x, y)
}

#[inline(always)]
unsafe fn vand_vm_vo64_vm_sse2_sleef(
    x: vopmask_sse2_sleef,
    y: vmask_sse2_sleef,
) -> vmask_sse2_sleef {
    _mm_and_si128(x, y)
}

#[inline(always)]
unsafe fn vandnot_vm_vo64_vm_sse2_sleef(
    x: vopmask_sse2_sleef,
    y: vmask_sse2_sleef,
) -> vmask_sse2_sleef {
    _mm_andnot_si128(x, y)
}

#[inline(always)]
unsafe fn vcast_vo64_vo32_sse2_sleef(o: vopmask_sse2_sleef) -> vopmask_sse2_sleef {
    _mm_shuffle_epi32(o, 0x50)
}

#[inline(always)]
unsafe fn vor_vo_vo_vo_sse2_sleef(
    x: vopmask_sse2_sleef,
    y: vopmask_sse2_sleef,
) -> vopmask_sse2_sleef {
    _mm_or_si128(x, y)
}

#[inline(always)]
unsafe fn visinf_vo_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vopmask_sse2_sleef {
    vreinterpret_vm_vd_sse2_sleef(_mm_cmpeq_pd(
        vabs_vd_vd_sse2_sleef(d),
        _mm_set1_pd(f64::INFINITY),
    ))
}

#[inline(always)]
unsafe fn visnan_vo_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vopmask_sse2_sleef {
    return vreinterpret_vm_vd_sse2_sleef(_mm_cmpneq_pd(d, d));
}

#[inline(always)]
unsafe fn visnegzero_vo_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vopmask_sse2_sleef {
    return veq64_vo_vm_vm_sse2_sleef(
        vreinterpret_vm_vd_sse2_sleef(d),
        vreinterpret_vm_vd_sse2_sleef(vcast_vd_d_sse2_sleef(-0.0)),
    );
}

#[inline(always)]
unsafe fn veq64_vo_vm_vm_sse2_sleef(
    x: vmask_sse2_sleef,
    y: vmask_sse2_sleef,
) -> vopmask_sse2_sleef {
    let t = _mm_cmpeq_epi32(x, y);
    vand_vm_vm_vm_sse2_sleef(t, _mm_shuffle_epi32(t, 0xb1))
}

#[inline(always)]
unsafe fn vilogb2k_vi_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vint_sse2_sleef {
    let mut q = vcastu_vi_vm_sse2_sleef(vreinterpret_vm_vd_sse2_sleef(d));
    q = vsrl_vi_vi_i_sse2_sleef!(q, 20);
    q = vand_vi_vi_vi_sse2_sleef(q, vcast_vi_i_sse2_sleef(0x7ff));
    q = vsub_vi_vi_vi_sse2_sleef(q, vcast_vi_i_sse2_sleef(0x3ff));
    return q;
}

#[inline(always)]
unsafe fn vcastu_vi_vm_sse2_sleef(vi: vmask_sse2_sleef) -> vint_sse2_sleef {
    _mm_shuffle_epi32(vi, 0x0d)
}

#[inline(always)]
unsafe fn vsub_vi_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vint_sse2_sleef {
    _mm_sub_epi32(x, y)
}

#[inline(always)]
unsafe fn vand_vi_vo_vi_sse2_sleef(m: vopmask_sse2_sleef, y: vint_sse2_sleef) -> vint_sse2_sleef {
    _mm_and_si128(m, y)
}

#[inline(always)]
unsafe fn vgt_vo_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vopmask_sse2_sleef {
    _mm_cmpgt_epi32(x, y)
}

#[inline(always)]
unsafe fn vldexp3_vd_vd_vi_sse2_sleef(
    d: vdouble_sse2_sleef,
    q: vint_sse2_sleef,
) -> vdouble_sse2_sleef {
    vreinterpret_vd_vm_sse2_sleef(vadd64_vm_vm_vm_sse2_sleef(
        vreinterpret_vm_vd_sse2_sleef(d),
        vcastu_vm_vi_sse2_sleef(vsll_vi_vi_i_sse2_sleef!(q, 20)),
    ))
}

#[inline(always)]
unsafe fn vcastu_vm_vi_sse2_sleef(vi: vint_sse2_sleef) -> vmask_sse2_sleef {
    _mm_and_si128(_mm_shuffle_epi32(vi, 0x73), _mm_set_epi32(-1, 0, -1, 0))
}

#[inline(always)]
unsafe fn vadd64_vm_vm_vm_sse2_sleef(x: vmask_sse2_sleef, y: vmask_sse2_sleef) -> vmask_sse2_sleef {
    _mm_add_epi64(x, y)
}

#[inline(always)]
unsafe fn vandnot_vi_vi_vi_sse2_sleef(x: vint_sse2_sleef, y: vint_sse2_sleef) -> vint_sse2_sleef {
    _mm_andnot_si128(x, y)
}

#[inline(always)]
unsafe fn ddmul_vd2_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble2_sse2_sleef {
    let xh = vupper_vd_vd_sse2_sleef(x);
    let xl = vsub_vd_vd_vd_sse2_sleef(x, xh);
    let yh = vupper_vd_vd_sse2_sleef(y);
    let yl = vsub_vd_vd_vd_sse2_sleef(y, yh);

    let s = vmul_vd_vd_vd_sse2_sleef(x, y);

    vdouble2_sse2_sleef {
        x: s,
        y: vadd_vd_5vd_sse2_sleef(
            vmul_vd_vd_vd_sse2_sleef(xh, yh),
            vneg_vd_vd_sse2_sleef(s),
            vmul_vd_vd_vd_sse2_sleef(xl, yh),
            vmul_vd_vd_vd_sse2_sleef(xh, yl),
            vmul_vd_vd_vd_sse2_sleef(xl, yl),
        ),
    }
}

#[inline(always)]
unsafe fn ddmul_vd2_vd2_vd2_sse2_sleef(
    x: vdouble2_sse2_sleef,
    y: vdouble2_sse2_sleef,
) -> vdouble2_sse2_sleef {
    let xh = vupper_vd_vd_sse2_sleef(x.x);
    let xl = vsub_vd_vd_vd_sse2_sleef(x.x, xh);
    let yh = vupper_vd_vd_sse2_sleef(y.x);
    let yl = vsub_vd_vd_vd_sse2_sleef(y.x, yh);

    let s = vmul_vd_vd_vd_sse2_sleef(x.x, y.x);

    vdouble2_sse2_sleef {
        x: s,
        y: vadd_vd_7vd_sse2_sleef(
            vmul_vd_vd_vd_sse2_sleef(xh, yh),
            vneg_vd_vd_sse2_sleef(s),
            vmul_vd_vd_vd_sse2_sleef(xl, yh),
            vmul_vd_vd_vd_sse2_sleef(xh, yl),
            vmul_vd_vd_vd_sse2_sleef(xl, yl),
            vmul_vd_vd_vd_sse2_sleef(x.x, y.y),
            vmul_vd_vd_vd_sse2_sleef(x.y, y.x),
        ),
    }
}

#[allow(overflowing_literals)]
#[inline(always)]
unsafe fn vupper_vd_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vdouble_sse2_sleef {
    return vreinterpret_vd_vm_sse2_sleef(vand_vm_vm_vm_sse2_sleef(
        vreinterpret_vm_vd_sse2_sleef(d),
        vcast_vm_i_i_sse2_sleef(0xffffffff, 0xf8000000),
    ));
}

#[inline(always)]
unsafe fn vcast_vm_i_i_sse2_sleef(i0: i32, i1: i32) -> vmask_sse2_sleef {
    return _mm_set_epi32(i0, i1, i0, i1);
}

#[inline(always)]
unsafe fn vadd_vd_3vd_sse2_sleef(
    v0: vdouble_sse2_sleef,
    v1: vdouble_sse2_sleef,
    v2: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vadd_vd_vd_vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(v0, v1), v2);
}

#[inline(always)]
unsafe fn vadd_vd_4vd_sse2_sleef(
    v0: vdouble_sse2_sleef,
    v1: vdouble_sse2_sleef,
    v2: vdouble_sse2_sleef,
    v3: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vadd_vd_3vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(v0, v1), v2, v3);
}

#[inline(always)]
unsafe fn vadd_vd_5vd_sse2_sleef(
    v0: vdouble_sse2_sleef,
    v1: vdouble_sse2_sleef,
    v2: vdouble_sse2_sleef,
    v3: vdouble_sse2_sleef,
    v4: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vadd_vd_4vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(v0, v1), v2, v3, v4);
}

#[inline(always)]
unsafe fn vadd_vd_6vd_sse2_sleef(
    v0: vdouble_sse2_sleef,
    v1: vdouble_sse2_sleef,
    v2: vdouble_sse2_sleef,
    v3: vdouble_sse2_sleef,
    v4: vdouble_sse2_sleef,
    v5: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vadd_vd_5vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(v0, v1), v2, v3, v4, v5);
}

#[inline(always)]
unsafe fn vadd_vd_7vd_sse2_sleef(
    v0: vdouble_sse2_sleef,
    v1: vdouble_sse2_sleef,
    v2: vdouble_sse2_sleef,
    v3: vdouble_sse2_sleef,
    v4: vdouble_sse2_sleef,
    v5: vdouble_sse2_sleef,
    v6: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vadd_vd_6vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(v0, v1), v2, v3, v4, v5, v6);
}

#[inline(always)]
unsafe fn vneg_vd_vd_sse2_sleef(d: vdouble_sse2_sleef) -> vdouble_sse2_sleef {
    return _mm_xor_pd(_mm_set1_pd(-0.0), d);
}

#[inline(always)]
unsafe fn vgather_vd_p_vi_sse2_sleef(slice: &[f64], vi: vint_sse2_sleef) -> vdouble_sse2_sleef {
    let mut a = [0; 4];
    vstoreu_v_p_vi_sse2_sleef(a.as_mut_ptr(), vi);
    return _mm_set_pd(
        slice.get(a[1] as usize).copied().unwrap_or(f64::NAN),
        slice.get(a[0] as usize).copied().unwrap_or(f64::NAN),
    );
}

#[inline(always)]
unsafe fn vstoreu_v_p_vi_sse2_sleef(p: *mut i32, v: vint_sse2_sleef) {
    _mm_storeu_si128(p as *mut __m128i, v);
}

#[inline(always)]
unsafe fn rempisub_sse2_sleef(x: vdouble_sse2_sleef) -> di_t_sse2_sleef {
    let c = vmulsign_vd_vd_vd_sse2_sleef(vcast_vd_d_sse2_sleef((1i64 << 52) as f64), x);
    let rint4x = vsel_vd_vo_vd_vd_sse2_sleef(
        vgt_vo_vd_vd_sse2_sleef(
            vabs_vd_vd_sse2_sleef(vmul_vd_vd_vd_sse2_sleef(vcast_vd_d_sse2_sleef(4.0), x)),
            vcast_vd_d_sse2_sleef((1i64 << 52) as f64),
        ),
        vmul_vd_vd_vd_sse2_sleef(vcast_vd_d_sse2_sleef(4.0), x),
        vorsign_vd_vd_vd_sse2_sleef(
            vsub_vd_vd_vd_sse2_sleef(
                vmla_vd_vd_vd_vd_sse2_sleef(vcast_vd_d_sse2_sleef(4.0), x, c),
                c,
            ),
            x,
        ),
    );
    let rintx = vsel_vd_vo_vd_vd_sse2_sleef(
        vgt_vo_vd_vd_sse2_sleef(
            vabs_vd_vd_sse2_sleef(x),
            vcast_vd_d_sse2_sleef((1i64 << 52) as f64),
        ),
        x,
        vorsign_vd_vd_vd_sse2_sleef(
            vsub_vd_vd_vd_sse2_sleef(vadd_vd_vd_vd_sse2_sleef(x, c), c),
            x,
        ),
    );
    di_t_sse2_sleef {
        d: vmla_vd_vd_vd_vd_sse2_sleef(vcast_vd_d_sse2_sleef(-0.25), rint4x, x),
        i: vtruncate_vi_vd_sse2_sleef(vmla_vd_vd_vd_vd_sse2_sleef(
            vcast_vd_d_sse2_sleef(-4.0),
            rintx,
            rint4x,
        )),
    }
}

#[inline(always)]
unsafe fn vorsign_vd_vd_vd_sse2_sleef(
    x: vdouble_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble_sse2_sleef {
    return vreinterpret_vd_vm_sse2_sleef(_mm_or_si128(
        _mm_castpd_si128(x),
        vsignbit_vm_vd_sse2_sleef(y),
    ));
}

#[inline(always)]
unsafe fn vtruncate_vi_vd_sse2_sleef(vd_sse2_sleef: vdouble_sse2_sleef) -> vint_sse2_sleef {
    return _mm_cvttpd_epi32(vd_sse2_sleef);
}

#[inline(always)]
unsafe fn ddnormalize_vd2_vd2_sse2_sleef(t: vdouble2_sse2_sleef) -> vdouble2_sse2_sleef {
    let s = vadd_vd_vd_vd_sse2_sleef(t.x, t.y);

    vdouble2_sse2_sleef {
        x: s,
        y: vadd_vd_vd_vd_sse2_sleef(vsub_vd_vd_vd_sse2_sleef(t.x, s), t.y),
    }
}

#[inline(always)]
unsafe fn ddmul_vd2_vd2_vd_sse2_sleef(
    x: vdouble2_sse2_sleef,
    y: vdouble_sse2_sleef,
) -> vdouble2_sse2_sleef {
    let xh = vupper_vd_vd_sse2_sleef(x.x);
    let xl = vsub_vd_vd_vd_sse2_sleef(x.x, xh);
    let yh = vupper_vd_vd_sse2_sleef(y);
    let yl = vsub_vd_vd_vd_sse2_sleef(y, yh);

    let s = vmul_vd_vd_vd_sse2_sleef(x.x, y);

    vdouble2_sse2_sleef {
        x: s,
        y: vadd_vd_6vd_sse2_sleef(
            vmul_vd_vd_vd_sse2_sleef(xh, yh),
            vneg_vd_vd_sse2_sleef(s),
            vmul_vd_vd_vd_sse2_sleef(xl, yh),
            vmul_vd_vd_vd_sse2_sleef(xh, yl),
            vmul_vd_vd_vd_sse2_sleef(xl, yl),
            vmul_vd_vd_vd_sse2_sleef(x.y, y),
        ),
    }
}

#[inline(always)]
unsafe fn vcast_vd2_d_d_sse2_sleef(h: f64, l: f64) -> vdouble2_sse2_sleef {
    vdouble2_sse2_sleef {
        x: vcast_vd_d_sse2_sleef(h),
        y: vcast_vd_d_sse2_sleef(l),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::*;

    #[test]
    fn test_sind2_u35sse2() {
        unsafe fn to_array(a: __m128d) -> [f64; 2] {
            let mut arr = [0.0; 2];

            _mm_storeu_pd(arr.as_mut_ptr(), a);

            arr
        }

        unsafe fn reference(a: f64) -> [f64; 2] {
            to_array(sleef_trig_sys::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0)))
        }

        unsafe fn port(a: f64) -> [f64; 2] {
            to_array(super::Sleef_sind2_u35sse2(_mm_set_pd(a, a + 1.0)))
        }

        fn prop(a: f64) -> TestResult {
            let (result, reference) = unsafe { (port(a), reference(a)) };

            let success = result
                .into_iter()
                .zip(reference)
                .all(|(a, b)| a == b || (a.is_nan() && b.is_nan()));

            if !success {
                println!();
                println!("input: {:?}", a);
                println!("result: {:?}", result);
                println!("reference: {:?}", reference);
            }

            TestResult::from_bool(success)
        }

        quickcheck(prop as fn(f64) -> TestResult);
    }
}
