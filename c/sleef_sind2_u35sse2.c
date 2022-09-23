#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <x86intrin.h>

#include <sleefinline_sse2.h>

// Useful for debugging
//
// Debug in VSCode by running "Debug C/C++ file" under play icon in top right
// corner while in this file.
int main(int argc, char **argv) {
    double a = 1.2926575274766375e38;

    __m128d vc = Sleef_sind2_u35sse2(_mm_set1_pd(a));

    double result[2];

    _mm_storeu_pd(result, vc);

    printf("%g, %g", result[0], result[1]);
}