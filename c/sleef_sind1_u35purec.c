#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sleefinline_purec_scalar.h>

// Useful for debugging
//
// Debug in VSCode by running "Debug C/C++ file" under play icon in top right
// corner while in this file.
int main(int argc, char **argv) {
    double a = 27503860628390360000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0;

    double result = Sleef_sind1_u35purec(a);

    printf("%g", result);
}