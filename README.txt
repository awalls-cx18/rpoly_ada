1. src/math.ads

A thin Ada wrapper around select libm math functions: cos, sin, exp, sqrt, log.
This exists because the GNAT system included with Fedora 9 (so very long ago)
did not properly convert floats when implementing these functions.  (The Intel
80-bit floating point type was mishandled, causing errant results.)


2. test/mtest.adb

A unit test program to test the math.ads wrapper.
It outputs a row of numbers for each degree in [0,360] degrees with the
following values:

    degrees  radians  sqrtf(radians)  cosf(radians)  sinf(radians)

which can be inspected for correctness.

$ gnatmake test/mtest.adb -Isrc -largs -lm
gcc -c -Itest/ -Isrc -I- test/mtest.adb
gcc -c -Itest/ -Isrc -I- /(path)/src/math.ads
gnatbind -Isrc -x mtest.ali
gnatlink mtest.ali -lm

$ ./mtest


3. src/generic_polynomial.ad[bs]

An Ada generic providing polynomial operations and functions for
polynomials with real (floating point) coefficients. Note that
polynomial division is not implemented, except for division by a scalar.


4. test/poly.adb

A unit test for the generic_polynomial.ad[bs] generic.  This program
reads in polynomial specifications from standard input and outputs the
results of certain operations.  The input is expected in three line groups:
   line 1: space separated coefficients for polynomial 1
   line 2: space separated coefficients for polynomial 2
   line 3: a single x-axis value evaluation point 

The test program also then tests some operations on some hard coded
polynomials.

$ gnatmake test/poly.adb -Isrc
gcc -c -Itest/ -Isrc -I- test/poly.adb
gcc -c -Itest/ -Isrc -I- /(path)/src/generic_polynomial.adb
gcc -c -Itest/ -Isrc -I- /(path)/src/jenkins_traub.adb
gcc -c -Itest/ -Isrc -I- /(path)/src/math.ads
gnatbind -Isrc -x poly.ali
gnatlink poly.ali


$ ./poly < /dev/null

$ ./poly
1 -2 1
1 -1
0
^D

$ ./poly < test/pjtest.in


5. src/jenkins_traub.ad[bs]

An Ada generic port of the netlib.org algorithm 493.f "RPOLY", originally
described in the paper:

  Jenkins, M. A., and Traub, J. F., "A Three-Stage Algorithm for Real
    Polynomials Using Quadratic Iteration", SIAM Journal of Numerical Analysis,
    Volume 7, Number 4, December 1970, pp 545-566

This port is intended to modularlize the original Fortran code when converting
to Ada, so that the coded algorithm is easier to follow than in the orignal
Fortran code.


6. test/jtest.adb, test/jtest.in

A unit test program, and test vector input file, for the jenkins_traub.ad[bs]
generic.  This program reads in polynomial specifications from standard input
and outputs the entered polynomial and the real and imaginary parts of the
polynomial roots.  The input is expected in one line groups:
   line 1: space separated coefficients for polynomial

$ gnatmake test/jtest.adb -Isrc/ -largs -lm
gcc -c -Itest/ -Isrc/ -I- test/jtest.adb
gcc -c -Itest/ -Isrc/ -I- /(path)/src/jenkins_traub.adb
gcc -c -Itest/ -Isrc/ -I- /(path)/src/math.ads
gnatbind -Isrc/ -x jtest.ali
gnatlink jtest.ali -lm

$ ./jtest
1 2 1
1 0 1
1 0 -1
1 3 3 1
1 -3 3 -1
^D

$ ./jtest < test/jtest.in


7. test/pjtest.adb, test/pjtest.in

A unit test for the generic_polynomial.ad[bs] generic, including
polynomial root finding using the jenkins_traub.ad[bs] generic, and a test
input vector file.  This program reads in polynomial specifications from
standard input and outputs the results of certain operations and polynomial
roots.  The input is expected in three line groups:
   line 1: space separated coefficients for polynomial 1
   line 2: space separated coefficients for polynomial 2
   line 3: a single x-axis value evaluation point 

The test program also then tests some operations on some hard coded
polynomials.

$ gnatmake test/pjtest -Isrc -largs -lm
gcc -c -Itest/ -Isrc/ -I- test/pjtest.adb
gcc -c -Itest/ -Isrc/ -I- /(path)/src/generic_polynomial.adb
gcc -c -Itest/ -Isrc/ -I- /(path)/src/jenkins_traub.adb
gcc -c -Itest/ -Isrc/ -I- /(path)/src/math.ads
gnatbind -Isrc/ -x pjtest.ali
gnatlink pjtest.ali -lm

$ ./pjtest < /dev/null

$ ./pjtest
1 2 1
1 0 1
0
^D

$ ./pjtest < test/pjtest.in
