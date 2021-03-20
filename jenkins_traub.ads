-- Jenkins, M. A., and Traub, J. F., "A Three-Stage Algorithm for Real
--   Polynomials Using Quadratic Iteration", SIAM Journal of Numerical Analysis,
--   Volume 7, Number 4, December 1970, pp 545-566

generic

   type FLOAT_TYPE is digits <>;
   type FLOAT_VECTOR_TYPE is array (POSITIVE range <>) of FLOAT_TYPE;

package JENKINS_TRAUB is

   -- Quadratic Equation - Numerically Safe
   procedure QUAD (A  : in     FLOAT_TYPE;
                   B1 : in     FLOAT_TYPE;
                   C  : in     FLOAT_TYPE;
                   SR :    out FLOAT_TYPE;
                   SI :    out FLOAT_TYPE;
                   LR :    out FLOAT_TYPE;
                   LI :    out FLOAT_TYPE);

   -- Quadratic synthetic division
   -- Divide z^2 + U z + V into P
   -- Q must be same length as P (not 2 coefficients shorter than P)
   -- The next to the last element of Q holds a copy of B, the 1/z term and
   -- the last element of Q holds a copy of A, the 1/z^2 term.
   -- The standard polynomial remainder, R = B (z + U) + A
   procedure QUADSD (U  : in     FLOAT_TYPE;
                     V  : in     FLOAT_TYPE;
                     P  : in     FLOAT_VECTOR_TYPE;
                     Q  :    out FLOAT_VECTOR_TYPE;
                     A  :    out FLOAT_TYPE;
                     B  :    out FLOAT_TYPE);

   -- Polynomial Zeros
   procedure RPOLY (OP     : in     FLOAT_VECTOR_TYPE;
                    DEGREE : in out INTEGER;
                    ZEROR  :    out FLOAT_VECTOR_TYPE;
                    ZEROI  :    out FLOAT_VECTOR_TYPE;
                    FAIL   :    out BOOLEAN);

end JENKINS_TRAUB;
