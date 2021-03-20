-- Copyright (C) 2004-2021 Andy Walls <awalls.cx18@gmail.com>
--
-- This file is part of the rpoly_ada library.
--
-- The rpoly_ada library is free software: you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the License,
-- or (at your option) any later version.
--
-- The rpoly_ada library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
-- General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with the rpoly_ada library.  If not, see
-- <https://www.gnu.org/licenses/>.

generic

   type FLOAT_TYPE is digits <>;
   type POLYNOMIAL_TYPE is array (NATURAL range <>) of FLOAT_TYPE;

package GENERIC_POLYNOMIAL is

   type MULTIPLICATION_ALGORITHM_TYPE is 
      (CONVOLUTION, KARATSUBA, KARATSUBA_CONVOLUTION);

   ZERO_POLYNOMIAL : constant POLYNOMIAL_TYPE := (0 => 0.0);

   function "*" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;
   function "*" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE) return POLYNOMIAL_TYPE;

   function "/" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE) return POLYNOMIAL_TYPE;

   function "+" (P : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;
   function "+" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;
   function "+" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE) return POLYNOMIAL_TYPE;

   function "-" (P : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;
   function "-" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;
   function "-" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE) return POLYNOMIAL_TYPE;

   function "+" (P,Q : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;

   function "-" (P,Q : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;

   procedure SET_MULTIPLICATION_ALGORITHM
               (ALG       : in MULTIPLICATION_ALGORITHM_TYPE;
                THRESHOLD : in POSITIVE := 1000);

   function "*" (P,Q : POLYNOMIAL_TYPE) return POLYNOMIAL_TYPE;

   function EVAL (P : POLYNOMIAL_TYPE; X : FLOAT_TYPE) return FLOAT_TYPE;

   procedure EVAL (P     : in     POLYNOMIAL_TYPE;
                   X     : in     FLOAT_TYPE;
                   Y     :    out FLOAT_TYPE;
                   DY_DX :    out FLOAT_TYPE);

   procedure ROOTS (P    : in     POLYNOMIAL_TYPE;
                    N    :    out INTEGER;
                    REAL :    out POLYNOMIAL_TYPE;
                    IMAG :    out POLYNOMIAL_TYPE;
                    FAIL :    out BOOLEAN);

end GENERIC_POLYNOMIAL;
