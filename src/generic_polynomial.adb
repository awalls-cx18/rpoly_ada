with JENKINS_TRAUB;

--generic

--   type FLOAT_TYPE is digits <>;
--   type POLYNOMIAL_TYPE is array (NATURAL range <>) of FLOAT_TYPE;

package body GENERIC_POLYNOMIAL is

   type FLOAT_VECTOR_TYPE is array (POSITIVE range <>) of FLOAT_TYPE;

   package JT is new JENKINS_TRAUB (FLOAT_TYPE        => FLOAT_TYPE,
                                    FLOAT_VECTOR_TYPE => FLOAT_VECTOR_TYPE);

   MULTIPLICATION_ALGORITHM : MULTIPLICATION_ALGORITHM_TYPE := CONVOLUTION; 
   KARATSUBA_THRESHOLD : POSITIVE := 1000;

   --
   -- Scalar * Polynomial
   -- Scalar * Polynomial Slice
   --
   function "*" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      Q : POLYNOMIAL_TYPE (P'range);

   begin

      if A = 0.0 then
         Q := (others => 0.0);
         return Q;
      elsif A = 1.0 then
         return P;
      elsif A = -1.0 then
         return -P;
      end if;

      for I in P'range loop
         Q(I) := P(I) * A;
      end loop;

      return Q;

   end "*";

   --
   -- Polynomial * Scalar
   -- Polynomial Slice * Scalar
   --
   function "*" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE)
      return POLYNOMIAL_TYPE is
   begin
      return A * P;
   end "*";

   --
   -- Polynomial / Scalar
   -- Polynomial Slice / Scalar
   --
   function "/" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE)
      return POLYNOMIAL_TYPE is

      Q : POLYNOMIAL_TYPE (P'range);

   begin

      if A = 0.0 then
         raise NUMERIC_ERROR;
      elsif A = 1.0 then
         return P;
      elsif A = -1.0 then
         return -P;
      end if;

      for I in P'range loop
         Q(I) := P(I) / A;
      end loop;

      return Q;

   end "/";

   --
   -- +Polynomial
   -- +(Polynomial Slice)
   --
   function "+" (P : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is
   begin
      return P;
   end "+";

   --
   -- Scalar + Polynomial
   -- Scalar + Polynomial Slice
   --
   function "+" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      Q : POLYNOMIAL_TYPE (0..P'last);

   begin

      -- Scalar + Null Polynomial => return an order 0 polynomial
      if P'length = 0 then
         return POLYNOMIAL_TYPE'(0 => A);
      end if;

      -- Scalar + Polynomial => add scalar to 0th order term
      -- Scalar + Polynomial Slice =>
      --                        extend polynomial slice to proper polynomial 
      --                        that has a 0th order term, then add scalar to
      --                        0th order term
      Q(P'range) := P;
      Q(0..(P'first-1)) := (others => 0.0);
      Q(0) := Q(0) + A; 
      return Q;

   end "+";

   --
   -- Polynomial + Scalar
   -- Polynomial Slice + Scalar
   --
   function "+" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE)
      return POLYNOMIAL_TYPE is
   begin
      return A + P;
   end "+";

   --
   -- -Polynomial
   -- -(Polynomial Slice)
   --
   function "-" (P : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      Q : POLYNOMIAL_TYPE (P'range);

   begin

      for I in P'range loop
         Q(I) := -P(I);
      end loop;

      return Q;

   end "-";

   --
   -- Scalar - Polynomial
   -- Scalar - Polynomial Slice
   --
   function "-" (A : FLOAT_TYPE; P : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is
   begin
      return A + (-P);
   end "-";

   --
   -- Polynomial - Scalar
   -- Polynomial Slice - Scalar
   --
   function "-" (P : POLYNOMIAL_TYPE; A : FLOAT_TYPE)
      return POLYNOMIAL_TYPE is
   begin
      return (-A) + P;
   end "-";

   function MAX (A, B : NATURAL)
      return NATURAL is
   begin
      if A > B then
         return A;
      end if;
      return B; -- If =, then favor the second argument
   end MAX;

   function MIN (A, B : NATURAL)
      return NATURAL is
   begin
      if B < A then
         return B;
      end if;
      return A; -- If =, then favor the first argument
   end MIN;

   --
   -- Polynomial + Polynomial
   -- Polynomial + Polynomial Slice
   -- Polynomial Slice + Polynomial
   -- Polynomial Slice + Polynomial Slice
   --
   function "+" (P,Q : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      R : POLYNOMIAL_TYPE (MIN (P'first, Q'first)..MAX (P'last, Q'last));

   begin

      if P'length = 0 then
         return Q;
      elsif Q'length = 0 then
         return P;
      end if;

      -- Prefer to copy the polynomial with more coefficients, because
      -- we prefer more copies than additions

      if P'length > Q'length then

         -- Copy
         R(P'range) := P;

         -- Fill
         R(R'first    .. (P'first-1)) := (others => 0.0);
         R((P'last+1) .. R'last     ) := (others => 0.0);

         -- Add
         for I in Q'range loop
            R(I) := R(I) + Q(I);
         end loop;

      else

         -- Copy
         R(Q'range) := Q;

         -- Fill
         R(R'first    .. (Q'first-1)) := (others => 0.0);
         R((Q'last+1) .. R'last     ) := (others => 0.0);

         -- Add
         for I in P'range loop
            R(I) := R(I) + P(I);
         end loop;

      end if;

      return R;

   end "+";        

   --
   -- Polynomial - Polynomial
   -- Polynomial - Polynomial Slice
   -- Polynomial Slice - Polynomial
   -- Polynomial Slice - Polynomial Slice
   --
   function "-" (P,Q : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      R : POLYNOMIAL_TYPE (MIN (P'first, Q'first)..MAX (P'last, Q'last));

   begin

      if P'length = 0 then
         return -Q;
      elsif Q'length = 0 then
         return P;
      end if;

      -- Copy
      R(P'range) := P;

      -- Fill
      R(R'first    .. (P'first-1)) := (others => 0.0);
      R((P'last+1) .. R'last     ) := (others => 0.0);

      -- Subtract
      for I in Q'range loop
         R(I) := R(I) - Q(I);
      end loop;

      return R;

   end "-";        

   function CONVOLVE (P,Q : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      --        min (M, n-l)
      -- r[n] = sum             q[k] p[n-k]
      --        k = max (m, n-L)
      --
      -- q'range = m..M
      -- p'range = l..L
      -- r'range = m+l..M+L

      R : POLYNOMIAL_TYPE (Q'first+P'first..Q'last+P'last);

      function MIN (A,B : INTEGER)
         return INTEGER is
      begin
         if B < A then
            return B;
         end if;
         return A;
      end MIN;

      function MAX (A,B : INTEGER)
         return INTEGER is
      begin
         if A > B then
            return A;
         end if;
         return B;
      end MAX;

   begin

      for N in R'range loop
         R(N) := 0.0;
         for K in MAX (INTEGER(Q'first), INTEGER(N) - INTEGER(P'last) ) ..
                  MIN (INTEGER(Q'last) , INTEGER(N) - INTEGER(P'first))    loop
            R(N) := R(N) + Q(NATURAL(K)) * P(NATURAL(INTEGER(N) - K));
         end loop;
      end loop;

      return R;

   end CONVOLVE;

   function KARATSUBA_MULT (P,Q : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is

      K : NATURAL := MIN (P'length, Q'length) / 2;

      P0 : POLYNOMIAL_TYPE (0..K-1) := P(P'first..P'first+K-1);
      Q0 : POLYNOMIAL_TYPE (0..K-1) := Q(Q'first..Q'first+K-1);
      R0 : POLYNOMIAL_TYPE (0..2*(K-1));

      P1 : POLYNOMIAL_TYPE (0..P'length-K-1) := P(P'first+K..P'last);
      Q1 : POLYNOMIAL_TYPE (0..Q'length-K-1) := Q(Q'first+K..Q'last);
      R2 : POLYNOMIAL_TYPE (0..Q'length+P'length-2*(K+1));

      R1 : POLYNOMIAL_TYPE (0..Q'length+P'length-2*(K+1));

      R : POLYNOMIAL_TYPE (Q'first+P'first..Q'last+P'last);

   begin

      R0 := P0 * Q0;
      R2 := P1 * Q1;
   
      R1 := (P0 + P1) * (Q0 + Q1) - (R0 + R2);

      R := (R0 & FLOAT_TYPE(0.0) & R2) + (POLYNOMIAL_TYPE'(0..K-1 => 0.0) & R1);

      return R;

   end KARATSUBA_MULT;

   procedure SET_MULTIPLICATION_ALGORITHM
               (ALG       : in MULTIPLICATION_ALGORITHM_TYPE;
                THRESHOLD : in POSITIVE := 1000             ) is
   begin
      MULTIPLICATION_ALGORITHM := ALG; 
      KARATSUBA_THRESHOLD := THRESHOLD;
   end SET_MULTIPLICATION_ALGORITHM;

   function "*" (P,Q : POLYNOMIAL_TYPE)
      return POLYNOMIAL_TYPE is
   begin

      if P'length = 0 then

         return P;

      elsif Q'length = 0 then

         return Q;

      elsif P'length = 1 then

         declare
            R : POLYNOMIAL_TYPE (Q'first+P'first..Q'last+P'last);
         begin
            R := P(P'first) * Q;
            return R;
         end;

      elsif Q'length = 1 then

         declare
            R : POLYNOMIAL_TYPE (Q'first+P'first..Q'last+P'last);
         begin
            R := Q(Q'first) * P;
            return R;
         end;

      end if;

      case MULTIPLICATION_ALGORITHM is

         when CONVOLUTION =>
            return CONVOLVE(P,Q);

         when KARATSUBA =>
            return KARATSUBA_MULT(P,Q);

         when KARATSUBA_CONVOLUTION =>

            if (P'length-1)+(Q'length-1) > KARATSUBA_THRESHOLD then
               return KARATSUBA_MULT(P,Q);
            else
               return CONVOLVE(P,Q);
            end if;

      end case;

   end "*";


   function EVAL (P : POLYNOMIAL_TYPE; X : FLOAT_TYPE)
      return FLOAT_TYPE is

      Y : FLOAT_TYPE;

   begin -- EVAL

      if P'length = 0 then
         return 0.0;
      end if;

      Y := P(P'last);

      for I in reverse P'first..P'last-1 loop
         Y := Y * X + P(I);
      end loop;

      for I in reverse 0..P'first-1 loop
         Y := Y * X;
      end loop;

      return Y;

   end EVAL;

   procedure EVAL (P     : in     POLYNOMIAL_TYPE;
                   X     : in     FLOAT_TYPE;
                   Y     :    out FLOAT_TYPE;
                   DY_DX :    out FLOAT_TYPE) is

      U, U_PRIME : FLOAT_TYPE;

   begin -- EVAL

      if P'length = 0 then
         Y := 0.0;
         DY_DX := 0.0;
         return;
      end if;

      U       := 0.0;
      U_PRIME := 0.0;

      for I in reverse P'first+1..P'last loop
         U       := U       * X + P(I);
         U_PRIME := U_PRIME * X + U;
      end loop;

      for I in reverse 1..P'first loop
         U       := U       * X;
         U_PRIME := U_PRIME * X + U;
      end loop;

      if P'first = 0 then
         Y := U * X + P(P'first);
      else
         Y := U * X;
      end if;
      DY_DX := U_PRIME;

      return;

   end EVAL;


   procedure ROOTS (P    : in     POLYNOMIAL_TYPE;
                    N    :    out INTEGER;
                    REAL :    out POLYNOMIAL_TYPE;
                    IMAG :    out POLYNOMIAL_TYPE;
                    FAIL :    out BOOLEAN) is

      COEFFS    : FLOAT_VECTOR_TYPE (1..P'last+1);
      ZEROR     : FLOAT_VECTOR_TYPE (1..P'last);
      ZEROI     : FLOAT_VECTOR_TYPE (1..P'last);
      DEGREE    : NATURAL;
      HIPOW     : NATURAL;
      FAILED    : BOOLEAN;

   begin -- ROOTS

      FAILED := true;
      HIPOW := 0;
      for I in reverse P'range loop
         if P(I) /= 0.0 then
            HIPOW := I;
            FAILED := false;
            exit;
         end if;
      end loop;
      
      if FAILED = true then
         -- the ZERO polynomial has an infinite number of roots
         N := 0;
         REAL := (others => 0.0);
         IMAG := (others => 0.0);
         FAIL := true;
         return;
      end if;
      
      if HIPOW = 0 then
         -- a constant non-zero polynomial has no roots
         N := 0;
         REAL := (others => 0.0);
         IMAG := (others => 0.0);
         FAIL := false;
         return;
      end if;

      -- FIXME -Final DEGREE & HIPOW should match
      -- FIXME this needs inspection & test
      DEGREE := 0;
      for I in reverse P'first..HIPOW loop
         DEGREE := DEGREE + 1;
         COEFFS (DEGREE) := P(I);
      end loop;

      -- FIXME this needs inspection & test
      for I in reverse 0..P'first-1 loop
         DEGREE := DEGREE + 1;
         COEFFS (DEGREE) := 0.0;
      end loop;
      DEGREE := DEGREE - 1;

      JT.RPOLY (OP     => COEFFS,
                DEGREE => DEGREE,
                ZEROR  => ZEROR,
                ZEROI  => ZEROI,
                FAIL   => FAILED);

      FAIL := FAILED;
      N := DEGREE;
      REAL (0 .. DEGREE-1) := POLYNOMIAL_TYPE(ZEROR (1 .. DEGREE));
      IMAG (0 .. DEGREE-1) := POLYNOMIAL_TYPE(ZEROI (1 .. DEGREE));
      return;
   end ROOTS;

end GENERIC_POLYNOMIAL;
