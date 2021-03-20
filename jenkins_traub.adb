with MATH;

package body JENKINS_TRAUB is

   type SHORT_FLOAT_TYPE is new SHORT_FLOAT;
   type SHORT_FLOAT_VECTOR_TYPE is array (POSITIVE range <>) of SHORT_FLOAT_TYPE;

   -- THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED
   -- IN VARIOUS PARTS OF THE PROGRAM. THE MEANING OF THE
   -- FOUR CONSTANTS ARE...
   --    ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR
   --            WHICH CAN BE DESCRIBED AS THE SMALLEST
   --            POSITIVE FLOATING POINT NUMBER SUCH THAT
   --            1.D0+ETA IS GREATER THAN 1.
   ETA : constant SHORT_FLOAT_TYPE := FLOAT_TYPE'epsilon;

   --   INFINY  THE LARGEST FLOATING-POINT NUMBER.
   --   SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER
   --       IF THE EXPONENT RANGE DIFFERS IN SINGLE AND
   --       DOUBLE PRECISION THEN SMALNO AND INFIN
   --       SHOULD INDICATE THE SMALLER RANGE.
   INFIN  : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE'last;
   --SMALNO : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE'safe_small;

   --   BASE    THE BASE OF THE FLOATING-POINT NUMBER SYSTEM USED.
   --BASE : constant POSITIVE := FLOAT_TYPE'machine_radix;

   -- THE VALUES BELOW CORRESPOND TO THE BURROUGHS B6700
   --   BASE = 8.
   --   ETA = .5*BASE**(1-26)
   --   INFIN = 4.3E68
   --   SMALNO = 1.0E-45

   -- ARE AND MRE REFER TO THE UNIT ERROR IN + AND *
   -- RESPECTIVELY. THEY ARE ASSUMED TO BE THE SAME AS ETA.
   ARE : constant SHORT_FLOAT_TYPE := ETA;
   MRE : constant SHORT_FLOAT_TYPE := ETA;
   --LO  : constant SHORT_FLOAT_TYPE := SMALNO/ETA;

   procedure RPOLY (OP     : in     FLOAT_VECTOR_TYPE;
                    DEGREE : in out INTEGER;
                    ZEROR  :    out FLOAT_VECTOR_TYPE;
                    ZEROI  :    out FLOAT_VECTOR_TYPE;
                    FAIL   :    out BOOLEAN)
   is

      -- INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION
      -- unit vector at -45 degrees
      XX : SHORT_FLOAT_TYPE := 1.0/MATH.SQRT_TWO;
      YY : SHORT_FLOAT_TYPE := -XX;

      P  : FLOAT_VECTOR_TYPE (OP'range); -- P polynomial
      QP : FLOAT_VECTOR_TYPE (OP'range); -- P quotient poly

      K  : FLOAT_VECTOR_TYPE (1..OP'last-1); -- K polynomial

      NZ : INTEGER; -- number of zeros found in a pass: 0, 1, or 2
      ZR : FLOAT_VECTOR_TYPE (1..2); -- Real part of zeros; small & large
      ZI : FLOAT_VECTOR_TYPE (1..2); -- Imaginary part of zeros; small & large

      J, N, NN : INTEGER; -- n := degree, nn := count of coefficients

      BND : SHORT_FLOAT_TYPE; -- lower bound on the moduli of zeros of P

      -- If a result could be obtained by catastrophic cancellation,
      -- check to see if the RESULT should/could have been 0, based
      -- on it's relationship to one of the terms.
      function ZERO_UNDER_CANCELLATION (RESULT : in FLOAT_TYPE;
                                        TERM   : in FLOAT_TYPE;
                                        FACTOR : in FLOAT_TYPE := 10.0)
         return BOOLEAN is
      begin
         return abs (RESULT) <= (abs (TERM) * FACTOR * FLOAT_TYPE (ETA));
      end ZERO_UNDER_CANCELLATION;
      pragma INLINE (ZERO_UNDER_CANCELLATION);

      -- Rotate a vector (x,y) by 94 degrees
      procedure ROTATE_94 (X       : in out SHORT_FLOAT_TYPE;
                           Y       : in out SHORT_FLOAT_TYPE) is

         ROT  : constant := 94.0/180.0 * MATH.PI;
         COSR : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE (MATH.COSF (ROT));
         SINR : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE (MATH.SINF (ROT));

         TMP : SHORT_FLOAT_TYPE;

      begin -- ROTATE_94
            TMP := COSR*X - SINR*Y;
            Y   := SINR*X + COSR*Y;
            X   := TMP;
      end ROTATE_94;

      procedure SCALE_AND_FIND_BOUND (P     : in out FLOAT_VECTOR_TYPE;
                                      BOUND :    out SHORT_FLOAT_TYPE) is

         PT       : SHORT_FLOAT_VECTOR_TYPE (1..P'last);
         MIN, MAX : SHORT_FLOAT_TYPE;

         function "/" (A, B : MATH.FLOAT) return MATH.FLOAT renames MATH."/";

         --FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.
         -- and store the moduli of all coefficients
         procedure FIND_MIN_MAX_ABS_COEFFS
         is
         begin -- FIND_MIN_MAX_ABS_COEFFS

            MAX := 0.0;
            MIN := INFIN;

            for I in 1..P'last loop

               PT(I) := abs(SHORT_FLOAT_TYPE(P(I)));

               if PT(I) > MAX then
                  MAX := PT(I);
               end if;

               if PT(I) /= 0.0 and PT(I) < MIN then
                  MIN := PT(I);
               end if;

            end loop;

         end FIND_MIN_MAX_ABS_COEFFS;

         -- SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS
         -- COMPUTES A SCALE FACTOR TO MULTIPLY THE
         -- COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE
         -- TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW
         -- INTERFERING WITH THE CONVERGENCE CRITERION.
         -- THE FACTOR IS A POWER OF THE BASE
         procedure SCALE
         is

            --   INFINY  THE LARGEST FLOATING-POINT NUMBER.
            --   SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER
            --       IF THE EXPONENT RANGE DIFFERS IN SINGLE AND
            --       DOUBLE PRECISION THEN SMALNO AND INFIN
            --       SHOULD INDICATE THE SMALLER RANGE.
            --INFIN  : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE'last;
            SMALNO : constant SHORT_FLOAT_TYPE := SHORT_FLOAT_TYPE'safe_small;
            LO     : constant SHORT_FLOAT_TYPE := SMALNO/ETA;

            --   BASE    THE BASE OF THE FLOATING-POINT NUMBER SYSTEM USED.
            BASE   : constant POSITIVE := FLOAT_TYPE'machine_radix;

            SC     : SHORT_FLOAT_TYPE;
            FACTOR : FLOAT_TYPE;
            L      : INTEGER;

         begin -- SCALE

            SC := LO/MIN;

            if SC <= 1.0 and MAX < 10.0 then
               return;
            elsif SC = 0.0 then
               SC := SMALNO;
            elsif (INFIN/SC) < MAX then
               return;
            end if;

            -- Ada83 RM 4.6: explicit cast rounds properly
            L := INTEGER(MATH.LOGF(MATH.FLOAT(SC))/MATH.LOGF(MATH.FLOAT(BASE)));
            FACTOR := (FLOAT_TYPE(BASE))**L;

            if FACTOR /= 1.0 then
               for I in 1..P'last loop
                  P(I) := FACTOR*P(I);
               end loop;
            end if;

         end SCALE;


         -- COMPUTE LOWER BOUND ON MODULI OF ZEROS.
         function LOWER_BOUND_OF_ZERO_MODULI
            return SHORT_FLOAT_TYPE is

            X, XM : SHORT_FLOAT_TYPE;
            FF, DX, DF : SHORT_FLOAT_TYPE;

            function "-" (A, B : MATH.FLOAT) return MATH.FLOAT renames MATH."-";

            -- Evaluate POLY at X using Horner's rule
            function HORNER (POLY : in SHORT_FLOAT_VECTOR_TYPE;
                             X    : in SHORT_FLOAT_TYPE)
               return SHORT_FLOAT_TYPE is

               VALUE : SHORT_FLOAT_TYPE;

            begin

               VALUE := POLY (1);
               for I in 2..POLY'last loop
                  VALUE := VALUE * X + POLY (I);
               end loop;

               return VALUE;
         
            end HORNER;
            pragma INLINE (HORNER);
               
            -- Evaluate P(x) and d(P(x))/dx at X by Horner's rule
            procedure HORNER_DERIVATIVE (POLY : in     SHORT_FLOAT_VECTOR_TYPE;
                                         X    : in     SHORT_FLOAT_TYPE;
                                         F    :    out SHORT_FLOAT_TYPE;
                                         DFDX :    out SHORT_FLOAT_TYPE) is

               FF, DF : SHORT_FLOAT_TYPE;

            begin
            
               FF := POLY (1);
               DF := FF;
               for I in 2..POLY'last-1 loop
                  FF := FF * X + POLY (I);
                  DF := DF * X + FF;
               end loop;
               FF := FF * X + POLY (POLY'last);

               F    := FF;
               DFDX := DF;

            end HORNER_DERIVATIVE;
            pragma INLINE (HORNER_DERIVATIVE);

         begin -- LOWER_BOUND_OF_ZERO_MODULI

            -- Solve for the unique positive zero of
            -- |a0|x^n + |a1|x^(n-1) + ... |a(n-1)|x - |an| = 0
            -- by Newton-Rhapson iteration to find a lower bound on the
            -- moduli of zeros
            -- page 562

            PT(PT'last) := -PT(PT'last);

            -- COMPUTE UPPER ESTIMATE OF BOUND

            -- Try the simple solution to |a0|x^n - |an| = 0 as an upper bound

            -- x := (abs (p(nn)/p(1))) ^ (1/n) -- n-th root
            -- is the nth root of |an/a0|
            X := SHORT_FLOAT_TYPE (
                               MATH.EXPF(
                                         (  MATH.LOGF(MATH.FLOAT(-PT(PT'last))) 
                                          - MATH.LOGF(MATH.FLOAT( PT(      1)))
                                         )
                                         / MATH.FLOAT(PT'last-1)
                                        )
                              );

            -- Also check the Newton-Rhapson step factor at the origin
            if PT(PT'last-1) /= 0.0 then
               XM := -PT(PT'last)/PT(PT'last-1);
               -- xm := abs (p(nn)/p(n)) -- newton step factor at x = 0
               -- is |an/a(n-1)|

               -- IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.
               -- choose the smaller upper bound
               if XM < X then
                  X := XM;
               end if;
            end if;
   
            --CHOP THE INTERVAL (0,X) UNTIL FF .LE. 0
            loop
               -- Do a 10/90 (vs 50/50) binary search for the transition
               -- from positive to negative 
               XM := X * 0.1;

               -- Evaluate PT at XM using Horner's rule
               FF := HORNER (POLY => PT, X => XM); 
               
               -- terminate when we know the zero crossing is between [.1x0, x0]
               exit when FF <= 0.0;

               X := XM;
            end loop;

            DX := X;
            -- DO NEWTON ITERATION UNTIL X CONVERGES TO TWO
            -- DECIMAL PLACES
            while abs(DX/X) > 0.005 loop

               -- Evaluate PT(x) and d(PT(x))/dx at X by Horner's rule
               HORNER_DERIVATIVE (POLY => PT, X => X, F => FF, DFDX => DF);

               -- Newton-Rhaphson step
               DX := FF/DF;
               X := X - DX;
            end loop;

            return X;

         end LOWER_BOUND_OF_ZERO_MODULI;

      begin -- SCALE_AND_FIND_BOUND

         --FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.
         -- and store the moduli of all coefficients
         FIND_MIN_MAX_ABS_COEFFS;

         -- SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS
         -- COMPUTES A SCALE FACTOR TO MULTIPLY THE
         -- COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE
         -- TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW
         -- INTERFERING WITH THE CONVERGENCE CRITERION.
         -- THE FACTOR IS A POWER OF THE BASE
         SCALE;

         -- COMPUTE LOWER BOUND ON MODULI OF ZEROS.
         BOUND := LOWER_BOUND_OF_ZERO_MODULI;

      end SCALE_AND_FIND_BOUND;

      -- COMPUTE THE DERIVATIVE polynomial
      -- scaled by the 1/N, the degree of the polynomial
      function SCALED_DERIVATIVE (P : FLOAT_VECTOR_TYPE)
         return FLOAT_VECTOR_TYPE is

         D : FLOAT_VECTOR_TYPE (1 .. P'last-1);
         F : FLOAT_TYPE := FLOAT_TYPE (P'last-1);

      begin -- SCALED_DERIVATIVE

         D(1) := P(1);
         for I in D'first+1..D'last loop
            D(I) := FLOAT_TYPE(P'last-I)*P(I)/F;
         end loop;

         return D;

      end SCALED_DERIVATIVE;
      pragma INLINE (SCALED_DERIVATIVE);

      -- Given THE DERIVATIVE scaled by 1/N AS THE INTIAL K POLYNOMIAL
      -- DO Single STEPS WITH NO SHIFT
      -- page 556,
      -- to accentuate the small zeros
      -- Page 563, eqn 9.5
      procedure SINGLE_NOSHIFT_SEQ (P : in     FLOAT_VECTOR_TYPE;
                                    K : in out FLOAT_VECTOR_TYPE;
                                    M : in     POSITIVE)
      is

         ZEROK : BOOLEAN;
         AA, BB, T : FLOAT_TYPE;

      begin -- SINGLE_NOSHIFT_SEQ

         AA := P(P'last);   -- page 556, Stage 1, P(z=0)

         BB := P(P'last-1); -- page 556, Stage 1, P linear coeff

         ZEROK := K(K'last) = 0.0; -- K0(z=0) = 0.0 test

         for JJ in 1..M loop -- page 561, M = 5

            --page 556, Stage 1

            if (not ZEROK) then	
               -- USE SCALED FORM OF RECURRENCE IF VALUE OF K AT 0 IS NONZERO
               -- Knext = (-P(z=0)/Kprev(z=0) * Kprev + P)/z  instead of
               -- Knext = (Kprev - Kprev(z=0)/P(z=0) * P)/z
               T := -AA/K(K'last); 
               for J in reverse 2..K'last loop
                  -- since K is 1 degree less than P, division by Z is handled
                  -- by assigning Knext(J) := Kprev(J-1) to divide the K poly,
                  -- and by assigning Knext(J) := P(J) to divide the P polynmial
                  K(J) := T*K(J-1) + P(J);
               end loop;
               K(1) := P(1);

               -- When evaluating Knext(z=0) => -an*bn-2/bn-1 + an-1,
               -- where P(z) = a0*z^n     + ... + an-1 * z + an        and
               --   Kprev(z) = b0*z^(n-1) + ... + bn-2 * z + bn-1
               -- catastrophic cancellation is declared a "0", when the end
               -- result is <= 10*epsilon
               ZEROK := ZERO_UNDER_CANCELLATION (RESULT => K (K'last),
                                                 TERM   => BB         );
            else
               -- USE UNSCALED FORM OF RECURRENCE
               -- that has a trivial form since Kprev(z=0)/P(z=0) = 0
               -- so Knext = (Kprev + 0 * P)/z = Kprev/z
               for J in reverse 2..K'last loop
                  -- since K is 1 degree less than P, division by Z is handled
                  -- by assigning Knext(J) := Kprev(J-1) to divide the K poly.
                  K(J) := K(J-1);
               end loop;
               K(1) := 0.0; -- since Kprev(z=0)/P(z=0) * a0 = 0
               ZEROK := K(K'last) = 0.0;
            end if;

         end loop;
      end SINGLE_NOSHIFT_SEQ;

      -- COMPUTES UP TO  L2  FIXED SHIFT K-POLYNOMIALS,
      -- TESTING FOR CONVERGENCE IN THE LINEAR OR QUADRATIC
      -- CASE. INITIATES ONE OF THE VARIABLE SHIFT
      -- ITERATIONS AND RETURNS WITH THE NUMBER OF ZEROS
      -- FOUND.
      -- L2 - LIMIT OF FIXED SHIFT STEPS
      -- NZ - NUMBER OF ZEROS FOUND
      procedure FXSHFR (L2      : in     INTEGER;
                        P       : in     FLOAT_VECTOR_TYPE;
                        SR_INIT : in     SHORT_FLOAT_TYPE;
                        U_INIT  : in     FLOAT_TYPE;
                        V_INIT  : in     FLOAT_TYPE;
                        K       : in     FLOAT_VECTOR_TYPE;
                        QP      :    out FLOAT_VECTOR_TYPE;
                        ZEROR   :    out FLOAT_VECTOR_TYPE;
                        ZEROI   :    out FLOAT_VECTOR_TYPE;
                        NZ      :    out INTEGER)  is

         QUOTP : FLOAT_VECTOR_TYPE (P'range);      -- P quotient poly

         KPOLY : FLOAT_VECTOR_TYPE (K'range) := K; -- K poly
         QUOTK : FLOAT_VECTOR_TYPE (K'range);      -- K quotient poly
         SZR : FLOAT_TYPE; -- real zero

         A, B, C, D, F, G, H : FLOAT_TYPE; 
         A1, A3, A7 : FLOAT_TYPE; 
   
         U, V  : FLOAT_TYPE;
         UI, VI, S : FLOAT_TYPE;
         BETAS, BETAV, OSS, OVV, SS, VV : SHORT_FLOAT_TYPE;
         TS, TV, OTS, OTV, TVV, TSS : SHORT_FLOAT_TYPE;
         NORM_TYPE, NZ1 : INTEGER;
         IFLAG, VPASS, SPASS, VTRY, STRY : BOOLEAN;

         DONE_Q_L : INTEGER;

         -- THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO
         -- COMPUTE THE NEXT K POLYNOMIAL AND NEW ESTIMATES OF
         -- THE QUADRATIC COEFFICIENTS.
         -- TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE
         -- CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW
         procedure CALCSC (A, B, C, D : in     FLOAT_TYPE;
                           U, V       : in     FLOAT_TYPE;
                           K          : in     FLOAT_VECTOR_TYPE;
                           A1, A3, A7 :    out FLOAT_TYPE;
                           F, G, H    :    out FLOAT_TYPE;
                           NORM_TYPE  :    out INTEGER) is

            E, F1, G1, H1 : FLOAT_TYPE; 
   
         begin -- calcsc
   
            if ZERO_UNDER_CANCELLATION (RESULT => C,
                                        TERM   => K (K'last),
                                        FACTOR => 100.0        ) and then
               ZERO_UNDER_CANCELLATION (RESULT => D,
                                        TERM   => K (K'last-1),
                                        FACTOR => 100.0        )       then
   
               NORM_TYPE := 3;
               -- TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR
               -- OF K
               A1 := 0.0; A3 := 0.0; A7 := 0.0;
               F := 0.0; G:= 0.0; H := 0.0;
               return;
   
            elsif abs(D) >= abs(C) then
   
               NORM_TYPE := 2;
               -- TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED
               -- BY D
               E := A/D;
               F1 := C/D;
               G1 := U*B;
               H1 := V*B;
               A3 := (A+G1)*E + H1*(B/D); -- pg 563, 9.8, numQK/D
               A1 := B*F1 - A;            -- pg 563, 9.8, denom/D
               A7 := (F1+U)*A + H1;       -- pg 563, 9.8, numQP/D
   
            else
               NORM_TYPE := 1;
               -- TYPE=1 INDICATES THAT ALL FORMULAS ARE DIVIDED
               -- BY C
               E := A/C;
               F1 := D/C;
               G1 := U*E;
               H1 := V*B;
               A3 := A*E + (H1/C+G1)*B;   -- pg 563, 9.8, numQK/C
               A1 := B - A*(D/C);         -- pg 563, 9.8, denom/C
               A7 := A + G1*D + H1*F1;    -- pg 563, 9.8, numQP/C
            end if;
   
            F := F1;
            G := G1;
            H := H1;

         end CALCSC;

         -- COMPUTES THE NEXT K POLYNOMIALS USING SCALARS
         -- COMPUTED IN CALCSC
         procedure NEXTK (NORM_TYPE        : in     INTEGER;
                          A, B, A1, A3, A7 : in     FLOAT_TYPE;
                          QP, QK           : in     FLOAT_VECTOR_TYPE;
                          K                : in out FLOAT_VECTOR_TYPE) is

            TEMP, A7L, A3L : FLOAT_TYPE;

         begin -- nextk

            if NORM_TYPE /= 3 then
               if NORM_TYPE = 1 then
                  TEMP := B;
               else
                  TEMP := A;
               end if;

               if ZERO_UNDER_CANCELLATION (RESULT => A1, TERM => TEMP) then
                  -- IF A1 IS NEARLY ZERO THEN USE A SPECIAL
                  -- FORM OF THE RECURRENCE
                  K(1) := 0.0;
                  K(2) := -A7*QP(1);
                  for I in 3..K'last loop
                     K(I) := A3*QK(I-2) - A7*QP(I-1);
                  end loop;
               else
                  -- USE SCALED FORM OF THE RECURRENCE
                  A7L := A7/A1;
                  A3L := A3/A1;
                  K(1) := QP(1);
                  K(2) := QP(2) - A7L*QP(1);
                  for I in 3..K'last loop
                     K(I) := A3L*QK(I-2) - A7L*QP(I-1) + QP(I);
                  end loop;
               end if;
            else
               -- USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3
               K(1..2) := (1..2 => 0.0);
               K(3..K'last) := QK(1..K'last-2);
            end if;
         end NEXTK;
   
         -- COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS
         -- USING THE SCALARS COMPUTED IN CALCSC.
         procedure NEWEST (NORM_TYPE : in     INTEGER;
                           P         : in     FLOAT_VECTOR_TYPE;
                           K         : in     FLOAT_VECTOR_TYPE;
                           U         : in     FLOAT_TYPE;
                           V         : in     FLOAT_TYPE;
                           A, B, C, D, F, G, H : in FLOAT_TYPE;
                           A1        : in     FLOAT_TYPE;
                           A3        : in     FLOAT_TYPE;
                           A7        : in     FLOAT_TYPE;
                           UU        :    out FLOAT_TYPE; 
                           VV        :    out FLOAT_TYPE) is

            A4, A5, B1, B2, C1, C2, C3, C4, TEMP : FLOAT_TYPE;

         begin -- newest
            -- USE FORMULAS APPROPRIATE TO SETTING OF TYPE.
            case NORM_TYPE is
               when 3 =>
                  -- IF TYPE=3 THE QUADRATIC IS ZEROED
                  UU := 0.0;
                  VV := 0.0;
                  return;
               when 2 =>
                  A4 := (A+G)*F + H;
                  A5 := (F+U)*C + V*D;
               when others => -- 1
                  A4 := A + U*B + H*F;
                  A5 := C + (U+V*F)*D;
            end case;
   
            -- EVALUATE NEW QUADRATIC COEFFICIENTS.
            B1 := -K(K'last) / P(P'last);
            B2 := -(K(K'last-1) + B1*P(P'last-1)) / P(P'last);
            C1 := V*B2*A1;
            C2 := B1*A7;
            C3 := B1*B1*A3;
            C4 := C1 - C2 - C3;
            TEMP := A5 + B1*A4 - C4;
   
            if TEMP = 0.0 then
               UU := 0.0;
               VV := 0.0;
            else
               UU := U - (U*(C3 + C2) + V*(B1*A1 + B2*A7))/TEMP;
               VV := V*(1.0 + C4/TEMP);
            end if;
         end NEWEST;


         -- VARIABLE-SHIFT H POLYNOMIAL ITERATION FOR A REAL
         -- ZERO.
         -- SSS   - STARTING ITERATE
         -- NZ    - NUMBER OF ZERO FOUND
         -- IFLAG - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL
         --         AXIS.
         procedure REALIT (P     : in     FLOAT_VECTOR_TYPE;
                           SSS   : in     FLOAT_TYPE;
                           K     : in out FLOAT_VECTOR_TYPE;
                           QP    :    out FLOAT_VECTOR_TYPE;
                           SZR   :    out FLOAT_TYPE;
                           NZ    :    out INTEGER;
                           IFLAG :    out BOOLEAN) is

            QUOTP  : FLOAT_VECTOR_TYPE (P'range);

            KPOLY  : FLOAT_VECTOR_TYPE (K'range)   := K;
            QUOTK  : FLOAT_VECTOR_TYPE (K'range);

            J                : INTEGER;
            MS, MP, OMP, EE  : SHORT_FLOAT_TYPE;
            PV, KV, T, S     : FLOAT_TYPE;
   
            -- Evaluate P(x) at X using Horner's rule
            function HORNER (POLY : in FLOAT_VECTOR_TYPE;
                             X    : in FLOAT_TYPE)
               return FLOAT_TYPE is

               VALUE : FLOAT_TYPE;

            begin

               VALUE := POLY (1);
               for I in 2..POLY'last loop
                  VALUE := VALUE * X + POLY (I);
               end loop;

               return VALUE;
         
            end HORNER;
            pragma INLINE (HORNER);

            -- Evaluate P(z) at X, and also compute P(z)/(z-X) out to
            -- the 1/z term
            procedure HORNER_QUOTIENT (POLY : in     FLOAT_VECTOR_TYPE;
                                       X    : in     FLOAT_TYPE;
                                       F    :    out FLOAT_TYPE;
                                       QUOT :    out FLOAT_VECTOR_TYPE) is

               VALUE : FLOAT_TYPE;

            begin

               VALUE := POLY (1);
               QUOT (1) := VALUE;
               for I in 2..POLY'last loop 
                  VALUE := VALUE * X + POLY (I);
                  QUOT (I) := VALUE;
               end loop;

               F := VALUE;

            end HORNER_QUOTIENT;
            pragma INLINE (HORNER_QUOTIENT);

         begin -- realit
   
            S     := SSS;
            J     := 0;
   
            OMP := INFIN;
            T   := 0.0;
   
            -- MAIN LOOP
            loop

               -- EVALUATE P AT S
               HORNER_QUOTIENT (POLY => P, X => S, QUOT => QUOTP, F => PV);
   
               MP := SHORT_FLOAT_TYPE(abs(PV));
               -- COMPUTE A RIGOROUS BOUND ON THE ERROR IN 
               -- EVALUATING P
               -- FIXME Make LINEAR_ERROR_ESTIMATE function, including
               --     expression after MP <=(20* ....
               MS := SHORT_FLOAT_TYPE(abs(S));
               EE := (MRE/(ARE+MRE))*abs(SHORT_FLOAT_TYPE(QUOTP(1)));
               for I in 2..QUOTP'last loop 
                  EE := EE*MS + abs(SHORT_FLOAT_TYPE(QUOTP(I)));
               end loop;
   
               -- ITERATION HAS CONVERGED SUFFICIENTLY IF THE
               -- POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND
               if MP <= (20.0*((ARE+MRE)*EE-MRE*MP)) then
                  QP    := QUOTP;
                  SZR   := S;
                  NZ    := 1;
                  IFLAG := FALSE;
                  exit;
               end if;
   
               J := J + 1;
               if J > 10 then
                  NZ    := 0;
                  IFLAG := FALSE;
                  exit;
               end if;
   
               if J > 1 and then
                  MP > OMP and then 
                  abs(T) <= 0.001*abs(S-T) then
                  -- A CLUSTER OF ZEROS NEAR THE REAL AXIS 
                  -- HAS BEEN ENCOUNTERED RETURN WITH IFLAG
                  -- SET TO INITIATE A QUADRATIC ITERATION
                  K     := KPOLY;
                  SZR   := S;
                  NZ    := 0;
                  IFLAG := true;
                  exit;
                  -- RETURN IF THE POLYNOMIAL VALUE HAS
                  -- INCREASED SIGNIFICANTLY
               end if;
   
               OMP := MP;
               -- COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW
               -- ITERATE
               HORNER_QUOTIENT (POLY => KPOLY, X => S, QUOT => QUOTK, F => KV);

               -- NEXTK equivalent
               if not ZERO_UNDER_CANCELLATION (RESULT => KV, 
                                               TERM   => KPOLY(KPOLY'last)) then
                  -- USE THE SCALED FORM OF THE RECURRENCE IF
                  -- THE VALUE OF K AT S IS NONZERO
                  T := -PV/KV;
                  KPOLY(1) := QUOTP(1);
                  for I in 2..KPOLY'last loop
                     KPOLY(I) := T*QUOTK(I-1) + QUOTP(I);
                  end loop;
               else
                  -- USE UNSCALED FORM
                  KPOLY(1) := 0.0;
                  KPOLY(2..KPOLY'last) := QUOTK(1..KPOLY'last-1);
               end if;

               KV := HORNER (POLY => KPOLY, X => S);

               if not ZERO_UNDER_CANCELLATION (RESULT => KV, 
                                               TERM   => KPOLY(KPOLY'last)) then
                  T := -PV/KV;
               else
                  T := 0.0;
               end if;

               S := S + T;
            end loop;
         end REALIT;

         -- VARIABLE-SHIFT K-POLYNOMIAL ITERATION FOR A
         -- QUADRATIC FACTOR CONVERGES ONLY IF THE ZEROS ARE
         -- EQUIMODULAR OR NEARLY SO.
         -- UU,VV - COEFFICIENTS OF STARTING QUADRATIC
         -- NZ - NUMBER OF ZERO FOUND
         procedure QUADIT (P     : in     FLOAT_VECTOR_TYPE;
                           UU    : in     FLOAT_TYPE;
                           VV    : in     FLOAT_TYPE;
                           K     : in     FLOAT_VECTOR_TYPE;
                           QP    :    out FLOAT_VECTOR_TYPE;
                           ZEROR :    out FLOAT_VECTOR_TYPE;
                           ZEROI :    out FLOAT_VECTOR_TYPE;
                           NZ    :    out INTEGER)    is
   
            QUOTP : FLOAT_VECTOR_TYPE (P'range);

            KPOLY : FLOAT_VECTOR_TYPE (K'range) := K; 
            QUOTK : FLOAT_VECTOR_TYPE (K'range);
            SZR, SZI, LZR, LZI : FLOAT_TYPE; 

            A, B, C, D, F, G, H : FLOAT_TYPE; 
            A1, A3, A7 : FLOAT_TYPE; 
   
            U, V  : FLOAT_TYPE;
            UI, VI : FLOAT_TYPE;
            MP, OMP, EE, RELSTP, T, ZM : SHORT_FLOAT_TYPE;
            NORM_TYPE, J : INTEGER;
            TRIED : BOOLEAN;
   
         begin -- quadit
            NZ := 0;
            TRIED := false;
            U := UU;
            V := VV;
            J := 0;
            RELSTP := 0.9;
            OMP := INFIN;
   
            -- MAIN LOOP
            loop
               QUAD(1.0, U, V, SZR, SZI, LZR, LZI);
               -- RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT
               -- CLOSE TO MULTIPLE OR NEARLY EQUAL AND OF OPPOSITE
               -- SIGN
               exit when abs(abs(SZR) - abs(LZR)) > 0.01*abs(LZR);
   
               -- EVALUATE POLYNOMIAL BY QUADRATIC SYNTHETIC 
               -- DIVISION
               QUADSD(U, V, P, QUOTP, A, B);
   
               -- R(S) = B (S + U) + A = B (SZR +iSZI - 2 SZR) + A = 
               --                                               A-SRZ*B+iSZI
               -- P(S) = R(S) => |P(S)| = |R(S)| ~~> |Re(R(S))| + |Im(R(S))| 
               --    for small |P(S)|
               MP := SHORT_FLOAT_TYPE(abs(A-SZR*B) + abs(SZI*B));
               -- COMPUTE A RIGOROUS BOUND ON THE ROUNDING ERROR IN
               -- EVALUTING P
               -- FIXME Make QUADRATIC_ERROR_ESTIMATE function, including
               --     expression after MP <=(20* ....
               ZM := SHORT_FLOAT_TYPE(MATH.SQRTF(MATH."abs"(MATH.FLOAT(V))));
               EE := 2.0*abs(SHORT_FLOAT_TYPE(QUOTP(1)));
               T := SHORT_FLOAT_TYPE(-SZR*B);
               for I in 2..QUOTP'last-1 loop
                  EE := EE*ZM + abs(SHORT_FLOAT_TYPE(QUOTP(I)));
               end loop;
               EE := EE*ZM + abs(SHORT_FLOAT_TYPE(A)+T);
               EE := (5.0*MRE+4.0*ARE)*EE - (5.0*MRE+2.0*ARE)*
                     (abs(SHORT_FLOAT_TYPE(A)+T)+abs(SHORT_FLOAT_TYPE(B))*ZM) +
                     2.0*ARE*abs(T);
   
               -- ITERATION HAS CONVERGED SUFFICIENTLY IF THE
               -- POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND
               if MP <= 20.0*EE then
                  QP := QUOTP;
                  ZEROR (1..2) := (1 => SZR, 2 => LZR);
                  ZEROI (1..2) := (1 => SZI, 2 => LZI);
                  NZ := 2;
                  exit;
               end if;
   
               J := J + 1;
               exit when J > 20;
   
               if J      >  1     and then
                  RELSTP <= 0.01  and then
                  MP     >= OMP   and then 
                  TRIED  =  false then
   
                  -- A CLUSTER APPEARS TO BE STALLING THE
                  -- CONVERGENCE.  FIVE FIXED SHIFT STEPS ARE
                  -- TAKEN WITH A U,V CLOSE TO THE CLUSTER
                  if RELSTP < ETA then
                     RELSTP := ETA;
                  else
                     RELSTP := SHORT_FLOAT_TYPE(MATH.SQRTF(MATH.FLOAT(RELSTP)));
                  end if;
                  U := U - U*FLOAT_TYPE(RELSTP);
                  V := V + V*FLOAT_TYPE(RELSTP);
                  QUADSD(U, V, P, QUOTP, A, B);
                  for I in 1..5 loop
                     QUADSD(U, V, KPOLY, QUOTK, C, D);
                     CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                            K => KPOLY,
                            A1 => A1, A3 => A3, A7 => A7,
                            F => F, G => G, H => H,
                            NORM_TYPE => NORM_TYPE);
                     NEXTK(NORM_TYPE => NORM_TYPE,
                           A => A, B => B, A1 => A1, A3 => A3, A7 => A7,
                           QP => QUOTP, QK => QUOTK, K => KPOLY);
                  end loop;
                  TRIED := true;
                  J := 0;
               end if;
   
               OMP := MP;
               -- CALCULATE NEXT K POLYNOMIAL AND NEW U AND V
               QUADSD(U, V, KPOLY, QUOTK, C, D);
               CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                      K => KPOLY,
                      A1 => A1, A3 => A3, A7 => A7,
                      F => F, G => G, H => H,
                      NORM_TYPE => NORM_TYPE);
               NEXTK(NORM_TYPE => NORM_TYPE,
                     A => A, B => B, A1 => A1, A3 => A3, A7 => A7,
                     QP => QUOTP, QK => QUOTK, K => KPOLY);

               QUADSD(U, V, KPOLY, QUOTK, C, D);
               CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                      K => KPOLY,
                      A1 => A1, A3 => A3, A7 => A7,
                      F => F, G => G, H => H,
                      NORM_TYPE => NORM_TYPE);
               NEWEST(NORM_TYPE => NORM_TYPE, P => P, K => KPOLY,
                      U => U, V => V,
                      A => A, B => B, C => C, D => D, F => F, G => G, H => H,
                      A1 => A1, A3 => A3, A7 => A7, UU => UI, VV => VI);
   
               -- IF VI IS ZERO THE ITERATION IS NOT CONVERGING
               exit when VI = 0.0;
   
               RELSTP := SHORT_FLOAT_TYPE(abs((VI-V)/VI));
               U := UI;
               V := VI;
   
            end loop;
         end QUADIT;


      begin -- fxshfr

         U := U_INIT;
         V := V_INIT;
         NZ := 0;
         BETAV := 0.25;
         BETAS := 0.25;
         OSS := SR_INIT;
         OVV := SHORT_FLOAT_TYPE(V_INIT);
         OTV := 1.0;
         OTS := 1.0;

         -- EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION
         -- SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V
         -- page 563, eqn 9.6
         QUADSD(U, V, P,     QUOTP, A, B);
         QUADSD(U, V, KPOLY, QUOTK, C, D);
         CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                K => KPOLY,
                A1 => A1, A3 => A3, A7 => A7,
                F => F, G => G, H => H,
                NORM_TYPE => NORM_TYPE);

         OUTER_LOOP:
         for J in 1..L2 loop

            -- CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V
            NEXTK(NORM_TYPE => NORM_TYPE,
                  A => A, B => B, A1 => A1, A3 => A3, A7 => A7,
                  QP => QUOTP, QK => QUOTK, K => KPOLY);
            --SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V
            -- page 563, eqn 9.6
            QUADSD(U, V, KPOLY, QUOTK, C, D);
   
            CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                   K => KPOLY,
                   A1 => A1, A3 => A3, A7 => A7,
                   F => F, G => G, H => H,
                   NORM_TYPE => NORM_TYPE);
            NEWEST(NORM_TYPE => NORM_TYPE, P => P, K => KPOLY,
                   U => U, V => V,
                   A => A, B => B, C => C, D => D, F => F, G => G, H => H,
                   A1 => A1, A3 => A3, A7 => A7, UU => UI, VV => VI);
   
            VV := SHORT_FLOAT_TYPE(VI);

            --ESTIMATE S
            if KPOLY(KPOLY'last) /= 0.0 then
               SS := SHORT_FLOAT_TYPE(-P(P'last)/KPOLY(KPOLY'last));
            else
               SS := 0.0;
            end if;

            TV := 1.0;
            TS := 1.0;

            INNER_LOOP:
            while J /= 1 and NORM_TYPE /= 3 loop

               -- COMPUTE RELATIVE MEASURES OF 
               -- CONVERGENCE OF S AND V SEQUENCES
               if VV /= 0.0 then
                  TV := abs((VV-OVV)/VV);
               end if;
               if SS /= 0.0 then
                  TS := abs((SS-OSS)/SS);
               end if;

               -- IF DECREASING, MULTIPLY TWO MOST 
               -- RECENT CONVERGENCE MEASURES
               if TV < OTV then
                  TVV := TV*OTV;
               else
                  TVV := 1.0;
               end if;
               if TS < OTS then
                  TSS := TS*OTS;
               else
                  TSS := 1.0;
               end if;

               -- COMPARE WITH CONVERGENCE CRITERIA
               VPASS := TVV < BETAV;
               SPASS := TSS < BETAS;
               exit INNER_LOOP when not (SPASS or VPASS);

               -- AT LEAST ONE SEQUENCE HAS PASSED THE
               -- CONVERGENCE TEST. STORE VARIABLES
               -- BEFORE ITERATING
               S := FLOAT_TYPE(SS);

               -- CHOOSE ITERATION ACCORDING TO THE
               -- FASTEST CONVERGING SEQUENCE
               VTRY := false;
               STRY := false;
               if SPASS and ((not VPASS) or (TSS < TVV)) then
                  -- linear to start
                  DONE_Q_L := 2;
               else
                  -- quadratic to start
                  DONE_Q_L := 1;
               end if;

               while DONE_Q_L /= 0 loop

                  case DONE_Q_L is

                     when 1 =>
                        QUADIT(P => P, UU => UI, VV => VI, K => KPOLY,
                               QP => QUOTP, ZEROR => ZEROR, ZEROI => ZEROI, 
                               NZ => NZ1);
                        if NZ1 > 0 then
                           NZ := NZ1;
                           QP := QUOTP;
                           exit OUTER_LOOP;
                        end if;

                        -- QUADRATIC ITERATION HAS FAILED.
                        -- FLAG THAT IT HAS BEEN TRIED AND
                        -- DECREASE THE CONVERGENCE CRITERION.
                        VTRY := true;
                        BETAV := BETAV*0.25;

                        -- TRY LINEAR ITERATION IF IT HAS
                        -- HAS NOT BEEN TRIED AND THE S
                        -- SEQUENCE IS CONVERGING
                        if (STRY or (not SPASS)) then
                           DONE_Q_L := 0;
                        else 
                           DONE_Q_L := 2;
                        end if;

                     when 2 =>
                        REALIT (P => P, SSS => S, K => KPOLY, QP => QUOTP,
                                SZR => SZR, NZ => NZ1, IFLAG => IFLAG);
                        if NZ1 > 0 then
                           NZ := NZ1;
                           QP := QUOTP;
                           ZEROR (1..2) := (1 => SZR, 2 => 0.0);
                           ZEROI (1..2) := (1 => 0.0, 2 => 0.0);
                           exit OUTER_LOOP;
                        end if;

                        -- LINEAR ITERATION HAS FAILED. FLAG
                        -- THAT IT HAS BEEN TRIED AND DECREASE THE 
                        -- CONVERGENCE CRITERION
                        STRY := true;
                        BETAS := BETAS*0.25;

                        if IFLAG then
                           -- IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE
                           -- REAL ZERO, ATTEMPT QUADRATIC INTERATION
                           DONE_Q_L := 1;
                           UI := -(SZR+SZR);
                           VI := SZR*SZR;
                        else
                           -- TRY QUADRATIC ITERATION IF IT HAS NOT
                           -- BEEN TRIED AND THE V SEQUENCE IS CONVERGING

                           if (VTRY or (not VPASS)) then
                              DONE_Q_L := 0;
                           else 
                              DONE_Q_L := 1;
                           end if;

                        end if;
                       
                     when others =>
                        DONE_Q_L := 0;
                  end case;
               end loop;

               -- RECOMPUTE QP AND SCALAR VALUES TO
               -- CONTINUE THE SECOND STAGE
               QUADSD(U, V, P,     QUOTP, A, B);
               QUADSD(U, V, KPOLY, QUOTK, C, D);
               CALCSC(A => A, B => B, C => C, D => D, U => U, V => V,
                      K => KPOLY,
                      A1 => A1, A3 => A3, A7 => A7,
                      F => F, G => G, H => H,
                      NORM_TYPE => NORM_TYPE);
               exit INNER_LOOP;
            end loop INNER_LOOP;

            OVV := VV;
            OSS := SS;
            OTV := TV;
            OTS := TS;

         end loop OUTER_LOOP;

      end FXSHFR;



   begin -- rpoly

      FAIL := false;
      N := DEGREE;
      NN := N + 1;

      -- ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.
      if OP(1) = 0.0 then
         FAIL := true;
         DEGREE := 0;
         return;
      end if;

      -- REMOVE THE ZEROS AT THE ORIGIN IF ANY
      while OP(NN) = 0.0 loop
         J := DEGREE - N + 1;
         ZEROR(J) := 0.0;
         ZEROI(J) := 0.0;
         NN := NN - 1;
         N  := N - 1;
      end loop;

      --MAKE A COPY OF THE COEFFICIENTS
      P(1..NN) := OP(1..NN);

      OUTER_LOOP:
      loop
         -- START THE ALGORITHM FOR ONE ZERO

         if N <= 2 then
            -- CALCULATE THE FINAL ZERO OR PAIR OF ZEROS
            case N is
               when 1 =>
                  ZEROR(DEGREE) := -P(2)/P(1);
                  ZEROI(DEGREE) := 0.0;

               when 2 =>
                  QUAD(P(1), P(2), P(3),
                       ZEROR(DEGREE-1), ZEROI(DEGREE-1),
                       ZEROR(DEGREE),   ZEROI(DEGREE));

               when others =>
                  null;

            end case;
            exit OUTER_LOOP;
         end if;

         -- SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS
         -- COMPUTES A SCALE FACTOR TO MULTIPLY THE
         -- COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE
         -- TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW
         -- INTERFERING WITH THE CONVERGENCE CRITERION.
         -- THE FACTOR IS A POWER OF THE BASE

         -- COMPUTE LOWER BOUND ON MODULI OF ZEROS.

         SCALE_AND_FIND_BOUND (P => P(1..NN), BOUND => BND);


         -- COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL,
         -- scaled by 1/N, AND DO 5 STEPS WITH NO SHIFT (page 561, M = 5)
         -- "to accentuate the small zeros" (page 563, eqn 9.5)
         K(1..N) := SCALED_DERIVATIVE (P => P(1..NN));
         SINGLE_NOSHIFT_SEQ (P => P(1..NN), K => K(1..N), M => 5);

         -- LOOP TO SELECT THE QUADRATIC  CORRESPONDING TO EACH
         -- NEW SHIFT
         SHIFT_LOOP:
         for CNT in 1..20 loop
            -- QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A
            -- NON-REAL POINT AND ITS COMPLEX CONJUGATE. THE POINT
            -- HAS MODULUS BND AND AMPLITUDE ROTATED BY 94 DEGREES
            -- FROM THE PREVIOUS SHIFT
            ROTATE_94 (X => XX, Y => YY);

            -- s(z) = (z - S)(z - S*) = z^2 -2*Re(S) z + |S|^2 = z^2 + U z + V 
            -- SR  := FLOAT_TYPE(BND*XX); -- Re(S) = |S|cos(r)
            -- SI  := FLOAT_TYPE(BND*YY); -- Im(S) = |S|sin(r)
            -- U   := -2.0*SR;
            -- V   := FLOAT_TYPE(BND*BND);
            -- V   := FLOAT_TYPE(BND); -- Error in 493.f: should be BND**2

            -- SECOND STAGE CALCULATION, FIXED QUADRATIC
            FXSHFR(L2      => 20*CNT,
                   P       => P(1..NN),
                   SR_INIT => BND*XX,
                   U_INIT  => -2.0*FLOAT_TYPE(BND*XX),
                   V_INIT  => FLOAT_TYPE(BND*BND),
                   K       => K(1..N),
                   QP      => QP(1..NN),
                   ZEROR   => ZR,
                   ZEROI   => ZI,
                   NZ      => NZ);
            
            if NZ /= 0 then
               -- THE SECOND STAGE JUMPS DIRECTLY TO
               -- ONE OF THE THIRD STAGE ITERATIONS
               -- AND RETURNS HERE IF SUCCESSFUL.
               -- DEFLATE THE POLYNOMIAL, STORE THE
               -- ZERO OR ZEROS AND RETURN TO THE MAIN
               -- ALGORITHM.

               J := DEGREE - N + 1;
               ZEROR(J) := ZR(1);
               ZEROI(J) := ZI(1);
               NN := NN - NZ;
               N := NN - 1;
               P(1..NN) := QP(1..NN);
               if NZ /= 1 then
                  ZEROR(J+1) := ZR(2);
                  ZEROI(J+1) := ZI(2);
               end if;
               exit SHIFT_LOOP;
            end if;
         end loop SHIFT_LOOP;

         if NZ = 0 then
            -- RETURN WITH FAILURE IF NO CONVERGENCE WITH 20 SHIFTS
            FAIL := true;
            DEGREE := DEGREE - N;
            exit OUTER_LOOP;
         end if;

      end loop OUTER_LOOP;
   end RPOLY;



   -- Quadratic Synthetic Division
   -- Divide z^2 + U z + V into P
   -- Q must be same length as P (not 2 coefficients shorter than P)
   -- The next to the last element of Q holds a copy of B, the 1/z term and
   -- the last element of Q holds a copy of A, the 1/z^2 term.
   -- The standard polynomial remainder, R = B (z + U) + A

   -- DIVIDES P BY THE QUADRATIC  1,U,V  PLACING THE
   -- QUOTIENT IN Q AND THE REMAINDER IN A,B

   procedure QUADSD (U  : in     FLOAT_TYPE;
                     V  : in     FLOAT_TYPE;
                     P  : in     FLOAT_VECTOR_TYPE;
                     Q  :    out FLOAT_VECTOR_TYPE;
                     A  :    out FLOAT_TYPE;
                     B  :    out FLOAT_TYPE) is

      A1, B1, C : FLOAT_TYPE;

   begin -- quadsd

      B1   := P(1);
      Q(1) := B1;
      A1   := P(2) - U*B1;
      Q(2) := A1;

      for I in 3..P'last loop
         C    := P(I) - U*A1 - V*B1;
         Q(I) := C;
         B1   := A1;
         A1   := C;
      end loop;

      A := A1;
      B := B1;

   end QUADSD;

   -- Quadratic formula
   -- CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+B1*Z+C.
   -- THE QUADRATIC FORMULA, MODIFIED TO AVOID
   -- OVERFLOW, IS USED TO FIND THE LARGER ZERO IF THE
   -- ZEROS ARE REAL AND BOTH ZEROS ARE COMPLEX.
   -- THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE
   -- PRODUCT OF THE ZEROS C/A.

   procedure QUAD (A  : in     FLOAT_TYPE;
                   B1 : in     FLOAT_TYPE;
                   C  : in     FLOAT_TYPE;
                   SR :    out FLOAT_TYPE;
                   SI :    out FLOAT_TYPE;
                   LR :    out FLOAT_TYPE;
                   LI :    out FLOAT_TYPE) is

      B, D, E : FLOAT_TYPE;
      TMPR, TMPI : FLOAT_TYPE;

   begin --quad

      if A = 0.0 then
         -- line
         if B1 = 0.0 then
            -- c = 0
            -- constant: no roots or infinite number of roots
            SR := 0.0;
         else
            -- b1*x + c = 0
            -- x intercept is the root
            SR := -C/B1;
         end if;

         SI := 0.0;
         LR := 0.0;
         LI := 0.0;

      elsif C = 0.0 then
         -- special quadratic: (x)(x - lr) = 0

         -- zero is a root
         SR := 0.0;
         SI := 0.0;

         -- x intercept of linear factor is a root
         LR := -B1/A;
         LI := 0.0;

      else
         -- general quadratic

         -- COMPUTE DISCRIMINANT AVOIDING OVERFLOW
         B := B1/2.0;
         if abs(B) >= abs(C) then
            E := 1.0 - (A/B) * (C/B);
            D := FLOAT_TYPE (MATH.SQRTL (MATH.LONG_DOUBLE(abs(E)))) * abs(B);
         else
            if C < 0.0 then
               E := B * (B/abs(C)) + A;
            else
               E := B * (B/abs(C)) - A;
            end if;
            D :=  FLOAT_TYPE (MATH.SQRTL (MATH.LONG_DOUBLE(abs(E)))) 
                * FLOAT_TYPE (MATH.SQRTL (MATH.LONG_DOUBLE(abs(C))));
         end if;

         if E >= 0.0 then
            -- REAL ZEROS
            if B >= 0.0 then
               TMPR := (-B-D)/A;
            else
               TMPR := (-B+D)/A;
            end if;
            LR := TMPR;
            LI := 0.0;
            if TMPR /= 0.0 then
               SR := (C/TMPR)/A;
            else
               SR := 0.0;
            end if;
            SI := 0.0;
         else
            -- COMPLEX CONJUGATE ZEROS
            TMPR := -B/A;
            TMPI := abs(D/A);
            SR := TMPR;
            SI := TMPI;
            LR := TMPR;
            LI := -TMPI;
         end if;
      end if;
   end QUAD;

end JENKINS_TRAUB;
