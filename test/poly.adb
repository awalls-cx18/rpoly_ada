with GENERIC_POLYNOMIAL;
with TEXT_IO;
--with Ada.Numerics.Float_Random;
--with Ada.Command_Line;

procedure POLY is

   --package RF renames Ada.Numerics.Float_Random;

   type POLY_TYPE is array (NATURAL range <>) of LONG_LONG_FLOAT;
   package POLY_OPS is 
                     new GENERIC_POLYNOMIAL (FLOAT_TYPE => LONG_LONG_FLOAT,
                                             POLYNOMIAL_TYPE => POLY_TYPE);

   package NIO is new TEXT_IO.INTEGER_IO (NATURAL);
   package FIO is new TEXT_IO.FLOAT_IO (LONG_LONG_FLOAT);

   --RGEN : RF.GENERATOR;

   M, N : NATURAL;
   P, Q : POLY_TYPE (0..50);
   X : LONG_LONG_FLOAT;
   --T : POLY_TYPE (0..1000);

   R : POLY_TYPE(0..2) := (0|2 => 1.0, 1 => 2.0);
   S : POLY_TYPE(2..3) := (2 => 2.0, 3 => 5.0);

   procedure PRINT_POLY (P : in POLY_TYPE) is
   begin

      for I in P'range loop

         if I /= P'first then
            TEXT_IO.PUT (" + ");
         end if;

         FIO.PUT (ITEM => P(I), FORE => 2, AFT => 0, EXP => 0);

         if I > 0 then
            TEXT_IO.PUT (" t^");
            NIO.PUT (ITEM => I, WIDTH => 0);
         end if;

      end loop;

      TEXT_IO.NEW_LINE;

   end PRINT_POLY;

begin -- POLY

   --case Ada.Command_Line.Argument_Count is
      --when 3 =>
         --POLY_OPS.SET_MULTIPLICATION_ALGORITHM (ALG => POLY_OPS.KARATSUBA);
      --when others =>
         --POLY_OPS.SET_MULTIPLICATION_ALGORITHM (ALG => POLY_OPS.CONVOLUTION);
   --end case;

   --RF.RESET(RGEN);
   --for I in 1..NATURAL'VALUE(Ada.Command_Line.Argument(1)) loop
      --for J in 0..NATURAL'VALUE(Ada.Command_Line.Argument(2)) loop
         --P(J) := RF.RANDOM(RGEN);
         --Q(J) := RF.RANDOM(RGEN);
      --end loop;
      --T := POLY_OPS."*"(P,Q);
   --end loop;
   --return;
         
   while not TEXT_IO.END_OF_FILE loop

      --POLY_OPS.SET_MULTIPLICATION_ALGORITHM (ALG => POLY_OPS.KARATSUBA);

      -- Read in first polynomial coefficients on one line
      M := 0;
      while not TEXT_IO.END_OF_LINE loop
         FIO.GET (P(M)); 
         M := M + 1;
      end loop;

      -- Get the next polynomial 
      exit when TEXT_IO.END_OF_FILE;
      TEXT_IO.SKIP_LINE;

      -- Read in second polynomial coefficients on one line
      N := 0;
      while not TEXT_IO.END_OF_LINE loop
         FIO.GET (Q(N)); 
         N := N + 1;
      end loop;

      -- Get an evaluation point
      exit when TEXT_IO.END_OF_FILE;
      TEXT_IO.SKIP_LINE;

      FIO.GET (X); 

      -- Print out the polynomial operations 
      TEXT_IO.PUT ("X = "); FIO.PUT (X); TEXT_IO.NEW_LINE;
      TEXT_IO.PUT ("P(X) = "); FIO.PUT (POLY_OPS.EVAL(P(0..M-1),X)); TEXT_IO.NEW_LINE;
      TEXT_IO.PUT ("Q(X) = "); FIO.PUT (POLY_OPS.EVAL(Q(0..N-1),X)); TEXT_IO.NEW_LINE;
      TEXT_IO.PUT ("P * Q = "); PRINT_POLY (POLY_OPS."*"(P(0..M-1),Q(0..N-1)));

      TEXT_IO.PUT ("(P * Q)(X) = ");
      FIO.PUT(POLY_OPS.EVAL(POLY_OPS."*"(P(0..M-1),Q(0..N-1)),X));
      TEXT_IO.NEW_LINE;

      
      -- Get the next polynomial 
      exit when TEXT_IO.END_OF_FILE;
      TEXT_IO.SKIP_LINE;
   end loop;

   TEXT_IO.NEW_LINE;

   PRINT_POLY (R);
   PRINT_POLY (POLY_OPS."*"(R,R));
   PRINT_POLY (POLY_OPS."*"(0.0 & R,R));
   PRINT_POLY (POLY_OPS."*"(R, 0.0 & R));
   PRINT_POLY (POLY_OPS."*"(0.0 & R, 0.0 & R));

   PRINT_POLY (S);
   PRINT_POLY (POLY_OPS."*"(S,S));
   PRINT_POLY (POLY_OPS."*"(S,R));
   PRINT_POLY (POLY_OPS."*"(R,S));

end POLY;
