-- Copyright (C) 2004-2021 Andy Walls <awalls.cx18@gmail.com>
--
-- This is the pjtest program.
--
-- The pjtest program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- The pjtest program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with the pjtest program.  If not, see <https://www.gnu.org/licenses/>.

with GENERIC_POLYNOMIAL;
with TEXT_IO;
--with Ada.Numerics.Float_Random;
--with Ada.Command_Line;

procedure PJTEST is

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
   T : POLY_TYPE (0..100);
   ZR, ZI : POLY_TYPE (0..99);
   RCOUNT : INTEGER;
   FAIL : BOOLEAN;
   X : LONG_LONG_FLOAT;

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

   -- Print Real and Imaginary parts of zeros
   procedure prtz (zr : in poly_type;
                   zi : in poly_type;
                   n  : in integer) is
   begin -- prtz
      text_io.new_line;
      for i in 0..n-1 loop
         fio.put (zr(i)); text_io.put (' ');
         fio.put (zi(i)); text_io.put_line ("i");
      end loop;
   end prtz;
begin -- PJTEST

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

      TEXT_IO.PUT ("P = "); PRINT_POLY (P(0..M-1));
      TEXT_IO.PUT ("P(X) = "); FIO.PUT (POLY_OPS.EVAL(P(0..M-1),X)); TEXT_IO.NEW_LINE;
      TEXT_IO.PUT ("P(X) Roots:");
      POLY_OPS.ROOTS (P(0..M-1), RCOUNT, ZR, ZI, FAIL);
      PRTZ (ZR, ZI, RCOUNT);

      TEXT_IO.PUT ("Q = "); PRINT_POLY (Q(0..N-1));
      TEXT_IO.PUT ("Q(X) = "); FIO.PUT (POLY_OPS.EVAL(Q(0..N-1),X)); TEXT_IO.NEW_LINE;
      TEXT_IO.PUT ("Q(X) Roots:");
      POLY_OPS.ROOTS (Q(0..N-1), RCOUNT, ZR, ZI, FAIL);
      PRTZ (ZR, ZI, RCOUNT);

      T(0..M+N-2) := POLY_OPS."*"(P(0..M-1),Q(0..N-1));
      TEXT_IO.PUT ("P * Q = "); PRINT_POLY (T(0..M+N-2));

      TEXT_IO.PUT ("(P * Q)(X) = ");
      FIO.PUT(POLY_OPS.EVAL(T(0..M+N-2),X));
      TEXT_IO.NEW_LINE;
      POLY_OPS.ROOTS (T(0..M+N-2), RCOUNT, ZR, ZI, FAIL);
      PRTZ (ZR, ZI, RCOUNT);

      
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

end PJTEST;
