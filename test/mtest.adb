-- Copyright (C) 2004-2021 Andy Walls <awalls.cx18@gmail.com>
--
-- This is the mtest program.
--
-- The mtest program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- The mtest program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with the mtest program.  If not, see <https://www.gnu.org/licenses/>.

with MATH;
with TEXT_IO;

procedure MTEST is

   package FIO is new TEXT_IO.FLOAT_IO (MATH.FLOAT);
   package IIO is new TEXT_IO.INTEGER_IO (INTEGER);

   DEG2RAD : constant := MATH.PI/180.0;
   R : MATH.FLOAT;

begin

   for I in 0..360 loop

      IIO.PUT (I); TEXT_IO.PUT (ASCII.HT);

      R := MATH.FLOAT(FLOAT(I) * DEG2RAD);
      FIO.PUT (R); TEXT_IO.PUT (ASCII.HT);

      FIO.PUT (MATH.SQRTF (R)); TEXT_IO.PUT (ASCII.HT);

      FIO.PUT (MATH.COSF  (R)); TEXT_IO.PUT (ASCII.HT);

      FIO.PUT (MATH.SINF (R)); TEXT_IO.NEW_LINE;

   end loop;
      
end MTEST;
