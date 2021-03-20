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
