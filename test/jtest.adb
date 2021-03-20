with jenkins_traub;
with text_io;

procedure jtest is

   type float_vector_type is array (positive range <>) of long_long_float;
   package jt is new jenkins_traub (float_type => long_long_float,
                                    float_vector_type => float_vector_type);
   package fio is new text_io.float_io(long_long_float);
   package iio is new text_io.integer_io(integer);

   p    : float_vector_type (1..21);
   n    : integer;

   -- Print Polynomial coefficients
   procedure prtc (p : in float_vector_type;
                   n : in integer) is
   begin -- prtc
      text_io.new_line;
      for i in 1..positive(n) loop
         fio.put (p(i)); text_io.put (" z^");
         iio.put (n+1-i,2); text_io.put_line(" +");
      end loop;
      fio.put (p(n+1)); text_io.new_line;
   end prtc;

   -- Print Real and Imaginary parts of zeros
   procedure prtz (zr : in float_vector_type;
                   zi : in float_vector_type;
                   n  : in integer) is
   begin -- prtz
      text_io.new_line;
      for i in 1..positive(n) loop
         fio.put (zr(i)); text_io.put (' ');
         fio.put (zi(i)); text_io.new_line;
      end loop;
   end prtz;

   procedure go (p : in float_vector_type;
                 n : in integer) is
      zr   : float_vector_type (1..20);
      zi   : float_vector_type (1..20);
      fail : boolean;
      deg  : integer := n;
   begin
      -- Print polynomial
      prtc(p, deg);

      -- Solve for zeros
      jt.rpoly (op     => p,
                degree => deg,
                zeror  => zr,
                zeroi  => zi,
                fail   => fail);

      -- Print zeros
      if fail then
         text_io.put("Fail - degree = ");
         iio.put (deg);
         text_io.new_line;
      else
         prtz(zr, zi, deg);
      end if;
   end go;

begin -- jtest

   while not text_io.end_of_file loop
      n := 0;

      -- Read in polynomial coefficients on one line
      while not text_io.end_of_line loop
         n := n + 1;
         fio.get (p(n)); 
      end loop;

      -- Print out the polynomial roots
      n := n - 1;
      go(p, n);
      text_io.new_line;

      -- Get the next polynomial 
      exit when text_io.end_of_file;
   text_io.skip_line;
   end loop;

-- text_io.put_line ("Example 1: Zeros 1,2,...,10.");
-- 1.0 -55.0 1320.0 -18150.0 157773.0 -902055.0 3416930.0 -8409500.0 12753576.0 -10628640.0 3628800.0

--text_io.put_line ("Example 2: Zeros -1,-2,-3.");
--1.0 6.0 11.0 6.0

--text_io.put_line ("Example 3: Zeros -2,-3.");
-- 1.0 5.0 6.0

end jtest;
