package MATH is

   type FLOAT       is new STANDARD.FLOAT;
   type DOUBLE      is new LONG_FLOAT;
   type LONG_DOUBLE is new LONG_LONG_FLOAT;

   PI : constant :=
                  3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;

   E : constant :=
                  2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;

   SQRT_TWO : constant := 1.41421_35623_73095_04880_16887_24209_69807_85696;

   function COSF  (X : in FLOAT) return FLOAT;
   function SINF  (X : in FLOAT) return FLOAT;
   function EXPF  (X : in FLOAT) return FLOAT;
   function SQRTF (X : in FLOAT) return FLOAT;
   function LOGF  (X : in FLOAT) return FLOAT;
   pragma INTERFACE (C, COSF);
   pragma INTERFACE (C, SINF);
   pragma INTERFACE (C, EXPF);
   pragma INTERFACE (C, SQRTF);
   pragma INTERFACE (C, LOGF);

   function COS  (X : in DOUBLE) return DOUBLE;
   function SIN  (X : in DOUBLE) return DOUBLE;
   function EXP  (X : in DOUBLE) return DOUBLE;
   function SQRT (X : in DOUBLE) return DOUBLE;
   function LOG  (X : in DOUBLE) return DOUBLE;
   pragma INTERFACE (C, COS);
   pragma INTERFACE (C, SIN);
   pragma INTERFACE (C, EXP);
   pragma INTERFACE (C, SQRT);
   pragma INTERFACE (C, LOG);

   function COSL  (X : in LONG_DOUBLE) return LONG_DOUBLE;
   function SINL  (X : in LONG_DOUBLE) return LONG_DOUBLE;
   function EXPL  (X : in LONG_DOUBLE) return LONG_DOUBLE;
   function SQRTL (X : in LONG_DOUBLE) return LONG_DOUBLE;
   function LOGL  (X : in LONG_DOUBLE) return LONG_DOUBLE;
   pragma INTERFACE (C, COSL);
   pragma INTERFACE (C, SINL);
   pragma INTERFACE (C, EXPL);
   pragma INTERFACE (C, SQRTL);
   pragma INTERFACE (C, LOGL);

end MATH;
