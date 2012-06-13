-------------------------------------------------------------------------------
-- The following random number generator is an implementation of the
-- Minimum Standard generator recommended in
--
--    Random Number Generators: Good ones are hard to find
--       Stephen K Park & Keith W Miller
--       Communications of the ACM, Oct 88, Vol 31 No 10 1192 - 1201
--
-- Seeds must be in the range 1..2147483646, that is (1..(2**31)-2)
-- Output will also be in that range. The generator is full period so that
-- all 2147483646 values will be generated before the initial seed repeats.
-- Dividing by 2147483647 (real) as in the Pascal code below will map it
-- into the range (0..1) if required.
--
-- [This program assumes that you are working on a machine with (at least)
-- 32 bit integers.  Folks using Gofer on a PC will have to stick with the
-- less sophisticated random number generator in the file `randoms'.]
-------------------------------------------------------------------------------

min_stand_test  :: Int -> Int
min_stand_test n = if test > 0 then test else test + 2147483647
		   where test = 16807 * lo - 2836 * hi
		         hi   = n `div` 127773
		         lo   = n `rem` 127773

min_stand_randoms :: Int -> [Int]
min_stand_randoms  = iterate min_stand_test

-- The article produced below also gives a test to check that the
-- random number generator is working.  We can duplicate this test
-- as follows:
--
--   ? strictIterate min_stand_test 1 !! 10000
--   1043618065
--   (149758 reductions, 240096 cells, 2 garbage collections)
--
-- Happily, this is the result that we expect to obtain.
--
-- The function strictIterate is defined below.  It is similar to the
-- standard iterate function except that it forces the evaluation of
-- each element in the list produced (except possibly the first).
-- Had we instead tried to evaluate:
--
--   iterate min_stand_test 1 !! 10000
--
-- Gofer would have first constructed the expression graph:
--
--   min_stand_test (min_stand_test (... (min_stand_test 1) ...))
--
-- in which the min_stand_test function is applied 10000 times to 1
-- and then attempted to evaluate this.  In either case, you'd need a
-- large heap to represent the complete expression and a large stack so
-- that you could handle 10000 levels of function calling.  Most standard
-- configurations of Gofer aren't set up with sufficiently large defaults
-- to make this possible, so the most likely outcome would be a runtime
-- error of one kind or another!

strictIterate    :: (a -> a) -> a -> [a]
strictIterate f x = x : strict (strictIterate f) (f x)

-------------------------------------------------------------------------------
-- Some comments and code from:
--
-- Random Number Generators: Good ones are hard to find
--    Stephen K Park & Keith W Miller
--    Communications of the ACM, Oct 88, Vol 31 No 10 1192 - 1201
-- 
-- Minimum standard random number generator implementations
-- 
-- This version of Random will be correct if reals are represented
-- with a 46-bit or larger mantissa (excluding the sign bit).
-- For example, this version will be correct on all systems that support
-- the IEEE 64-bit real arithmetic standard since the mantissa in that case
-- is 53-bits.
-- ... from page 1195 upper right quadrant
-- 
-- var seed : real;
-- ...
-- function Random : real;
-- 	(* Real Version 1 *)
-- const
--    a = 16807.0;
--    m = 2147483647.0;
-- var
--    temp : real;
-- begin
--    temp := a * seed;
--    seed :=
--       temp - m * Trunc(temp / m);
--    Random := seed / m;
-- end;
-- 
-- ... from page 1195 lower right quadrant, variant by L. Schrage, 1979, 1983
--
-- var seed : integer;
-- ...
-- function Random : real;
-- 	(* Integer Version 2 *)
-- const
--    a = 16807;
--    m = 2147483647;
--    q = 127773;	(* m div a *)
--    r = 2836;	(* m mod a *)
-- var
--    lo, hi, test : integer;
-- begin
--    hi := seed div q;
--    lo := seed mod q;
--    test := a * lo - r * hi;
--    if test > 0 then
--       seed := test
--    else
--       seed := test + m;
-- 
--    Random := seed / m;
-- end;
-- 
-- From page 1195 lower left quadrant
--
-- seed := 1;
-- for n := 1 to 10000 do
--    u := Random;
-- Writeln('The current value of seed is : ', seed);
-- (* Expect 1043618065 *)
-------------------------------------------------------------------------------
