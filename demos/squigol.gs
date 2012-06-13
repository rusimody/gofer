-- A couple of examples defining an ascii form of squigol notation for Gofer:
-- All of these are of course just different syntax for standard prelude
-- functions:

infixr  5  **,   <|,   <-/-,  -/->,  -//->

f ** xs  =  [ f x | x<-xs ]             -- map
p <| xs  =  [ x | x<-xs, p x ]          -- filter

(a <-/- f) []     = a                   -- foldr
(a <-/- f) (x:xs) = f x ((a <-/- f) xs)

(f -/-> a) []     = a                   -- foldl
(f -/-> a) (x:xs) = (f -/-> f a x) xs

(f -//-> a) xs    = a : (case xs of     -- scanl
                         []     -> []
                         (x:xs) -> (f -//-> f a x) xs)

-- Here's another piece of notation -- not squigol, but of a similar flavour
-- which would enable us to do away with the zipWith family of functions:
--
-- map f xs1 << xs2 << ... << xsn  = zipWithn f xs1 xs2 ... xsn
--
-- in terms of the old notation, (<<) = zipWith (\f x->f x)

infixl 0 <<

f:fs << x:xs  = f x : (fs << xs)
_    << _     = []
