-- Some examples of functional programming for Gofer

-- Factorials:

fact n = product [1..n]                     -- a simple definition

fac n  = if n==0 then 1 else n * fac (n-1)  -- a recursive definition

fac' 0 = 1                                  -- using two equations
fac' n = n * fac (n-1)

facts  = scanl (*) 1 [1..]                  -- the infinite list of factorials

facts' = 1 : zipWith (*) facts' [1..]       -- another way of doing it

facFix = fixedPt f                          -- using a fixed point combinator
         where  f g 0       = 1             -- overlapping patterns
                f g n       = n * g (n-1)
                fixedPt f = g where g = f g -- fixed point combinator

facCase = \n -> case n of
                  0     ->  1
                  (m+1) -> (m+1) * facCase m

-- Fibonacci numbers:

fib 0     = 0                               -- using pattern matching:
fib 1     = 1                               -- base cases...
fib (n+2) = fib n + fib (n+1)               -- recursive case

fastFib n    = fibs !! n                    -- using an infinite stream
               where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

cnkfib 1       = 1			    -- using cnk patterns, in a form
cnkfib 2       = 1			    -- suggested by Tony Davie
cnkfib (2*n)   = (cnkfib(n+1))^^2 - (cnkfib(n-1))^^2
cnkfib (2*n+1) = (cnkfib(n+1))^^2 + (cnkfib n   )^^2

x^^0           = 1			    -- A fast implementation of
x^^(2*n)       = xn*xn where xn = x^^n	    -- exponentiation
x^^(2*n+1)     = x * x^^(2*n)

-- Perfect numbers:

factors n    = [ i | i<-[1..n-1], n `mod` i == 0 ]
perfect n    = sum (factors n) == n
firstperfect = head perfects
perfects     = filter perfect [1..]

-- Prime numbers:

primes       = map head (iterate sieve [2..])
sieve (p:xs) = [ x | x<-xs, x `rem` p /= 0 ]

-- Pythagorean triads:

triads n     = [ (x,y,z) | ns=[1..n], x<-ns, y<-ns, z<-ns, x*x+y*y==z*z ]

-- The Hamming problem:

hamming     :: [Int]
hamming      = 1 : (map (2*) hamming || map (3*) hamming || map (5*) hamming)
               where (x:xs) || (y:ys)  | x==y  =  x : (xs || ys)
                                       | x<y   =  x : (xs || (y:ys))
                                       | y<x   =  y : (ys || (x:xs))

-- Digits of e:

eFactBase ::  [Int]
eFactBase  =  map head (iterate scale (2:repeat 1))

scale      =  renorm . map (10*) . tail
renorm ds  =  foldr step [0] (zip ds [2..])

step (d,n) bs | (d `mod` n + 9) < n  = (d/n) : b : tail bs
              | otherwise            = c     : b : tail bs
              where b' = head bs
                    b  = (d+b') `mod` n
                    c  = (d+b') `div` n

-- Pascal's triangle

pascal = iterate (\row -> zipWith (+) ([0]++row) (row++[0])) [1]

showPascal = (layn . map show . take 14) pascal
