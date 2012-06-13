-- This file contains a Gofer implementation of the programs described in:
--
-- Computing with lattices: An application of type classes,
-- Mark P. Jones,
-- Technical report PRG-TR-11-90,
-- Programming Research Group,
-- Oxford University Computing Laboratory, June 1990.
--
-- A substantially revised version of this paper has now been published
-- in the Journal of Functional Programming, Volume 2, Number 4, Oct 1992.
--

class Eq a => Lattice a where           -- A type class representing lattices
    bottom, top :: a
    meet, join  :: a -> a -> a
    lt          :: a -> a -> Bool
    x `lt` y     = (x `join` y) == y

instance Lattice Bool where             -- Simple instances of Lattice
    bottom = False
    top    = True
    meet   = (&&)
    join   = (||)

instance (Lattice a, Lattice b) => Lattice (a,b) where
    bottom             = (bottom,bottom)
    top                = (top,top)
    (x,y) `meet` (u,v) = (x `meet` u, y `meet` v)
    (x,y) `join` (u,v) = (x `join` u, y `join` v)


-- Defining the least fixed point operator:

fix f          = firstRepeat (iterate f bottom)
firstRepeat xs = head [ x | (x,y) <- zip xs (tail xs), x==y ]


-- Maximum and minimum frontiers:

data Minf a = Minf [a]
data Maxf a = Maxf [a]

instance Eq a => Eq (Minf a) where                -- Equality on Frontiers
    (Minf xs) == (Minf ys)  = setEquals xs ys

instance Eq a => Eq (Maxf a) where
    (Maxf xs) == (Maxf ys)  = setEquals xs ys

xs `subset` ys  = all (`elem` ys) xs
setEquals xs ys =  xs `subset` ys  &&  ys `subset` xs

instance Lattice a => Lattice (Minf a) where      -- Lattice structure
    bottom                     = Minf []
    top                        = Minf [bottom]
    (Minf xs) `meet` (Minf ys) = minimal [ x`join`y | x<-xs, y<-ys ]
    (Minf xs) `join` (Minf ys) = minimal (xs++ys)

instance Lattice a => Lattice (Maxf a) where
    bottom                     = Maxf []
    top                        = Maxf [top]
    (Maxf xs) `meet` (Maxf ys) = maximal [ x`meet`y | x<-xs, y<-ys ]
    (Maxf xs) `join` (Maxf ys) = maximal (xs++ys)

-- Find maximal elements of a list xs with respect to partial order po:

maximalWrt po = loop []
 where loop xs []                 = xs
       loop xs (y:ys)
            | any (po y) (xs++ys) = loop xs ys
            | otherwise           = loop (y:xs) ys

minimal :: Lattice a => [a] -> Minf a       -- list to minimum frontier
minimal  = Minf . maximalWrt (flip lt)
maximal :: Lattice a => [a] -> Maxf a       -- list to maximum frontier
maximal  = Maxf . maximalWrt lt

-- A representation for functions of type Lattice a => a -> Bool:

data Fn a = Fn (Minf a) (Maxf a)

instance (Eq (Minf a), Eq (Maxf a)) => Eq (Fn a) where
    Fn f1 f0 == Fn g1 g0  =  f1==g1 -- && f0==g0

instance (Lattice (Minf a), Lattice (Maxf a)) => Lattice (Fn a) where
    bottom               = Fn bottom top
    top                  = Fn top bottom
    Fn u l `meet` Fn v m = Fn (u `meet` v) (l `join` m)
    Fn u l `join` Fn v m = Fn (u `join` v) (l `meet` m)

-- Navigable lattices:

class (Lattice (Minf a), Lattice (Maxf a)) => Navigable a where
    succs :: a -> Minf a
    preds :: a -> Maxf a

maxComp :: Navigable a => [a] -> Maxf a   -- implementation of complement
maxComp  = foldr meet top . map preds
minComp :: Navigable a => [a] -> Minf a
minComp  = foldr meet top . map succs

instance Navigable Bool where             -- instances of Navigable
    succs False = Minf [True]
    succs True  = Minf []
    preds False = Maxf []
    preds True  = Maxf [False]

instance (Navigable a, Navigable b) => Navigable (a,b) where
    succs (x,y) = Minf ([(sx,bottom) | Minf xs = succs x, sx<-xs] ++
                        [(bottom,sy) | Minf ys = succs y, sy<-ys])
    preds (x,y) = Maxf ([(px,top)    | Maxf xs = preds x, px<-xs] ++
                        [(top,py)    | Maxf ys = preds y, py<-ys])

instance Navigable a => Navigable (Fn a) where
    succs (Fn f1 f0) = Minf [Fn (Minf [y]) (preds y) | Maxf ys = f0, y<-ys]
    preds (Fn f1 f0) = Maxf [Fn (succs x) (Maxf [x]) | Minf xs = f1, x<-xs]

-- Upwards and downwards closure operators:

upwards (Minf [])         = []
upwards ts@(Minf (t:_))   = t : upwards (ts `meet` succs t)

downwards (Maxf [])       = []
downwards ts@(Maxf (t:_)) = t : downwards (ts `meet` preds t)

elements :: Navigable a => [a]    -- enumerate all elements in lattice
elements  = upwards top

-- Dual lattices:

class (Lattice a, Lattice b, Dual b a) => Dual a b where
    comp :: a -> b

instance Dual Bool Bool where
    comp = not

