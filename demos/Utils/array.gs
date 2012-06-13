-- An inefficient implementation of Haskell arrays based on the
-- functional specification in the Haskell report version 1.2
--
-- To save you some typing, just in case you wanted to use this
-- stuff ... but don't expect constant time lookup!

infixl 9  !
infixl 9  //
infix  1  :=

data Assoc a b =  a := b  
data Array a b = MkArray (a,a) (a -> b) 

array	   :: (Ix a) => (a,a) -> [Assoc a b] -> Array a b
listArray  :: (Ix a) => (a,a) -> [b] -> Array a b
(!)	   :: (Ix a) => Array a b -> a -> b
bounds	   :: (Ix a) => Array a b -> (a,a)
indices	   :: (Ix a) => Array a b -> [a]
elems	   :: (Ix a) => Array a b -> [b]
assocs	   :: (Ix a) => Array a b -> [Assoc a b]
accumArray :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b
(//)	   :: (Ix a) => Array a b -> [Assoc a b] -> Array a b
accum	   :: (Ix a) => (b -> c -> b) -> Array a b -> [Assoc a c] -> Array a b
amap	   :: (Ix a) => (b -> c) -> Array a b -> Array a c
ixmap	   :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

array b ivs           = MkArray b
			 (\j -> case [v | (i := v) <- ivs, i == j] of
 			    [v] -> v
			    []  -> error "(!){PreludeArray}: \
					 \undefined array element"
			    _   -> error "(!){PreludeArray}: \
					 \multiply defined array element")
listArray b vs	      = array b (zipWith (:=) (range b) vs)

(!) (MkArray _ f)     = f
bounds (MkArray b _)  = b
indices		      = range . bounds
elems a               = [a!i | i <- indices a]
assocs a              = [i := a!i | i <- indices a]
a // us		      = array (bounds a)
			    ([i := a!i | i <- indices a \\ [i | i:=_ <- us]]
			     ++ us)

accum f               = foldl (\a (i := v) -> a // [i := f (a!i) v])

accumArray f z b      = accum f (array b [i := z | i <- range b])
amap f a              = array b [i := f (a!i) | i <- range b]
                        where b = bounds a
ixmap b f a           = array b [i := a ! f i | i <- range b]

instance (Ix a, Text a, Text b) => Text (Assoc a b)  where
	showsPrec _ (a := b) = shows a . showString " := " . shows b

instance (Ix a) => Ix (a,a) where
    range ((ma,mb),(na,nb)) = [(a,b) | a <- range (ma,na), b <- range (mb,nb)]

-- Some applications, taken from the Gentle Introduction ...

fibs n = a where a = array (0,n) ([ 0 := 1, 1 := 1 ] ++
                                  [ i := a!(i-2) + a!(i-1) | i <- [2..n] ])

fibs10 = fibs 10

wavefront n = a where a = array ((1,1),(n,n))
                             ([ (1,j) := 1 | j <- [1..n] ] ++
                              [ (i,1) := 1 | i <- [2..n] ] ++
                              [ (i,j) := a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j)
                                           | i <- [2..n], j <- [2..n] ])

listwave n = [ [wf!(i,j) | j <- [1..n]] | i <- [1..n] ]
             where wf = wavefront n

