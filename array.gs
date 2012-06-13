-------------------------------------------------------------------------------
-- This file contains a Gofer implementation of the Haskell array datatype
-- using new Gofer primitives added in Gofer 2.30.
--
-- This file requires the standard, or cc prelude.
-- You will not be able to use this file unless the version of Gofer that
-- is installed on your machine has been compiled with the HASKELL_ARRAYS
-- flag set to 1.
--
-- Based on the standard prelude for Haskell 1.2.
-- Mark P Jones, 1994
-------------------------------------------------------------------------------

module PreludeArray( Array, Assoc((:=)), array, listArray, (!), bounds,
                    indices, elems, assocs, accumArray, (//), accum, amap,
                    ixmap
                  ) where

infixl 9 !
infixl 9 //
infix  1 :=

-- Associations:  Frankly, any pair type would do just as well ... ------------

data Assoc a b =  a := b

instance (Eq a, Eq b) => Eq (Assoc a b) where
    (x := y) == (u := v)  =  x==u && y==v

instance (Ord a, Ord b) => Ord (Assoc a b) where
    (x := y) <= (u := v)  =  x<u  ||  (x==u && y<=v)

instance (Text a, Text b) => Text (Assoc a b) where
    showsPrec d (x := y)
       = if d > 1 then showChar '(' . s . showChar ')'
                  else s
         where s = showsPrec 2 x . showString " := " . showsPrec 2 y

-- Array primitives: ----------------------------------------------------------

array      :: Ix a => (a,a) -> [Assoc a b] -> Array a b
listArray  :: Ix a => (a,a) -> [b] -> Array a b
(!)	   :: Ix a => Array a b -> a -> b
bounds     :: Ix a => Array a b -> (a,a)
indices	   :: Ix a => Array a b -> [a]
elems      :: Ix a => Array a b -> [b]
assocs	   :: Ix a => Array a b -> [Assoc a b]
accumArray :: Ix a => (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b
(//)       :: Ix a => Array a b -> [Assoc a b] -> Array a b
accum      :: Ix a => (b -> c -> b) -> Array a b -> [Assoc a c] -> Array a b
amap	   :: Ix a => (b -> c) -> Array a b -> Array a c
ixmap	   :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

instance (Ix a, Eq [Assoc a b]) => Eq (Array a b) where
    a == a'   =   assocs a == assocs a'

instance (Ix a, Ord [Assoc a b]) => Ord (Array a b) where
    a <= a'   =   assocs a <= assocs a'

instance (Ix a, Text (a,a), Text [Assoc a b]) => Text (Array a b) where
    showsPrec p a = if (p>9) then showChar '(' . s . showChar ')' else s
     where s = showString "array " .
	       shows (bounds a)    .
	       showChar ' '        .
	       shows (assocs a)

-- Implementation: ------------------------------------------------------------

primitive primArray "primArray"
    :: (a -> Int) -> (a,a) -> [Assoc a b] -> Array a b
primitive primUpdate "primUpdate"
    :: (a -> Int) -> Array a b -> [Assoc a b] -> Array a b
primitive primAccum "primAccum"
    :: (a -> Int) -> (b -> c -> b) -> Array a b -> [Assoc a c] -> Array a b
primitive primAccumArray "primAccumArray"
    :: (a -> Int) -> (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b
primitive primBounds    "primBounds"    :: Array a b -> (a,a)
primitive primElems     "primElems"     :: Array a b -> [b]
primitive primSubscript "primSubscript" :: (a -> Int) -> Array a b -> a -> b
primitive primAmap      "primAmap"	:: (b -> c) -> Array a b -> Array a c

array bounds assocs = primArray (index bounds) bounds assocs
listArray b vs	    = array b (zipWith (:=) (range b) vs)
(!) a               = primSubscript (index (bounds a)) a 
bounds              = primBounds
indices		    = range . bounds
elems               = primElems
assocs a            = zipWith (:=) (indices a) (elems a)
accumArray f z b    = primAccumArray (index b) f z b
a // as             = primUpdate (index (bounds a)) a as
accum f a           = primAccum (index (bounds a)) f a
amap                = primAmap
ixmap b f a         = array b [ i := (a ! f i) | i <- range b ]

instance (Ix a, Ix b) => Ix (a,b) where
    range ((l,l'),(u,u'))
       = [ (i,i') | i <- range (l,u), i' <- range (l',u') ]
    index ((l,l'),(u,u')) (i,i')
       = index (l,u) i * rangeSize (l',u') + index (l',u') i'
    inRange ((l,l'),(u,u')) (i,i')
       = inRange (l,u) i && inRange (l',u') i'

rangeSize        :: (Ix a) => (a,a) -> Int
rangeSize r@(l,u) = index r u + 1

-------------------------------------------------------------------------------
