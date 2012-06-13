-------------------------------------------------------------------------------
-- Using constructor classes to model concepts from category theory.  A more
-- general approach than that provided by the standard constructor classes
-- prelude cc.prelude, acknowledging the fact that not everybody works in the
-- same category all the time ...
--
-- [Use the standard.prelude to load this file in Gofer 2.28]
--
-- Mark P. Jones, March 1993
-------------------------------------------------------------------------------

-- Categories: ----------------------------------------------------------------

class Cat fn where
    identity :: fn a a
    compose  :: fn b c -> fn a b -> fn a c

instance Cat (->) where
    identity x    = x
    compose f g x = f (g x)


-- In general, functors can go between different categories: ------------------

class (Cat fn, Cat fn') => Functor fn fn' f where
    functor :: fn a b -> fn' (f a) (f b)

instance Functor (->) (->) [] where
    functor f []     = []
    functor f (x:xs) = f x : functor f xs

data Tree a  =  Leaf a  |  Tree a :^: Tree a

instance Functor (->) (->) Tree where
    functor f (Leaf x)  = Leaf (f x)
    functor f (l :^: r) = functor f l :^: functor f r


-- An endofunctor has the same source and target categories: ------------------

class Functor fn fn f => EndoFunctor fn f
instance Functor fn fn f => EndoFunctor fn f


-- Monads are built on top of endofunctors: -----------------------------------

class EndoFunctor fn m => Monad fn m where
    result :: fn a (m a)
    join   :: fn (m (m a)) (m a)
    
instance Monad (->) [] where
    result x = [x]
    join     = foldr (++) []


-- Kleisli categories: --------------------------------------------------------

type Kleisli fn m a b = fn a (m b)
  in kleisliId     :: Monad fn m => Kleisli fn m a a,
     kleisliComp   :: Monad fn m => Kleisli fn m b c ->
                                        Kleisli fn m a b -> Kleisli fn m a c,
     kleisli       :: fn a (m b) -> Kleisli fn m a b,
     kleisliMap    :: Monad fn m => Kleisli fn m a b -> fn (m a) (m b),
     kleisliUniv   :: Monad fn m => fn a (m b) -> Kleisli fn m (Id a) b,
     kleisliCouniv :: Monad fn m => Kleisli fn m (Id a) b -> fn a (m b),
     idKleisli

kleisliId       = result
kleisliComp f g = compose join (compose (functor f) g)
kleisli f       = f
kleisliMap f    = compose join (functor f)
kleisliUniv     = id
kleisliCouniv   = id

instance Monad fn m => Cat (Kleisli fn m) where
    identity = kleisliId
    compose  = kleisliComp

instance Monad fn m => Functor (Kleisli fn m) fn m where
    functor = kleisliMap


-- The identity functor: ------------------------------------------------------

type Id x = x in idFunctor :: fn a b -> fn (Id a) (Id b),
                 idResult  :: Cat fn => fn a (Id a),
                 idJoin    :: Cat fn => fn (Id (Id a)) (Id a),
                 idKleisli :: Monad fn m => fn a b ->Kleisli fn m (Id a) (Id b),
		 kleisliUniv, kleisliCouniv

idFunctor = id
idResult  = identity
idJoin    = identity
idKleisli = compose result

instance Functor fn fn Id where
    functor = idFunctor

instance Monad fn Id where
    result  = idResult
    join    = idJoin

instance Monad fn m => Functor fn (Kleisli fn m) Id where
    functor = idKleisli


-- Natural transformations: ---------------------------------------------------

{- You'd think this was easy.  But uncomment the following and you'll find
   that the obvious definition gives an ambiguous type for eta.  It's not
   obvious whether you can genuinely express naturality in this framework.

   Note that these definitions work if you restrict attention to single
   category as in cc.prelude.

class (Functor c d f, Functor c d g) => NatTransf c d f g where
    eta :: f a -> g a

instance NatTransf (->) (->) Tree [] where
    eta (Leaf x)  = [x]
    eta (l :^: r) = eta l ++ eta r
-}


-- Since we've come this far, let's (try to) code up adjunctions too: ---------

class (Functor ca cb left, Functor cb ca right) =>
      Adjoint ca cb left right where
    univ   :: ca a (right b) -> cb (left a) b
    couniv :: cb (left a) b -> ca a (right b)

    -- ideally, we'd also like to include the unit and counit of an adjunction
    -- in this definition.  These can be defined by:
    --     unit   = couniv identity
    --     counit = univ identit
    -- but, once again, the types for these turn out to be ambiguous; they
    -- only determine the category in which the unit or counit (resp) lies,
    -- not the intermediate category.

-- the well-know categorical construction of an adjunction from a monad: ------

instance Monad fn m => Adjoint fn (Kleisli fn m) Id m where
    univ   = kleisliUniv
    couniv = kleisliCouniv

-------------------------------------------------------------------------------
