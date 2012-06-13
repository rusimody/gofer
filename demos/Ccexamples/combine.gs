-- combine.gs				Mark P Jones  December 1992
--
-- The following script resulted from a discussion between myself
-- and Luc Duponcheel on the comp.lang.functional newsgroup in
-- December 1992.
--
-- This code fragment shows how the composition of monads can be
-- described in Gofer using constructor classes.

class (Monad m, Monad l) => Composable m l where
    prod  :: l (m (l a)) -> m (l a)
    swap  :: l (m a) -> m (l a)
    app   :: (l a -> m b) -> l (m a) -> m b

    prod  = app (result . join)
    swap  = prod . map (map result)
    app f = join . map f . swap

mmap f = swap . map f

instance Composable m [ ] where
    swap []     = [ [] ]
    swap (x:xs) = [ y:ys | y<-x, ys<-swap xs ]

type Comp f g a = f (g a) in mapComp, resultComp, joinComp

mapComp :: (Functor f, Functor g) => (a -> b) -> (Comp f g a -> Comp f g b)
mapComp  = map . map

instance (Functor f, Functor g) => Functor (Comp f g) where
    map = mapComp

resultComp :: (Monad f, Monad g) => a -> Comp f g a
resultComp  = result . result

joinComp   :: (Composable f g) => Comp f g (Comp f g a) -> Comp f g a
joinComp    = join . map prod

instance Composable f g => Monad (Comp f g) where
    result = resultComp
    join   = joinComp

