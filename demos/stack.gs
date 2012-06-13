-- Stacks: using restricted type synonyms

type Stack a = [a] in emptyStack, push, pop, topOf, isEmpty

emptyStack :: Stack a
emptyStack  = []

push       :: a -> Stack a -> Stack a
push        = (:)

pop        :: Stack a -> Stack a
pop []      = error "pop: empty stack"
pop (_:xs)  = xs

topOf      :: Stack a -> a
topOf []    = error "topOf: empty stack"
topOf (x:_) = x

isEmpty    :: Stack a -> Bool
isEmpty     = null

instance Eq a => Eq (Stack a) where
    s1 == s2 | isEmpty s1 = isEmpty s2
             | isEmpty s2 = isEmpty s1
             | otherwise  = topOf s1 == topOf s2 && pop s1 == pop s2

-- A slightly different presentation:

type Stack' a = [a] in
   emptyStack' :: Stack' a,
   push'       :: a -> Stack' a -> Stack' a,
   pop'        :: Stack' a -> Stack' a,
   topOf'      :: Stack' a -> a,
   isEmpty'    :: Stack' a -> Bool

emptyStack'  = []

push'        = (:)

pop' []      = error "pop': empty stack"
pop' (_:xs)  = xs

topOf' []    = error "topOf': empty stack"
topOf' (x:_) = x

isEmpty'     = null

instance Eq a => Eq (Stack' a) where
    s1 == s2 | isEmpty' s1 = isEmpty' s2
             | isEmpty' s2 = isEmpty' s1
             | otherwise   = topOf' s1 == topOf' s2 && pop' s1 == pop' s2

