-- ccexamples.gs				        Mark P. Jones, 1992
--
-- This file contains a range of examples using the system of constructor
-- classes implemented in Gofer 2.28.  You will need to start Gofer running
-- with the cc.prelude to use this file.
--

-- Constructor class examples: ----------------------------------------------

class Functor2 f where
     map2 :: (a -> b) -> (c -> d) -> (f a c -> f b d)

-- The identity monad (well nearly): ----------------------------------------

data Id a = Id a

instance Functor Id where map f (Id x)  = Id (f x)
instance Monad   Id where result        = Id
			  join (Id x)   = x
			  Id x `bind` f = f x

-- The `Maybe' datatype: ----------------------------------------------------

data Maybe a = Just a | Nothing

instance Functor Maybe where
    map f (Just x) = Just (f x)
    map f Nothing  = Nothing

instance Monad Maybe where
    result x         = Just x
    Just x  `bind` f = f x
    Nothing `bind` f = Nothing

instance Monad0 Maybe where
    zero = Nothing

instance MonadPlus Maybe where
    Nothing ++ y = y
    x       ++ y = x

trap              :: Maybe a -> a -> a
Just x  `trap` def = x
Nothing `trap` def = def

listToMaybe	  :: [a] -> Maybe a		-- a monad homomorphism
listToMaybe        = concat . map result

-- Error monads --------------------------------------------------------------

class Monad m => ErrorMonad m where	-- a class of monads for describing
    fail :: String -> m a		-- computations that might go wrong

data Error a = Done a | Err String	-- a variation on the maybe type

instance Functor Error where		-- which is a Functor,
    map f (Done x) = Done (f x)
    map f (Err s)  = Err s

instance Monad Error where		-- a Monad,
    result           = Done
    Done x  `bind` f = f x
    Err msg `bind` f = Err msg

instance ErrorMonad Error where		-- and an ErrorMonad ...
    fail = Err

-- Parser monad: ------------------------------------------------------------

type Parser token value = [token] -> [(value,[token])]
 in mapP, resultP, joinP, bindP, zeroP, orP, sat, tok, toks, spaces, parse

mapP        :: (a -> b) -> Parser t a -> Parser t b
mapP f p     = \s -> [ (f x, s') | (x,s') <- p s ]

resultP     :: a -> Parser t a
resultP v    = \s -> [(v,s)]

joinP       :: Parser t (Parser t a) -> Parser t a
joinP pp     = \s -> [ (x,s'') | (p,s') <- pp s, (x,s'') <- p s' ]

bindP       :: Parser t a -> (a -> Parser t b) -> Parser t b
p `bindP` f  = \s -> [ (a',s'') | (a,s') <- p s, (a',s'') <- f a s' ]

zeroP       :: Parser t a
zeroP        = \s -> []

orP         :: Parser t a -> Parser t a -> Parser t a
p `orP` q    = \s -> p s ++ q s

sat         :: (t -> Bool) -> Parser t t
sat p []     = []
sat p (h:ts) = [ (h,ts) | p h ]

tok	    :: Eq t => t -> Parser t t
tok t        = sat (t==)

toks        :: Eq [t] => [t] -> Parser t ()
toks w       = \ts -> [ ((),drop n ts) | w == take n ts ]
	       where n = length w

spaces	    :: Parser Char a -> Parser Char a
spaces p     = p . dropWhile isSpace

parse       :: Parser t a -> [t] -> Maybe a
parse p ts   = listToMaybe [ x | (x,[]) <- p ts ]

instance Functor   (Parser t) where map    = mapP
instance Monad     (Parser t) where result = resultP
				    bind   = bindP
				    join   = joinP
instance Monad0    (Parser t) where zero   = zeroP
instance MonadPlus (Parser t) where (++)   = orP

-- Continuation monad: ------------------------------------------------------

type Cont r a = (a -> r) -> r
  in mapC, resultC, joinC, bindC, callcc

mapC       :: (a -> b) -> Cont r a -> Cont r b
mapC f m    = \k -> m (k . f)

resultC    :: a -> Cont r a
resultC x   = \k -> k x

joinC      :: Cont r (Cont r a) -> Cont r a
joinC m     = \k -> m (\x -> x k)

bindC      :: Cont r a -> (a -> Cont r b) -> Cont r b
m `bindC` f = \k -> m (\y -> (f y) k)

callcc     :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callcc g    = \k -> g (\x k' -> k x) k

instance Functor (Cont r) where map    = mapC

instance Monad   (Cont r) where result = resultC
				bind   = bindC
				join   = joinC

-- State monads: ------------------------------------------------------------

class Monad (m s) => StateMonad m s where
    update :: (s -> s) -> m s s		-- the principal characteristic of a
    set    :: s -> m s s		-- state based compuation is that you
    fetch  :: m s s			-- can update the state!
    set new = update (\old -> new)
    fetch   = update id

incr :: StateMonad m Int => m Int Int
incr  = update (1+)

random  :: StateMonad m Int => Int -> m Int Int
random n = update min_stand_test `bind` \m ->
           result (m `mod` n)

min_stand_test  :: Int -> Int       -- see demos/minsrand.gs for explanation
min_stand_test n = if test > 0 then test else test + 2147483647
		   where test = 16807 * lo - 2836 * hi
		         hi   = n `div` 127773
		         lo   = n `rem` 127773

data State s a = ST (s -> (a,s))	-- The standard example: state
					-- transformers (not used in the rest
					-- of this program).
instance Functor (State s) where
    map f (ST st) = ST (\s -> let (x,s') = st s in (f x, s'))

instance Monad (State s) where
    result x      = ST (\s -> (x,s))
    ST m `bind` f = ST (\s -> let (x,s') = m s
                                  ST f'  = f x
                              in  f' s')

instance StateMonad State s where
    update f = ST (\s -> (s, f s))

ST m `startingWith` s0 = result where (result,_) = m s0

data STM m s a = STM (s -> m (a,s))	-- a more sophisticated example,
					-- where the state monad is
					-- parameterised by a second,
					-- arbitrary monad.

instance Monad m => Functor (STM m s) where
    map f (STM xs) = STM (\s -> [ (f x, s') | ~(x,s') <- xs s ])

instance Monad m => Monad (STM m s) where
    result x        = STM (\s -> result (x,s))
    join (STM xss)  = STM (\s -> [ (x,s'') | ~(STM xs, s') <- xss s,
                                             ~(x,s'') <- xs s' ])
    STM xs `bind` f = STM (\s -> xs s `bind` (\(x,s') ->
                                 let STM f' = f x
                                 in  f' s'))

instance ErrorMonad m => ErrorMonad (STM m s) where
    fail msg = STM (\s -> fail msg)

instance StateMonad (STM m) s where
    update f = STM (\s -> result (s, f s))

protect          :: Monad m => m a -> STM m s a
protect m         = STM (\s -> [ (x,s) | x<-m ])

execute          :: Monad m => s -> STM m s a -> m a
execute s (STM f) = [ x | ~(x,s') <- f s ]

-- Reader monad: ------------------------------------------------------------
-- I imagine there must be some deep philosophical reason why the following
-- functions turn out to be very well-known combinators?
-----------------------------------------------------------------------------

type Reader r a = r -> a
  in mapR, resultR, bindR, joinR, read, readOnly

mapR       :: (a -> b) -> (Reader r a -> Reader r b)
mapR f m    = f . m						-- B

resultR    :: a -> Reader r a
resultR x   = \r -> x						-- K

joinR      :: Reader r (Reader r a) -> Reader r a
joinR mm    = \r -> mm r r					-- W?

bindR      :: Reader r a -> (a -> Reader r b) -> Reader r b
x `bindR` f = \r -> f (x r) r					-- S

read       :: Reader r r
read r      = r

readOnly   :: Reader s a -> State s a
readOnly m  = ST (\s -> (m s, s))

instance Functor (Reader r) where map    = mapR

instance Monad   (Reader r) where result = resultR
				  bind   = bindR
				  join   = joinR

-- Output monad: ------------------------------------------------------------

type Output a = (a, ShowS)
  in mapO, resultO, bindO, joinO, write

mapO		  :: (a -> b) -> Output a -> Output b
mapO f (x, ss)	   = (f x, ss)

resultO		  :: a -> Output a
resultO x	   = (x, id)

bindO		  :: Output a -> (a -> Output b) -> Output b
(a, ss) `bindO` f  = let (b, ss') = f a in (b, ss . ss')

joinO             :: Output (Output a) -> Output a
joinO ((m,ss'),ss) = (m, ss . ss')

write             :: String -> Output ()
write msg          = ((), (++) msg)

instance Functor Output where map    = mapO

instance Monad   Output where result = resultO
			      bind   = bindO
			      join   = joinO

-- Association lists ---------------------------------------------------------

type Assoc v t = [(v,t)] in mapAssoc, noAssoc, extend, lookup

instance Functor (Assoc v) where map = mapAssoc

mapAssoc      :: (a -> b) -> (Assoc v a -> Assoc v b)
mapAssoc f vts = [ (v, f t) | (v,t) <- vts ]

noAssoc       :: Assoc v t
noAssoc        = []

extend        :: v -> t -> Assoc v t -> Assoc v t
extend v t a   = [(v,t)] ++ a

lookup        :: (Eq v, ErrorMonad m) => v -> Assoc v t -> m t
lookup v       = foldr find (fail "Undefined value")
                 where find (w,t) alt | w==v      = result t
                                      | otherwise = alt

-- Types: -------------------------------------------------------------------

data Type v = TVar v			-- Type variable
            | Fun (Type v) (Type v)	-- Function type

instance Text v => Text (Type v) where
    showsPrec p (TVar v)          = shows v
    showsPrec p (Fun (TVar v) r)  = shows v . showString " -> " . shows r
    showsPrec p (Fun l r)         = showChar '(' . shows l . showChar ')'
                                    . showString " -> "
                                    . shows r

instance Functor Type where  map f (TVar v)   = TVar (f v)
                             map f (Fun d r)  = Fun (map f d) (map f r)
instance Monad   Type where  result v         = TVar v
                             TVar v  `bind` f = f v
                             Fun d r `bind` f = Fun (d `bind` f) (r `bind` f)

vars           :: Type v -> [v]
vars (TVar v)   = [v]
vars (Fun d r)  = vars d ++ vars r

-- Substitutions: -----------------------------------------------------------

type Subst m v = v -> m v

nullSubst  :: Monad m => Subst m v
nullSubst   = result

(>>)       :: (Eq v, Monad m) => v -> m v -> Subst m v
(v >> t) w  = if v==w then t else result w

varBind v t = if (v `elem` vars t) then fail "unification fails"
				   else result (v>>t)

unify (TVar v)  (TVar w)
              | v==w      = result nullSubst
              | otherwise = result (v>>TVar w)
unify (TVar v)  t         = varBind v t
unify t         (TVar v)  = varBind v t
unify (Fun d r) (Fun e s) = [ s2 @@ s1 | s1 <- unify d e,
					 s2 <- unify (apply s1 r)
						     (apply s1 s) ]

-- Terms: --------------------------------------------------------------------

data Term v = Var v                   -- variable
            | Ap  (Term v) (Term v)   -- application
            | Lam v (Term v)          -- lambda abstraction

examples = [ lamx x,				-- identity
             k,					-- k
             s,					-- s
             lamx (lamy (lamz (Ap x (Ap y z)))),-- b
             lamx (Ap x x),			-- \x. x x
	     Ap (Ap s k) k,			-- s k k
             Ap (Ap s (Ap k s)) k,		-- s (k s) k
             x					-- unbound x
           ]
           where s    = lamx (lamy (lamz (Ap (Ap x z) (Ap y z))))
                 k    = lamx (lamy x)
                 x    = Var "x"
                 y    = Var "y"
                 z    = Var "z"
                 lamx = Lam "x"
                 lamy = Lam "y"
                 lamz = Lam "z"

-- Type inference: -----------------------------------------------------------

type Infer a = STM Error Int a
type Expr    = Term String
type Assume  = Assoc String (Type Int)

infer            :: Assume -> Expr -> Infer (Subst Type Int, Type Int)
infer a (Var v)   = lookup v a                           `bind` \t      ->
                    result (nullSubst,t)
infer a (Lam v e) = newVar                               `bind` \b      ->
                    infer (extend v (TVar b) a) e        `bind` \(s,t)  ->
                    result (s, s b `Fun` t)
infer a (Ap l r)  = infer a l                            `bind` \(s,lt) ->
                    infer (map (apply s) a) r            `bind` \(t,rt) ->
                    newVar                               `bind` \b      ->
                    unify (apply t lt) (rt `Fun` TVar b) `bind` \u      ->
                    result (u @@ t @@ s, u b)

newVar :: Infer Int
newVar  = incr

try    = layn (map (show' . typeOf) examples)

typeOf = map (show.snd) . execute 0 . infer noAssoc

-- Now for something rather different: Trees: -------------------------------

class Functor t => TreeCon t where	 -- tree constructors
    branches :: t a -> [t a]

-- standard calculations involving trees

depth :: TreeCon t => t a -> Int
depth  = (1+) . foldl max 0 . map depth . branches

dfs   :: TreeCon t => t a -> [t a]
dfs t  = t : concat (map dfs (branches t))

bfs   :: TreeCon t => t a -> [t a]
bfs    = concat . lev
 where lev t = [t] : foldr cat [] (map lev (branches t))
       cat   = longzw (++)

longzw f (x:xs) (y:ys) = f x y : longzw f xs ys
longzw f []     ys     = ys
longzw f xs     []     = xs

paths t | null br   = [ [t] ]
        | otherwise = [ t:p | b<-br, p<-paths b ]
          where br = branches t

-- now here are a variety of trees, all of which are instances of
-- the TreeCon class above:

data Tree a  =  Leaf a  |  Tree a :^: Tree a

instance Functor Tree where		-- `context free relabeling'
    map f (Leaf a)  = Leaf (f a)
    map f (l :^: r) = map f l :^: map f r

instance Monad Tree where		-- `substitution'
    result             = Leaf
    Leaf x    `bind` f = f x
    (l :^: r) `bind` f = (l `bind` f) :^: (r `bind` f)

instance TreeCon Tree where		-- the tree structure
    branches (Leaf n)  = []
    branches (l :^: r) = [l,r]

data LabTree l a  =  Tip a  |  LFork l (LabTree l a) (LabTree l a)

instance Functor (LabTree l) where
    map f (Tip x)       = Tip (f x)
    map f (LFork x l r) = LFork x (map f l) (map f r)

instance Monad (LabTree l) where
    result               = Tip
    Tip x       `bind` f = f x
    LFork x l r `bind` f = LFork x (l `bind` f) (r `bind` f)

instance TreeCon (LabTree l) where
   branches (Tip x)       = []
   branches (LFork x l r) = [l,r]

data STree a  =  Empty  | Split a (STree a) (STree a)

instance Functor STree where
    map f Empty         = Empty
    map f (Split x l r) = Split (f x) (map f l) (map f r)

instance TreeCon STree where
    branches Empty         = []
    branches (Split x l r) = [l,r]

data GenTree a =  Node a [GenTree a]

instance Functor GenTree where
    map f (Node x gts) = Node (f x) (map (map f) gts)

instance TreeCon GenTree where
    branches (Node x gts) = gts

-- The tree labeling program: -----------------------------------------------

label     :: Tree a -> Tree (a,Int)		-- error prone explicit
label tree = fst (lab tree 0)			-- counters
 where lab (Leaf n)  c  =  (Leaf (n,c), c+1)
       lab (l :^: r) c  =  (l' :^: r', c'')
                           where (l',c')  = lab l c
                                 (r',c'') = lab r c'

label1     :: Tree a -> Tree (a,Int)		-- monad version
label1 tree = lab tree `startingWith` 0
 where lab (Leaf n)  = incr                  `bind` \c ->
                       result (Leaf (n,c))
       lab (l :^: r) = lab l                 `bind` \l' ->
                       lab r                 `bind` \r' ->
                       result (l' :^: r')

label2     :: Tree a -> Tree (a,Int)		-- using monad comprehensions
label2 tree = lab tree `startingWith` 0
 where lab (Leaf n)  = [ Leaf (n,c) | c <- incr ]
       lab (l :^: r) = [  l :^: r   | l <- lab l, r <- lab r ]

-- A `while loop' for an arbitrary monad: -----------------------------------

while    :: Monad m => m Bool -> m b -> m ()
while c s = c  `bind` \b ->
            if b then s         `bind` \x ->
                      while c s
                 else result ()

skip :: Monad m => m ()
skip  = result ()

loop  = while isDot skip

isDot = [ True | x <- sat ('.'==) ]

-- End of program -----------------------------------------------------------
