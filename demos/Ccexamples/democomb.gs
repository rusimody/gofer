-- A demonstration of the use of a composition of monads to implement an
-- `imperative' version of the fibonacci numbers program.
--
-- Original code by Luc Dupocheel, with small changes to allow the code to
-- be used with the standard Gofer 2.28 cc.prelude.
-- 

-- begin --------------------------------------------------------------------

infix  1 :=:, =:
infixr 0 :&:, &

-- expressions --------------------------------------------------------------

type Value = Int
type Name  = String

data Expression = Con Int | Var String | Expression :+: Expression

-- evaluating expressions ---------------------------------------------------

type Rom = [(Name,Value)]

expr1 :: Expression -> Read Rom Value
expr1 (Con v) = result v
expr1 (Var n) = lookup n
expr1 (e:+:f) = [ x+y | x <- expr1 e, y <- expr1 f ]

-- evaluating expressions reused --------------------------------------------

type Ram = Int

expr2 :: Expression -> Composition (Read Rom) (State Ram) Value
expr2 (Con v) =      plug (expr1 (Con v))
expr2 (Var n) = join [ plug (doS (+1) v) | v <- plug (expr1 (Var n))]
expr2 (e:+:f) =      [ x+y | x <- expr2 e, y <- expr2 f ]
 

-- programs ------------------------------------------------------------------

data Program = Name :=: Expression | Out Expression | Program :&: Program | Forever Program

-- interpreting programs -----------------------------------------------------

type Memory = Rom
type Output = [Value]

(=:) :: Name -> Value -> Memory -> Memory
n =: v = ((n,v) :)

out :: Value -> Output -> Output 
out v = (v :)

(&) :: () -> () -> ()
() & () = ()

prg1 :: Program -> Cont (Memory -> Output) ()
prg1 (n:=:e)     = join [ plug (doS (x=:v) ()) | x <- result n,
                                                 v <- plug (expr1 e)]
prg1 (Out e)     = join [ doC (out v) | v <- plug (expr1 e) ]
prg1 (p:&:q)     =      [ u&v | u <- prg1 p , v <- prg1 q] 
prg1 (Forever p) =      [ v&w | v <- prg1 p, w <- prg1 (Forever p)]
  
-- expressions : an example

rom :: Rom
rom = [("n",3),("m",4)]

expression :: Expression
expression = ((Con 1 :+: Var "n") :+: ((Var "m" :+: Con 2) :+: Var "n"))

showV :: Value -> String -> String
showV v = showString "value : " . shows v . showChar '\n'

instance Text (Read Rom Value) where 
	showsPrec p (Rd f) = let v = f rom in showV v . showChar '\n' 

ex1 = show (expr1 expression)

-- expressions with state : an example

ram0 :: Ram
ram0 = 0

showR :: Ram -> String -> String
showR r = showString "ram   : " . shows r . showChar '\n'

instance Text (Composition (Read Rom) (State Ram) Value) where 
	showsPrec p (Comp f) = let (v,r) = tS (dR f rom) ram0
                               in  showV v . showR r . showChar '\n'
                               
ex2 = show (expr2 expression)

-- programs an example --------------------------------------------------------

mem0 :: Memory
mem0 = []

cont0 :: () -> Memory -> Output
cont0 () mem = []

instance Text (Cont (Memory -> Output) ()) where
	showsPrec p (Cnt f) = fold showV (f cont0 mem0) . showChar '\n'

output :: Name -> Program
output = Out . Var

program :: Program
program = "x" :=: Con 1                :&: 
          "y" :=: Con 1                :&: 
          output "x"                   :&:
          output "y"                   :&: 
          Forever (
          "z" :=: Var "x" :+: Var "y"  :&:
          "x" :=: Var "y"              :&:
          "y" :=: Var "z"              :&: 
          output "z"      
          )
 
main :: Dialogue
main = appendChan stdout fibs exit done 
       where fibs = show (prg1 program)


-- Monad morphisms, algebras and composition: --------------------------------

class (Monad m, Monad n) => MonadMorphism m n where
    plug :: m a -> n a

class Monad m => Algebra m a where
    bindA      :: m x -> (x -> a) -> a
    applyA     :: (x -> a) -> m x -> a
    (##)       :: (x -> m y) -> (y -> a) -> x -> a
    joinA      :: m a -> a
 
    applyA      = flip bindA
    g ## f      = (`bindA` f) . g
    joinA       = (`bindA` id)
    x `bindA` f = joinA (map f x) 

instance Monad m => Algebra m (m x) where
    bindA = bind

class (Monad m, Monad n) => ComposableMonads m n where
    prod  :: n (m (n x)) -> m (n x)
    swap  :: n (m x) -> m (n x)

    prod   = map join . swap
    swap   = prod . map (map result)
 
bindC :: ComposableMonads m n => n x -> (x -> m (n y)) -> m (n y)
x `bindC` f = prod (map f x)

applyC :: ComposableMonads m n => (x -> m (n y)) -> n x -> m (n y)
applyC = flip bindC

applyC' :: ComposableMonads m n => (n x -> m y) -> n (m x) -> m y
applyC' f = apply f . swap

(**) :: ComposableMonads m n => (x -> n y) -> (y -> m (n z)) -> x -> m (n z)
g ** f = (`bindC` f) . g

mapC :: ComposableMonads m n => (x -> m y) -> n x -> m (n y)
mapC f  = swap . map f

-- I prefer to use 'data' instead of 'type' for Composition
-- This avoids the need for restricted type synonyms 
-- but it complicates things a bit (pmoC is needed ...)

data Composition m n a = Comp (m (n a))

pmoC (Comp x) = x

instance (Functor f, Functor g) => Functor (Composition f g) where
	map f = Comp . (map . map) f . pmoC

instance ComposableMonads m n => Monad (Composition m n) where
	result = Comp . (result . result)
	join   = Comp . join . map (prod . map pmoC) . pmoC

-- this would give an overlap

--instance MonadMorphism n (Composition m n) where
--	plug = Comp . result 

--instance MonadMorphism m (Composition m n) where
--	plug = Comp . map result

instance (Monad (Composition m l), Monad (Composition r s),
          MonadMorphism m r, MonadMorphism l s) =>
         MonadMorphism (Composition m l) (Composition r s) where 
	plug = Comp . map plug . plug . pmoC

mfold f = foldr (@@) result . map f
fold  f = foldr (.) id      . map f

-- The read monad: -----------------------------------------------------------

data Read r x = Rd (r -> x)
dR (Rd f) = f

instance Functor (Read r) where
	map f (Rd g) = Rd (\r -> let x = g r in f x)

instance Monad (Read r) where
	result x        = Rd (\r -> x)
	(Rd g) `bind` f = Rd (\r -> let x = g r in dR (f x) r)  	

lookup :: Eq b => b -> Read [(b,a)] a
lookup x = Rd f where f ((y,v):bs) | x == y    = v
                                   | otherwise = f bs

-- The state monad: ----------------------------------------------------------

data State s x = St (s -> (x,s))
tS (St f) = f

instance Functor (State s) where
	map f (St g) = St (\s -> let (x,t) = g s in (f x,t))

instance Monad (State s) where
	result x        = St (\s -> (x,s))
	(St g) `bind` f = St (\s -> let (x,t) = g s in tS (f x) t) 

doS       :: (s -> s) -> x -> State s x 
doS f x    = St g where g s = (x, f s)  

-- The continuation monad: ---------------------------------------------------

data Cont a x = Cnt ((x -> a) -> a)
tnC (Cnt f) = f

instance Functor (Cont a) where
	map f (Cnt g) = Cnt (\c -> g (c . f))

instance Monad (Cont a) where
	result x         = Cnt (\c -> c x)
	(Cnt g) `bind` f = Cnt (\c -> g (\x -> tnC (f x) c))

doC :: (a -> a) -> Cont (s -> a) ()
doC f = Cnt (\c s -> f (c () s))

instance MonadMorphism (State s) (Cont (s -> a)) where
	plug (St g) = Cnt (\c s -> let (x,t) = g s in c x t)

instance MonadMorphism (Read r) (Cont (r -> a)) where
	plug (Rd g) = Cnt (\c r -> let x = g r in c x r)

-- The read-state monad: -----------------------------------------------------

instance ComposableMonads (Read r) (State s) where
	swap mf = Rd (\x -> [ dR m x | m <- mf ]) 

-- needed explicitly because of overlap in general case

instance MonadMorphism (State s) (Composition (Read r) (State s)) where
	plug = Comp . result 

instance MonadMorphism (Read r) (Composition (Read r) (State s)) where
	plug = Comp . map result

-- end -----------------------------------------------------------------------
