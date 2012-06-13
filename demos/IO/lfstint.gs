-- Here is a version of the interpreter with lazy stream output that
-- is described in the extended version of:
--
--   `Lazy Functional State Threads'
--   John Launchbury and Simon Peyton Jones
--   (short version is in PLDI '94).
--
-- This program requires array.gs, iomonad.gs, and ioarray.gs to run.
-- For example, in the demos/IO directory, try:
--
--   :load ../../array.gs ../../iomonad.gs ../../ioarray.gs lfstint.gs
--

data Com = Assign Var Exp | Read Var | Write Exp | While Exp [Com]
type Var = Char
data Exp = Variable Var | Const Int | Plus Exp Exp | Eq Exp Exp | Le Exp Exp

interpret :: [Com] -> [Int] -> [Int]
interpret cs input = runST (newArr ('a', 'z') 0 `thenST` \store ->
                            newVar input        `thenST` \inp   ->
                            command cs store inp)

type Store s = MutArr s Var

command :: [Com] -> Store s Int -> MutVar s [Int] -> ST s [Int]
command cs store inp = obey cs
 where
  -- obey :: [Com] -> ST s [Int]
  obey []                = returnST []
  obey (Assign v e : cs) = eval e              `thenST` \a ->
                           writeArr store v a  `thenST_`
                           obey cs
  obey (Read v     : cs) = readVar inp         `thenST` \(x:xs) ->
                           writeArr store v x  `thenST_`
                           writeVar inp xs     `thenST_`
                           obey cs
  obey (Write e    : cs) = eval e              `thenST` \out ->
                           obey cs             `thenST` \outs ->
                           returnST (out:outs)
  obey (While e bs : cs) = eval e              `thenST` \val ->
                           if val==0 then
                              obey cs
                           else
                              obey (bs ++ While e bs : cs)

  -- eval :: Exp -> ST s Int
  eval (Variable v) = readArr store v
  eval (Const n)    = returnST n
  eval (Plus l r)   = binary (+) l r
  eval (Eq l r)     = binary (\x y -> if x==y then 1 else 0) l r
  eval (Le l r)     = binary (\x y -> if x<=y then 1 else 0) l r

  binary f l r      = eval l                   `thenST` \l' ->
                      eval r                   `thenST` \r' ->
                      returnST (f l' r')

-- Some sample programs:

prog1 = [ Write (Const 1),
          While (Const 1) [],
          Write (Const 2) ]

prog2 = [ Assign 'a' (Const 1),
          While (Le (Variable 'a') (Const 10))
             [ Write (Variable 'a'),
               Assign 'a' (Plus (Variable 'a') (Const 1))
             ]
        ]

prog3 = [ Assign 'a' (Const 0),
          While (Const 1)
             [ Write (Variable 'a'),
               Assign 'a' (Plus (Variable 'a') (Const 1))
             ]
        ]

prog4 = [ While (Const 1)
             [ Read 'a',
               Write (Plus (Variable 'a') (Const 1))
             ]
        ]

prog5 = [ Read 'a',
          While (Variable 'a')
             [ Write (Plus (Variable 'a') (Const 1)),
               Read 'a'
             ]
        ]

prog6 = [ Assign 't' (Const 0),
          Assign 'n' (Const 0),
          Read 'a',
          While (Variable 'a')
             [ Assign 't' (Plus (Variable 't') (Variable 'a')),
               Assign 'n' (Plus (Variable 'n') (Const 1)),
               Read 'a'
             ],
          Write (Variable 't'),
          Write (Variable 'n')
        ]

test = interpret prog6 ([1..10] ++ [0])

------------------------------------------------------------------------------
