-----------------------------------------------------------------------------
-- Parsing simple arithmetic expressions using combinators in Gofer
--
-- Mark P. Jones, April 4, 1993

infixr 6 &&&
infixl 5 >>>
infixr 4 |||

type Parser a = String -> [(a,String)]

result       :: a -> Parser a
result x s    = [(x,s)]

(|||)        :: Parser a -> Parser a -> Parser a
(p ||| q) s   = p s ++ q s

(&&&)        :: Parser a -> Parser b -> Parser (a,b)
(p &&& q) s   = [ ((x,y),s1) | (x,s0) <- p s, (y,s1) <- q s0 ]

(>>>)        :: Parser a -> (a -> b) -> Parser b
(p >>> f) s   = [ (f x, s0) | (x,s0) <- p s ]

many         :: Parser a -> Parser [a]
many p        = q where q = p &&& q >>> (\(x,xs) -> x:xs)
                            |||
                            result []

many1        :: Parser a -> Parser [a]
many1 p       = p &&& many p >>> (\(x,xs) -> x:xs)

sat          :: (Char -> Bool) -> Parser Char
sat p (c:cs)
        | p c = [ (c,cs) ]
sat p cs      = []

tok          :: String -> Parser String
tok s cs      = loop s cs
                where loop ""     cs            = [(s,cs)]
                      loop (s:ss) (c:cs) | s==c = loop ss cs
                      loop _      _             = []

digit        :: Parser Int
digit         = sat isDigit >>> \d -> ord d - ord '0'

number       :: Parser Int
number        = many1 digit >>> foldl (\a x -> 10*a+x) 0

-- Original version:
-- eval "1"          (540 reductions, 933 cells)
-- eval "(1)"        (5555 reductions, 8832 cells)
-- eval "((1))"      (50587 reductions, 80354 cells, 1 garbage collection)
-- eval "(((1)))"    (455907 reductions, 724061 cells, 7 garbage collections)
-- eval "1+2+3+4+5"  (1296 reductions, 2185 cells)
-- eval "1+"         (828 reductions, 1227 cells)

{-
expr   = term &&& tok "+" &&& expr >>> (\(x,(p,y)) -> x + y)  |||
         term &&& tok "-" &&& expr >>> (\(x,(m,y)) -> x - y)  |||
         term

term   = atom &&& tok "*" &&& term >>> (\(x,(t,y)) -> x * y)  |||
         atom &&& tok "/" &&& term >>> (\(x,(d,y)) -> x / y)  |||
         atom
-}

atom   = tok "-" &&& number >>> (\(u,n) -> -n)                |||
         number                                               |||
         tok "(" &&& expr &&& tok ")" >>> (\(o,(n,c)) -> n)

-- Putting the initial prefix parser first:
-- eval "1"           (96 reductions, 168 cells)
-- eval "(1)"         (191 reductions, 335 cells)
-- eval "((1))"       (283 reductions, 498 cells)
-- eval "(((1)))"     (375 reductions, 661 cells)
-- eval "1+2+3+4+5"   (472 reductions, 905 cells)
-- eval "1+"          (124 reductions, 251 cells)

expr   = term &&& (tok "+" &&& expr >>> (\(p,y) -> (+y))       |||
                   tok "-" &&& expr >>> (\(m,y) -> subtract y) |||
                   result id) >>> \(n,f) -> f n

term   = atom &&& (tok "*" &&& term >>> (\(t,y) -> (*y))       |||
                   tok "/" &&& term >>> (\(d,y) -> (/y))       |||
                   result id) >>> \(n,f) -> f n

eval s = case expr s of ((x,""):_) -> x
                        _          -> error "Syntax error in input"

