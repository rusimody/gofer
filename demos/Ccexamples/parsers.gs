-------------------------------------------------------------------------------
-- A variety of parsers, with the ability to use overloading to choose between
-- parsers by a top-level type signature:
--
-- ? parse topExpr "1+2"  :: [Int]            ==> [3
--                                                 Program error:
--                                                   Unexpected character `+'
-- ? parse topExpr "1+2"  :: Maybe Int        ==> Just 3
-- ? parse topExpr "1+2"  :: ParseResult Int  ==> ParsedAs 3
--
-- ? parse topExpr "(1+2" :: [Int]           ==> Program error: missing `)'
-- ? parse topExpr "(1+2" :: Maybe Int       ==> Program error: missing `)'
-- ? parse topExpr "(1+2" :: ParseResult Int ==> ParseError "missing `)'"
-- 
-- Mark P. Jones, April 12 1993
-------------------------------------------------------------------------------

infixr 7 `seq`
infixl 6 `pam`, `bind_`

-- All parsers are constructed from a monad in the following way: -------------

type Parser m a = String -> m (a,String)
  in mapP, resultP, bindP, zeroP, plusP, parse, lookahead, sat, errPE

mapP           :: Monad m => (a -> b) -> Parser m a -> Parser m b
mapP f p s      = [ (f x, s') | ~(x,s') <- p s ]

resultP        :: Monad m => a -> Parser m a
resultP x s     = result (x,s)

bindP          :: Monad m => Parser m a -> (a -> Parser m b) -> Parser m b
(p `bindP` q) s = p s `bind` \ ~(x,s') -> q x s'

zeroP          :: Monad0 m => Parser m a
zeroP s         = zero

plusP          :: MonadPlus m => Parser m a -> Parser m a -> Parser m a
(p `plusP` q) s = p s ++ q s

instance Monad m => Functor (Parser m) where
  map = mapP

instance Monad m => Monad (Parser m) where
  result = resultP
  bind   = bindP

instance Monad0 m => Monad0 (Parser m) where
  zero = zeroP

instance MonadPlus m => MonadPlus (Parser m) where
  (++) = plusP

class MonadPlus (Parser m) => ParseMonad m where
  parseError  :: String -> Parser m a
  parseError s = error s  -- the user really ought to use a monad that
                          -- provides a better defn than this if they
                          -- want to use parseError in real programs

-- Auxiliary functions, using the definition of Parser: -----------------------

parse          :: Monad m => Parser m a -> String -> m a
parse p s       = [ x | ~(x,s') <- p s ]

parse' p s       = [ x | ~(x,s') <- p s ]

lookahead      :: Monad m => Parser m String
lookahead s     = [ (s,s) ]

sat            :: Monad0 m => (Char -> Bool) -> Parser m Char
sat p []        = zero
sat p (h:ts)    = [ (h,ts) | p h ]

-- General utility functions: -------------------------------------------------

pam            :: Functor f => f a -> (a -> b) -> f b
m `pam` f       = map f m

bind_          :: Monad m => m a -> m b -> m b
p `bind_` q     = p `bind` const q

seq            :: Monad m => m a -> m b -> m (a,b)
p `seq` q       = p `bind` \x -> q `bind` \y -> result (x,y)

many           :: MonadPlus m => m a -> m [a]
many p          = q where q = (p `bind` \x -> q `bind` \xs -> result (x:xs))
                              ++
                              result []

many1          :: MonadPlus m => m a -> m [a]
many1 p         = p `bind` \x -> many p `bind` \xs -> result (x:xs)

tok            :: ParseMonad m => String -> Parser m ()
tok             = foldr bind_ (result ()) . map (sat . (==))

-- Simple parsers, uncontrolled backtracking, list of parses: -----------------

instance ParseMonad []

-- The Maybe monad: -----------------------------------------------------------

data Maybe a = Just a | None

instance Functor Maybe where
    map f (Just x)  = Just (f x)
    map f None      = None

instance Monad Maybe where
    result          = Just
    Just x `bind` f = f x
    None   `bind` f = None

instance Monad0 Maybe where
    zero            = None

instance MonadPlus Maybe where
    None   ++ y     = y
    Just x ++ y     = Just x

instance ParseMonad Maybe

-- Simple parsers, uncontrolled backtracking, list of parses: -----------------

data ParseResult a = ParsedAs a
                   | ParseError String
                   | Backtrack

instance Functor ParseResult where
  map f (ParsedAs x)     = ParsedAs (f x)
  map f (ParseError msg) = ParseError msg
  map f Backtrack        = Backtrack

instance Monad ParseResult where
  result x                = ParsedAs x
  ParsedAs x     `bind` f = f x
  ParseError msg `bind` f = ParseError msg
  Backtrack      `bind` f = Backtrack

instance Monad0 ParseResult where
  zero = Backtrack

instance MonadPlus ParseResult where
  Backtrack  ++ y = y
  other      ++ y = other

errPE      :: String -> Parser ParseResult a
errPE msg s = ParseError msg

instance ParseMonad ParseResult where
  parseError = errPE

-- A silly grammar for arithmetic expressions: -------------------------------

topExpr, expr, term, atom, number, digit:: ParseMonad m => Parser m Int

topExpr = expr        `bind` \e ->
          lookahead   `bind` \s ->
          if null s then result e
                    else parseError ("Unexpected character `"++[head s]++"'")

expr    = term `bind` \x ->
          (tok "+" `bind_` expr `pam` (x+)   ++
           tok "-" `bind_` expr `pam` (x-)   ++
           result x)

term    = atom `bind` \x ->
          (tok "*" `bind_` term `pam` (x*)   ++
           tok "/" `bind_` term `pam` (x/)   ++
           result x)

atom    =  tok "-" `bind_` expr `pam` negate
          ++
           tok "(" `bind_` expr `bind` (\n -> tok ")" `bind_` result n
                                              ++
                                              parseError "missing `)'")
          ++
           number

number  = many1 digit `pam` foldl1 (\a x -> 10*a+x)

digit   = sat isDigit `pam` \d -> ord d - ord '0'

-------------------------------------------------------------------------------
