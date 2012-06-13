------------------------------------------------------------------------------
-- This file contains a Gofer implementation of Lazy state threads, as
-- described in the PLDI '94 paper by John Launchbury and Simon Peyton
-- Jones, using new Gofer primitives added in Gofer 2.30.
--
-- This file is included for the benefit of those interested in
-- experimenting with the use of lazy functional state threads.
-- You may expect to see changes to the definitions in this file,
-- to track future proposals for monadic I/O in Haskell.
--
-- This file requires the standard, or cc prelude.
-- You will not be able to use this file unless the version of Gofer that
-- is installed on your machine has been compiled with the IO_MONAD flag
-- set to 1.
--
-- Mark P Jones, 1994
------------------------------------------------------------------------------

module LazyStateThd( thenST, thenST_, returnST, newVar, readVar, WriteVar,
		     mutVarEq, getch, putchar, thenIO, seqST, putString,
		     getchar, interleaveST
		   ) where

infixr `thenST_`, `thenST`

primitive returnST     "primSTReturn"   :: a -> ST s a
primitive thenST       "primSTBind"     :: ST s a -> (a -> ST s b) -> ST s b
primitive newVar       "primSTNew"      :: a -> ST s (MutVar s a)
primitive readVar      "primSTDeref"    :: MutVar s a -> ST s a
primitive writeVar     "primSTAssign"   :: MutVar s a -> a -> ST s ()
primitive mutvarEq     "primSTMutVarEq" :: MutVar s a -> MutVar s a -> Bool
primitive getch        "primIOGetch"    :: IO Char
primitive putchar      "primIOPutchar"  :: Char -> IO ()
primitive thenIO       "primIOBind"     :: IO a -> (a -> IO b) -> IO b
primitive interleaveST "primSTInter"    :: ST s a -> ST s a

instance Eq (MutVar s a) where (==) = mutvarEq

thenST_       :: ST s () -> ST s b -> ST s b
p `thenST_` q  = p `thenST` \() -> q

seqST         :: [ST s ()] -> ST s ()
seqST          = foldr thenST_ (returnST ())

putString     :: String -> IO ()
putString      = seqST . map putchar

getchar = getch       `thenST` \c ->
          putchar c   `thenST_`
          returnST c

------------------------------------------------------------------------------
