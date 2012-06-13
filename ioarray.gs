------------------------------------------------------------------------------
-- This file contains a Gofer implementation of the monadic array
-- primitives for Lazy state threads, as described in the PLDI '94
-- paper by John Launchbury and Simon Peyton Jones, using new Gofer
-- primitives added in Gofer 2.30.
--
-- This file must be loaded only after both array.gs and iomonad.gs,
-- and requires the standard, or cc prelude.
--
-- You will not be able to use this file unless the version of Gofer that
-- is installed on your machine has been compiled with the IO_MONAD flag
-- and the HASKELL_ARRAYS flag set to 1.
--
-- Mark P Jones, 1994
------------------------------------------------------------------------------

module LazyStateArr( newArr, readArr, writeArr, freezeArr ) where

primitive primSTNewArr   "primSTNewArr"
          :: (a -> Int) -> (a,a) -> b -> ST s (MutArr s a b)
primitive primSTReadArr  "primSTReadArr"
          :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> ST s b
primitive primSTWriteArr "primSTWriteArr"
          :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> b -> ST s ()
primitive primSTFreeze   "primSTFreeze"
          :: MutArr s a b -> ST s (Array a b)

newArr       :: Ix a => (a,a) -> b -> ST s (MutArr s a b)
newArr bounds = primSTNewArr (index bounds) bounds

readArr      :: Ix a => MutArr s a b -> a -> ST s b
readArr       = primSTReadArr index

writeArr     :: Ix a => MutArr s a b -> a -> b -> ST s ()
writeArr      = primSTWriteArr index

freezeArr    :: Ix a => MutArr s a b -> ST s (Array a b)
freezeArr     = primSTFreeze

------------------------------------------------------------------------------
