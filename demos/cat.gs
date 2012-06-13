-- A version of the Unix utility cat coded up using the I/O facilities of
-- Gofer, with a dash of Gofer overloading to enable the use of different
-- argument forms:
--

-- Here is a simple version, not using any overloading:
-- (this version should work in Haskell)

unixCat :: [String] -> Dialogue
unixCat  = foldr showFile done
           where showFile name cont = readFile name abort
                                      (\s->appendChan stdout s abort cont)

-- Now we get a little ambitious and write some Gofer-only code:

class    Cat a        where cat  :: a -> Dialogue
instance Cat String   where cat n = showFile n done
instance Cat [String] where cat   = foldr showFile done

showFile name cont = readFile name abort (\s->appendChan stdout s abort cont)
