-- Graphical Knights-Tour program : --------------------------------------------
--
--
-- author : Luc Duponcheel
--

--
-- when typing
--
-- ? showHorses ""
--
-- the program should produce on your terminal something like :
--


--     01  64  53  58  03  60  51  34

--     54  57  02  61  52  35  04  49

--     63  30  55  46  59  50  33  22

--     56  43  62  31  36  21  48  05

--     29  14  45  42  47  32  23  20

--     44  41  28  15  18  37  06  09

--     13  16  39  26  11  08  19  24

--     40  27  12  17  38  25  10  07

-- (93975 reductions, 179126 cells)


--
-- (with odd numbers in inverse-video)
--


--
-- On a Sun SPARC the program has to run in a shell tool 
--                                            ^^^^^

--
-- On my Amiga I can also experiment with colours ...
--

--
-- The following well known strategy is used :
--
-- 	choose a move is which is such that, 
-- 	after having done the move, 
-- 	a minimal number of next moves is possible.
--
--
-- If your computer is `slow enough' then you will notice that finding such a 
-- move takes longer in the beginning than at the end of the move sequence.
-- 

--
-- all attempts to find a faster solution are encouraged
--                        ^^^^^^

--
-- PS:
--
-- if you want to avoid that your boss can see that you are
-- playing silly games at work, then you can always type in
--
-- ? clearscreen "" 
--

--------------------------------------------------------------------------------

--
-- the general purpose function 
-- revcomp 
-- composes a list of functions in reverse order
--

revcomp       :: [a -> a] -> a -> a
revcomp []     = id
revcomp (f:fs) = revcomp fs . f


--
-- some screen oriented functions 
-- it is possible that you'll have to
-- redefine them if you do not work with
-- an ANSI-compliant terminal.
--

escape = showChar '\ESC' . showChar '['

inverse = escape . showString "7m"
normal  = escape . showString "m"

goto x y = escape . shows y . showChar ';' . shows x . showChar 'H'

clearscreen = showString "\ESC[2J"  -- ANSI version
clearscreen = showChar '\^L'        -- Sun window

continue = normal . goto 0 20


-- main types

type Horse  = (Int,Int)
type Horses = [Horse]

type PartOfBoard = [(Int,Int)]


-- all possible moves from (u,v) to (x,y)

(|-->) :: Horse -> Horse -> Bool
(u,v) |--> (x,y) = (x == u+1) && (y == v-2) ||  
                   (x == u-2) && (y == v-1) ||
                   (x == u-2) && (y == v+1) || 
                   (x == u+2) && (y == v-1) || 
                   (x == u+2) && (y == v+1) || 
                   (x == u+1) && (y == v+2) || 
                   (x == u-1) && (y == v+2)    

{-
                   (x == u-1) && (y == v-2)    --  NOT used!
-}

horsesOn    :: PartOfBoard -> Horse -> Horses
horsesOn pb h = [ h' | h' <- pb, h |--> h' ]


-- strategy 

(>>)            :: [a] -> [b] -> Bool
_      >> [_]    = True
[_]    >> _      = False
(_:ms) >> (_:ns) = ms >> ns

minimalize         :: (a -> [b]) -> [a] -> (a,[b])
minimalize f [h]    = (h,f h)
minimalize f (h:hs) = let (k,ms) = minimalize f hs ; ns = f h in 
                       if ns >> ms then (k,ms) else (h,ns)


-- how to find all horses (  -: stands for `minus`  )

(-:)        :: Eq a => [a] -> a -> [a]
(x:xs) -: y 
 | x == y    = xs
 | otherwise = x : (xs -: y)
[]     -: x  = []


horses :: Horses
horses = fst (moves 64)
          where
           moves 1     = ([(1,1)],[ (i,j) | i <- [1..8], j <- [1..8] ] -: (1,1))
           moves (n+1) = let 
                           (hs@(hn:_),b) = moves n
                           f = horsesOn b
                           (h,_) = minimalize f (f hn)
                          in
                           (h:hs,b-:h)



-- How to show a move

showMove (x,y) n = g (x,y) . f n 
                    where g1 (x,y) = goto (2+4*x) (2+2*y)
                          g (x,y) 
                              | even (x+y) = g1 (x,y) . inverse 
                              | otherwise  = g1 (x,y) . normal 
                          f1 n 
                              | n < 10     = showChar '0'  
                              | otherwise  = id
                          f n = let m = 65 - n in f1 m . shows m
 
-- How to show all horses

showHorses 
 = clearscreen . revcomp (zipWith showMove horses [1..]) . continue 


--------------------------------------------------------------------------------

