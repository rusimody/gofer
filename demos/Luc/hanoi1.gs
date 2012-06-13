-- Graphical Hanoi program : ---------------------------------------------------
--
--
-- author : Luc Duponcheel
--

--
-- when typing
--
-- ? hanoi n  (n is any natural number)
--
-- the program should produce a list of tower configurations.
--


--
-- On a Sun SPARC the program has to run in a shell tool 
-- (I assume that any ANSI-compliant terminal is OK)
--    

--
-- PS:
--
-- if you want to avoid that your boss can see that you are
-- playing silly games at work, then you can always type in
--
-- ? clearscreen "" 
--

--------------------------------------------------------------------------------

-- general purpose function `comp' composes a list of functions 

comp       :: [a -> a] -> a -> a
comp []     = id
comp (f:fs) = f . comp fs 

-- some screen oriented functions 

escape = showChar '\ESC' . showChar '['

inverse = escape . showString "7m"
normal  = escape . showString "m"

clearscreen = showString "\ESC[2J"  -- ANSI version
clearscreen = showChar '\^L'        -- Sun window

-- how to show one disk

showSpace = showString . space 

showDisk x = showSpace (10-x) 
            . inverse . showSpace (2*x)  -- shows the disk in black
            . normal . showSpace (10-x)

-- how to show one horizontal level (3, possibly dummy, disks)

newlevel  = showChar '\n'

showLevel (0,0,0) = newlevel
showLevel (x,0,0) = showDisk x . newlevel
showLevel (x,y,0) = showDisk x . showDisk y . newlevel
showLevel (x,y,z) = showDisk x . showDisk y . showDisk z . newlevel

-- padding the towers vertically with dummy disks

pad xs len = [ 0 | x <- [0..(len - length xs)] ] ++ xs

-- actual moves

next ((a:as),bs,cs) (0,1) = (as,(a:bs),cs)
next ((a:as),bs,cs) (0,2) = (as,bs,(a:cs))
next (as,(b:bs),cs) (1,0) = ((b:as),bs,cs)
next (as,(b:bs),cs) (1,2) = (as,bs,(b:cs))
next (as,bs,(c:cs)) (2,0) = ((c:as),bs,cs)
next (as,bs,(c:cs)) (2,1) = (as,(c:bs),cs)

-- how to show one tower configuration

showConfiguration n (as,bs,cs) = 
     comp [ showLevel ts | ts <- zip3 (pad as n) (pad bs n) (pad cs n) ]

-- how to show all tower configurations

showConfigurations n cnf []     =  showConfiguration  n cnf
showConfigurations n cnf (x:xs) =  showConfiguration  n cnf
                                 . showConfigurations n (next cnf x) xs

-- how to start

startconf n = ([1..n],[],[])

-- how to continue (main code : is surprisingly simple)

cont 0 [a,b,c] = [] 
cont n [a,b,c] = cont (n-1) [a,c,b] ++ [(a,b)] ++ cont (n-1) [c,b,a]

-- hanoi

hanoi n = showConfigurations n (startconf n) (cont n [0,2,1]) ""

--------------------------------------------------------------------------------

