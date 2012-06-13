-- Graphical Towers-Of-Hanoi program : -----------------------------------------
--
--
-- author : Luc Duponcheel
--
-- the program is partly based on an earlier program which is 
-- originally written in Miranda* by Johan Vanslembrouck.
--
-- *Miranda is a trademark of Software Research Limited.
--

--
-- The program makes use of screen-oriented functions. 
-- It is possible that you'll have to redefine them if 
-- you do not work with an ANSI-compliant terminal.
--
                                              
--------------------------------------------------------------------------------


-- general purpose function `comp' composes a list of functions

comp       :: [a -> a] -> a -> a
comp []     = id
comp (f:fs) = f . comp fs 


-- screen oriented functions

escape = showChar '\ESC' . showChar '['

inverse = escape . showString "7m"
normal  = escape . showString "m"

goto x y = escape . shows y . showChar ';' . shows x . showChar 'H'

clearscreen = showString "\ESC[2J"  -- ANSI version
clearscreen = showChar '\^L'        -- Sun window

start  = clearscreen 
stop   = normal 


-- how to put and get a disk

showSpace = showString . space 

putDisk n x y = inverse . goto (n-x) y . showSpace (2*x)  
getDisk n x y = normal  . goto (n-x) y . showSpace (2*x)  

-- next configuartion

next ((a:as),bs,cs) (0,1) = (as,(a:bs),cs)
next ((a:as),bs,cs) (0,2) = (as,bs,(a:cs))
next (as,(b:bs),cs) (1,0) = ((b:as),bs,cs)
next (as,(b:bs),cs) (1,2) = (as,bs,(b:cs))
next (as,bs,(c:cs)) (2,0) = ((c:as),bs,cs)
next (as,bs,(c:cs)) (2,1) = (as,(c:bs),cs)

-- action to be performed

action n ((a:as),bs,cs) (0,1) 
 = let la = length as ; lb = length bs in 
   getDisk (2*n) a (2*n-la) . 
   comp [ putDisk (2*n) 1 (2*n-la-i) . getDisk (2*n) 1 (2*n-la-i)  
                                                        | i <- [1..n-la+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [3,4] ] .
   comp [ putDisk (5*n) 1 (n+i-2) . getDisk (5*n) 1 (n+i-2)        
                                                        | i <- [1..n-lb+1] ] .
   putDisk (5*n) a (2*n - lb)
action n ((a:as),bs,cs) (0,2) 
 = let la = length as ; lc = length cs in
   getDisk (2*n) a (2*n - la) . 
   comp [ putDisk (2*n) 1 (2*n-la-i) . getDisk (2*n) 1 (2*n-la-i) 
                                                        | i <- [1..n-la+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [3,4,5,6,7] ] .
   comp [ putDisk (8*n) 1 (n+i-2) . getDisk (8*n) 1 (n+i-2)        
                                                        | i <- [1..n-lc+1] ] .
   putDisk (8*n) a (2*n - lc)
action n (as,(b:bs),cs) (1,0) 
 = let lb = length bs ; la = length as in
   getDisk (5*n) b (2*n - lb) . 
   comp [ putDisk (5*n) 1 (2*n-lb-i) . getDisk (5*n) 1 (2*n-lb-i)  
                                                        | i <- [1..n-lb+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [4,3] ] .
   comp [ putDisk (2*n) 1 (n+i-2) . getDisk (2*n) 1 (n+i-2)        
                                                        | i <- [1..n-la+1] ] .
   putDisk (2*n) b (2*n - la) 
action n (as,(b:bs),cs) (1,2) 
 = let lb = length bs ; lc = length cs in
   getDisk (5*n) b (2*n - lb) . 
   comp [ putDisk (5*n) 1 (2*n-lb-i) . getDisk (5*n) 1 (2*n-lb-i)  
                                                        | i <- [1..n-lb+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [6,7] ] .
   comp [ putDisk (8*n) 1 (n+i-2) . getDisk (8*n) 1 (n+i-2)        
                                                        | i <- [1..n-lc+1] ] .
  putDisk (8*n) b (2*n - lc)
action n (as,bs,(c:cs)) (2,0) 
 = let lc = length cs ; la = length as in
   getDisk (8*n) c (2*n - lc) . 
   comp [ putDisk (8*n) 1 (2*n-lc-i) . getDisk (8*n) 1 (2*n-lc-i)  
                                                        | i <- [1..n-lc+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [7,6,5,4,3] ] .
   comp [ putDisk (2*n) 1 (n+i-2) . getDisk (2*n) 1 (n+i-2)        
                                                        | i <- [1..n-la+1] ] .
   putDisk (2*n) c (2*n - la)
action n (as,bs,(c:cs)) (2,1) 
 = let lc = length cs ; lb = length bs in
   getDisk (8*n) c (2*n - lc) . 
   comp [ putDisk (8*n) 1 (2*n-lc-i) . getDisk (8*n) 1 (2*n-lc-i)  
                                                        | i <- [1..n-lc+1] ] .
   comp [ putDisk (i*n) 1 (n-1) . getDisk (i*n) 1 (n-1) | i <- [7,6] ] .
   comp [ putDisk (5*n) 1 (n+i-2) . getDisk (5*n) 1 (n+i-2)        
                                                        | i <- [1..n-lb+1] ] .
   putDisk (5*n) c (2*n - lb)


-- how to show the initial configuration

showInit n = comp [ putDisk (2*n) x (y+n) | (x,y) <- zip [1..n] [1..n] ] 


-- the actual moves

moves n cnfg []     =  [] 
moves n cnfg (x:xs) = move : moves n nextcnfg xs
                     where 
                      nextcnfg = next cnfg x 
                      move = action n cnfg x 


-- how to show the moves

showMoves n = comp (moves n ([1..n],[],[]) (hanoi n [0,2,1]))
     

-- main code (simple!)
                             
hanoi 0 [a,b,c] = [] 
hanoi n [a,b,c] = hanoi (n-1) [a,c,b] ++ [(a,b)] ++ hanoi (n-1) [c,b,a]


-- how to show it all

showHanoi n = start . showInit n .  showMoves n . stop 


--------------------------------------------------------------------------------

