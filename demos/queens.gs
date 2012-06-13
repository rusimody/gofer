-- The 8-queens problem from Bird and Wadler's book.
--
-- Be warned: printing out the complete list of solutions (all 92 of them)
-- by evaluating: layn (map show' (queens 8)) takes well over 1 million
-- reductions and uses nearly 2.5 million cells... it may take some time to
-- execute on slower systems! :-)

queens 0          = [[]]
queens (m+1)      = [ p++[n] | p<-queens m, n<-[1..8], safe p n ]

safe p n          = all not [ check (i,j) (m,n) | (i,j) <- zip [1..] p ]
                    where m = 1 + length p

check (i,j) (m,n) = j==n || (i+j==m+n) || (i-j==m-n)
 

