-- Here are a collection of fairly standard functions for manipulating
-- one form of binary trees

data Tree a = Lf a | Tree a :^: Tree a

reflect t@(Lf x)  = t
reflect (l:^:r)   = r :^: l

mapTree f (Lf x)  = Lf (f x)
mapTree f (l:^:r) = mapTree f l :^: mapTree f r

-- Functions to calculate the list of leaves on a tree:

leaves, leaves'  :: Tree a -> [a]

leaves (Lf l)     = [l]                     -- direct version
leaves (l:^:r)    = leaves l ++ leaves r

leaves' t         = leavesAcc t []          -- using an accumulating parameter
                    where leavesAcc (Lf l)  = (l:)
                          leavesAcc (l:^:r) = leavesAcc l . leavesAcc r

-- Picturing a tree:

drawTree :: Text a => Tree a -> String
drawTree  = unlines . thd3 . pic
 where pic (Lf a)  = (1,1,["-- "++show a])
       pic (l:^:r) = (hl+hr+1, hl+1, top pl ++ mid ++ bot pr)
                     where (hl,bl,pl) = pic l
                           (hr,br,pr) = pic r
                           top        = zipWith (++) (copy (bl-1) "   " ++
                                                      [" ,-"] ++
                                                      copy (hl-bl) " | ")
                           mid        = ["-| "]
                           bot        = zipWith (++) (copy (br-1) " | " ++
                                                      [" `-"] ++
                                                      copy (hr-br) "   ")

-- Finally, here is an example due to Richard Bird, which uses lazy evaluation
-- and recursion to create a `cyclic' program which avoids multiple traversals
-- over a data structure:

replaceAndMin m (Lf n)  =  (Lf m, n)
replaceAndMin m (l:^:r) =  (rl :^: rr, ml `min` mr)
                           where (rl,ml) = replaceAndMin m l
                                 (rr,mr) = replaceAndMin m r

replaceWithMin t = mt where (mt,m) = replaceAndMin m t

sample  = (Lf 12 :^: (Lf 23 :^: Lf 13)) :^: Lf 10
sample2 = sample  :^: sample
sample4 = sample2 :^: sample2
