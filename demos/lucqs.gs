-- A `prettier' version of the 8 queens program that displays the
-- solutions to the 8 queens problems on chess boards ... you need
-- a terminal that uses standard ANSI (I think) control sequences
-- to switch between normal and inverse video to use this program.
--
-- Written by Luc Duponcheel, March 1993

-- this is standard

row n = [(n,m) | m <- [1..8]]

qss 0 = [[]]
qss n = [ q:qs | qs <- qss (n-1) , q <- row n, all (ok q) qs]

ok (m,n) (i,j) = j/=n && (i+j/=m+n) && (i-j/=m-n)

-- fold is (among others) useful for showing lists WITHOUT '[' , ',' , ']'
-- BTW the definition of fold is similar to the one of map
-- fold and map can easily be generalised 

fold :: (a -> b -> b) -> [a] -> b -> b
fold f [] = id
fold f (x:xs) = f x . fold f xs

-- For inverse video

inv = [chr 27] ++ "[7m"
res = [chr 27] ++ "[m"

-- how to show Blanks and Queens

data Mode  = Md   (Int,Int)

data Queen = Qn   (Int,Int)
data Blank = Blnk (Int,Int)

instance Text Mode where
	showsPrec p (Md (n,m)) | even s = showString inv
	                       | odd  s = showString res
                                 where s = (n+m)

instance Text Queen where
	showsPrec p (Qn (n,m))   = shows (Md (n,m)) . showString "++"
 	                    

instance Text Blank where
	showsPrec p (Blnk (n,m)) = shows (Md (n,m)) . showString "  "
        showList = fold shows

blanksBefore (n,m) = [Blnk (n,i) | i <- [1..(m-1)]] 
blanksAfter  (n,m) = [Blnk (n,i) | i <- [(m+1)..8]] 

-- how to show Rows and Boards

data Row   = Rw  (Int,Int)
data Board = Brd [Row]


instance Text Row where
	showsPrec p (Rw q)
	  = showChar '\t' . shows (blanksBefore q) 
            . shows (Qn q) . 
            shows (blanksAfter q) . showString res . showChar '\n'

instance Text Board where
	showsPrec p (Brd qs) = showChar '\n' . fold shows qs . showChar '\n'
        showList = fold shows
   
main :: Dialogue
main = appendChan stdout solutions exit done
       where solutions = show ([Brd [Rw q | q <- qs] | qs <- (qss 8)])

