-- Some simple Gofer programs for manipulating matrices.
--

type Matrix k = [Row k]          -- matrix represented by a list of its rows
type Row k    = [k]              -- a row represented by a list of literals

-- General utility functions:

shapeMat    :: Matrix k -> (Int, Int)
shapeMat mat = (rows mat, cols mat)

rows        :: Matrix k -> Int
rows mat     = length mat

cols        :: Matrix k -> Int
cols mat     = length (head mat)

idMat       :: Int -> Matrix Int
idMat 0      = []
idMat (n+1)  = [1:copy n 0] ++ map (0:) (idMat n)

-- Matrix multiplication:

multiplyMat                     :: Matrix Int -> Matrix Int -> Matrix Int
multiplyMat a b | cols a==rows b = [[row `dot` col | col<-b'] | row<-a]
                | otherwise      = error "incompatible matrices"
                 where v `dot` w = sum (zipWith (*) v w)
                       b'        = transpose b

-- An attempt to implement the standard algorithm for converting a matrix
-- to echelon form...

echelon   :: Matrix Int -> Matrix Int
echelon rs = rs,                                if null rs || null (head rs)
           = map (0:) (echelon (map tail rs)),  if null rs2
           = piv : map (0:) (echelon rs'),      otherwise
             where rs'            = map (adjust piv) (rs1++rs3)
                   (rs1,rs2)      = span leadZero rs
                   leadZero (n:_) = n==0
                   (piv:rs3)      = rs2
 
-- To find the echelon form of a matrix represented by a list of rows rs:
-- 
-- {first line in definition of echelon}:
--  If either the number of rows or the number of columns in the matrix
--  is zero (i.e. if null rs || null (head rs)), then the matrix is
--  already in echelon form.
-- 
-- {definition of rs1, rs2, leadZero in where clause}:
--  Otherwise, split the matrix into two submatrices rs1 and rs2 such that
--  rs1 ++ rs2 == rs  and all of the rows in rs1 begin with a zero.
--
-- {second line in definition of echelon}:
--  If rs2 is empty (i.e. if null rs2) then every row begins with a zero
--  and the echelon form of rs can be found by adding a zero on to the
--  front of each row in the echelon form of (map tail rs).
--
-- {Third line in definition of echelon, and definition of piv, rs3}:
--  Otherwise, the first row of rs2 (denoted piv) contains a non-zero
--  leading coefficient.  After moving this row to the top of the matrix
--  the original matrix becomes  piv:(rs1++rs3).
--  By subtracting suitable multiples of piv from (suitable multiples of)
--  each row in (rs1++rs3) {see definition of adjust below}, we obtain a
--  matrix of the form:
--
--          <----- piv ------>
--          __________________
--          0  |
--          .  |
--          .  |      rs'        where rs' = map (adjust piv) (rs1++rs3)
--          .  |
--          0  |
--
--  whose echelon form is  piv : map (0:) (echelon rs').
--

adjust              :: Num a => Row a -> Row a -> Row a
adjust (m:ms) (n:ns) = zipWith (-) (map (n*) ms) (map (m*) ns)

-- A more specialised version of this, for matrices of integers, uses the
-- greatest common divisor function gcd in an attempt to try and avoid
-- result matrices with very large coefficients:
--
-- (I'm not sure this is really worth the trouble!)

adjust'              :: Row Int -> Row Int -> Row Int
adjust' (m:ms) (n:ns) = ns,                                  if g==0
                      = zipWith (\x y -> b*y - a*x) ms ns,   otherwise
                        where g = gcd m n
                              a = n/g
                              b = m/g
-- end!!
