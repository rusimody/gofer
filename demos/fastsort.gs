{- list sorting: see L.C.Paulson, ML for the working programmer, Cambidge, p100
-- The list is split into ascending chunks which are then merged in pairs.

samsort l = sorting [] 0 l
  where	sorting ls k []		= head(mergepairs ls 0)
	sorting	ls k (x:xs)	= sorting (mergepairs (run:ls) kinc) kinc tl
	  where	(run, tl)	= nextrun [x] xs
		kinc		= k+1
	nextrun run []		= (reverse run, [])
	nextrun	rs@(r:_) xl@(x:xs)
		| x<r		= (reverse rs, xl)
		| otherwise	= nextrun (x:rs) xs
	mergepairs [l] _ = [l]
	mergepairs lx@(l1:l2:ls) k
		| k`mod`2 == 1	= lx
		| otherwise	= mergepairs((merge l1 l2):ls) (k/2)
-}

-- this mergesort uses a partioning mechanism like quicksort to build
-- longer initial sequences. It also uses a non-counting mergePairs.
-- Bob Buckley 30-MAR-93 (Bob.Buckley@levels.unisa.edu.au)

msort xs = mergePhase (runPhase xs)
  where	mergePhase [x]		= x
	mergePhase [x,y]	= merge x y	-- redundant case
	mergePhase l		= mergePhase (mergePairs l)
	mergePairs [x1,x2]	= [merge x1 x2]	-- redundant case
	mergePairs (x1:x2:xs)	= merge x1 x2 : mergePairs xs
	mergePairs l		= l		-- note: l=[] or l=[_]
	runPhase []	= [[]]
	runPhase (e:es) = takeAsc [e] es
	takeAsc asc []	= [reverse asc]
	takeAsc xs@(x:_) zs@(z:zr)
		| x<=z		= takeAsc (z:xs) zr
		| otherwise	= takeDec xs [z] zr
	takeDec asc dec []	= [merge (reverse asc) dec]
	takeDec xs@(x:_) ys@(y:_) zs@(z:zr)
		| z<y		= takeDec xs (z:ys) zr
		| x<=z		= takeDec (z:xs) ys zr
		| otherwise	= merge (reverse xs) ys : runPhase zs

