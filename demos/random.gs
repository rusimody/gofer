-- Generate a list of random numbers of length n

randoms :: Int -> [Int]
randoms  = iterate (\seed-> (77*seed+1) `rem` 1024)

rand100  = sort (take 100 (randoms 1000))   -- a sample distribution

adjs []  = []				    -- a list of pairs of adjacent
adjs xs  = zip xs (tail xs)		    -- elements in a list


