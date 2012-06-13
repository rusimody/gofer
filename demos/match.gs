
match []       ys       = null ys
match ('*':ps) xs       = or (map (match ps) (tails xs))
match (p:ps)   []       = False
match (p:ps)   (c:cs)
            | p==c      = match ps cs
            | otherwise = False

-- Some combinatorial problems:

tails []         = [[]]
tails xs'@(x:xs) = xs' : tails xs

inits []         = [[]]
inits (x:xs)     = [] : map (x:) (inits xs)

perms []         = [[]]
perms (x:xs)     = concat (map (inter x) (perms xs))
                   where inter x []         = [[x]]
                         inter x ys'@(y:ys) = (x:ys') : map (y:) (inter x ys)

subs []          = [[]]
subs (x:xs)      = subs xs ++ map (x:) (subs xs)

segs             = concat . map tails' . reverse . inits
                   where tails' []         = []
                         tails' xs'@(_:xs) = xs' : tails' xs
