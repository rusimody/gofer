------------------------------------------------------------------------------
-- Here is a version of the graph algorithms described in:
-- 
-- Lazy Depth-First Search and Linear Graph Algorithms in Haskell
-- David King and John Launchbury
-- 
-- Together with some additional code for printing tree structures ...
--
-- This program requires array.gs, iomonad.gs, and ioarray.gs to run.
-- For example, in the demos/IO directory, try:
--
--   :load ../../array.gs ../../iomonad.gs ../../ioarray.gs ldfs.gs
--
-- Of course, it would be sensible to put these things in a project file!
--
------------------------------------------------------------------------------

type Vertex  = Char

-- Representing graphs:

type Table a = Array Vertex a
type Graph   = Table [Vertex]

vertices :: Graph -> [Vertex]
vertices  = indices

type Edge = Assoc Vertex Vertex

edges    :: Graph -> [Edge]
edges g   = [ v := w | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ v := f v (t!v) | v <- indices t ]

type Bounds = (Vertex, Vertex)

outdegree :: Graph -> Table Int
outdegree  = mapT numEdges
             where numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG  = accumArray (flip (:)) []

graph = buildG ('a','j')
         (reverse
          [ 'a' := 'b',  'a' := 'f',  'b' := 'c',
            'b' := 'e',  'c' := 'a',  'c' := 'd',
            'e' := 'd',  'g' := 'h',  'g' := 'j',
            'h' := 'f',  'h' := 'i',  'h' := 'j' ]
         )

transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ w := v | (v := w) <- edges g ]

indegree :: Graph -> Table Int
indegree  = outdegree . transposeG


-- Depth-first search

-- Specification and implementation of depth-first search:

data Tree a   = Node a (Forest a)
type Forest a = [Tree a]

dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

type Set s    = MutArr s Vertex Bool

mkEmpty      :: Bounds -> ST s (Set s)
mkEmpty bnds  = newArr bnds False

contains     :: Set s -> Vertex -> ST s Bool
contains m v  = readArr m v

include      :: Set s -> Vertex -> ST s ()
include m v   = writeArr m v True

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds  `thenST` \m ->
                       chop m ts)

chop         :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m []     = returnST []
chop m (Node v ts : us)
              = contains m v `thenST` \visited ->
                if visited then
                  chop m us
                else
                  include m v `thenST` \_  ->
                  chop m ts   `thenST` \as ->
                  chop m us   `thenST` \bs ->
                  returnST (Node v as : bs)

-- Depth-first search algorithms

-- Algorithm 1: depth first search numbering

preorder            :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF           :: Forest a -> [a]
preorderF ts         = concat (map preorder ts)

preOrd :: Graph -> [Vertex]
preOrd  = preorderF . dff

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zipWith (:=) vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF

-- Algorithm 2: topological sorting

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF   :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd      :: Graph -> [Vertex]
postOrd       = postorderF . dff

topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

-- Algorithm 3: connected components

components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

scc          :: Graph -> Forest Vertex
scc g         = dfs (transposeG g) (reverse (postOrd g))

scc'         :: Graph -> Forest Vertex
scc' g        = dfs g (reverse (postOrd (transposeG g)))

-- Algorithm 5: Classifying edges

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v rs) = [ v := w | Node w us <- ts ] ++
                          concat (map flat ts)

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree!v

-- Algorithm 6: Finding reachable vertices

reachable    :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

path         :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

-- Algorithm 7: Biconnected components

bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (u,du,lu) xs <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,dv,lv) ts)
      = [ Node (v:vs) us | (l,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws us) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]

figure4 = buildG ('a','i') (vs ++ reverse [ v:=w | (w:=v) <- vs ])
          where vs = [ 'b' := 'a', 'e' := 'a', 'c' := 'b',
                       'd' := 'c', 'b' := 'd', 'f' := 'e',
                       'h' := 'e', 'g' := 'f', 'e' := 'g',
                       'i' := 'h', 'a' := 'i', 'h' := 'a' ]

figure5 = showForest (map (label figure4 dnum) f)
          where f    = dff figure4
                dnum = preArr (bounds figure4) f

figure7 = showForest (bcc figure4)

-- Utility functions for drawing trees and forests:

showTree :: Text a => Tree a -> String
showTree  = drawTree . mapTree show

showForest :: Text a => Forest a -> String
showForest  = unlines . map showTree

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

drawTree        :: Tree String -> String
drawTree         = unlines . draw

draw (Node x ts) = grp this (space (length this)) (stLoop ts)
 where this          = s1 ++ x ++ " "

       stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst rst   = zipWith (++) (fst:repeat rst)

       [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]

-- Instances of Eq and Text that are not included in the Gofer preludes:

instance (Eq a, Eq b, Eq c) => Eq (a,b,c) where
    (a,b,c)==(p,q,r) = a==p && b==q && c==r

instance (Text a, Text b, Text c) => Text (a,b,c) where
    showsPrec d (x,y,z) = showChar '(' . shows x . showChar ',' .
                                         shows y . showChar ',' .
                                         shows z . showChar ')'

------------------------------------------------------------------------------
