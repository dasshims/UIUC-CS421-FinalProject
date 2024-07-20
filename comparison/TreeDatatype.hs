{-# OPTIONS_GHC -fglasgow-exts  -fgenerics #-} -- GHCi complains without it
                                -- when I import it...

module TreeDatatype where

import Data.Generics

-- A parameterised datatype for binary trees with data at the leafs
--   and possible "weight" labels
data WTree a w = Leaf a
               | Fork (WTree a w) (WTree a w)
               | WithWeight (WTree a w) w
       deriving Show


-- A typical tree
mytree :: WTree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

-- and another
mytree2 :: WTree Int Int
mytree2 = Fork (Leaf 42)
               (WithWeight (Fork (Leaf 88) (Leaf 37)) 3)

-- yet one more
mytree3 :: WTree Int Int
mytree3 = Fork (WithWeight (Leaf 42) 1)
               (WithWeight (Leaf 88) 2)

-- this one has nested weights
mytree4 :: WTree Int Int
mytree4 = Fork (WithWeight (WithWeight (Leaf 42) 1) 1)
               (WithWeight (Leaf 88) 2)


-- Constructors for larger WTrees

mkWTree :: Int -> WTree Int Int
mkWTree n | n <= 1 = Leaf n
          | even n = WithWeight (mkWTree (div n 2)) n
          | otherwise = Fork (mkWTree (3*n+1)) (mkWTree (n-1))

powerOfTwo :: Int -> WTree a w -> WTree a w
powerOfTwo 0     t = t
powerOfTwo (n+1) t = Fork (powerOfTwo n t) (powerOfTwo n t)

onewtree = mkWTree 15
bigwtree n = powerOfTwo n onewtree

sizeWTree (Leaf _)         = 1
sizeWTree (Fork l r)       = sizeWTree l + sizeWTree r
sizeWTree (WithWeight t w) = 1 + sizeWTree t

deepSeqWTree (Leaf x)         = seq x
deepSeqWTree (Fork l r)       = deepSeqWTree l . deepSeqWTree r
deepSeqWTree (WithWeight t w) = deepSeqWTree t . seq w
