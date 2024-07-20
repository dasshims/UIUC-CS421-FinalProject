{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- Generic transpose:
-- gtranspose:: f (g a) -> g (f a)
--
-- Note: the above equation does not define transpose precisely enough
-- for example, if f == Either True and g == [] and a == Int
-- then it's possibly OK to have these reductions
-- gtranspose Left True --> []
-- gtranspose Left True --> [Left False]
-- gtranspose Right [1] --> [Right 1]
-- gtranspose Right [1] --> [Left False]
--
-- To disambiguate, we need to parameterize on the functions
--   a     -> f a
--   [g a] -> g a
-- These functions are to make the requisite choices.

module Transpose {- (transposeList, transposeListBTree) -} where

import BinTreeDatatype
import BTreeDats

import Syb4A

-- This is the generic map
gmap f x = gapp TL_recon ((\x y -> f x) :+: HNil) x


gtran_sig :: (a->f a) -> ([g (f a)] -> g (f a)) -> f (g a) -> g (f a)
gtran_sig = undefined

gtranspose injf joing t | False = gtran_sig injf joing t
gtranspose injf joing t = gapp (TL_red joing) (prede t injf :+: HNil) t
 where
 prede_sig :: f (g a) -> (a -> f a) -> g a -> g (f a) -> g (f a)
 prede_sig = undefined
 prede t injf | False = prede_sig t injf
 prede _ injf = \ga y -> gmap injf ga


-- transposeList :: [[a]] -> [[a]]
transposeList t = gtranspose (:[]) merge t
  where
  merge [] = []
  merge [l1,[]] = l1
  merge [l1,l2] = zipWith (++) l1 l2

ttl1 = transposeList [[1::Int,2,3],[4,5,6]]
ttl2 = transposeList [[1::Int,2,3],[4,5,6],[7,8,9]]
ttl3 = transposeList ["abc","def","ghi"]
-- ["adg","beh","cfi"]

transposeListBTree t = gtranspose (:[]) merge t
  where
  merge [] = Leaf []
  merge [Leaf l1, Leaf l2] = Leaf (l1 ++ l2)

ttb1 = transposeListBTree [Leaf (1::Int), Leaf 2, Leaf 3]
-- Leaf [1,2,3]


