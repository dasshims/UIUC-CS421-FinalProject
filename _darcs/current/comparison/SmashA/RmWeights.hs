{-# OPTIONS_GHC -fglasgow-exts  #-}

-- Remove Weights from a WTree: ad-hoc processing for a particular
-- _data_ constructor

module RmWeights (rmWeightsWTree) where

import TreeDatatype
import TreeDats

import Syb4A

rmWeights tt t = gapp TL_recon (prop tt :+: HNil) t
 where
 prop :: WTree a b -> WTree a b -> WTree a b -> WTree a b
 prop _ _ (WithWeight t weight) = t -- disregard weight
 prop _ _ d = d

rmWeightsWTree :: WTree Int Int -> WTree Int Int
rmWeightsWTree t = rmWeights t t

main = print $ (rmWeightsWTree mytree4,rmWeightsWTree mytree3)

-- A different way of accomplishing the same: using gfold rather than gmap

-- Note however that this does not satisfy the test because the function
-- should not refer to constructors other than WithWeight
rmWeights1 t = gapp (TL_red_ctr prop)
	 ((\(x::Int) _ -> Leaf x) :+: HNil) t
 where
 prop "Leaf" [x] = x
 prop "Fork" [tl, tr] = Fork tl tr
 prop "WithWeight" [t,weight] = t -- disregard weight

rmWeightsWTree1 :: WTree Int Int -> WTree Int Int
rmWeightsWTree1 = rmWeights1

main1 = print $ (rmWeightsWTree1 mytree4,rmWeightsWTree1 mytree3)

