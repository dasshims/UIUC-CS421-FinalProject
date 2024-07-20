{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

-- Tree folding

module SmashA.Reduce (sizeListTree, collectListTree, sumListTree) where

import TreeDatatype
import SmashA.TreeDats

import SmashA.Syb4A

-- count the leaves
sizeListTree t =  gapp (TL_red_ctr prop) HNil t
 where
 prop "Leaf" ws = (1::Int)
 prop _ ws = sum ws


-- collect the Int values from the leaves of the tree
-- We don't just collect all Ints (because the tree may have Ints not only
-- at the leaves
collectIntLeaves inj red t = 
    gapp (TL_red_ctr prop)
	 ((\(x::Int) -> inj x) :+: HNil) t
 where
 prop "WithWeight" [w,weight] = w -- disregard weight
 prop _ ws = red ws


collectListTree t = collectIntLeaves (:[]) concat t
sumListTree t = collectIntLeaves id sum t

example :: [Tree Int Int]
example = [WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
          ,WithWeight (Leaf 25 `Fork` Leaf 48) 2]

test1 = sizeListTree example
test2 = collectListTree example
test3 = sumListTree example
