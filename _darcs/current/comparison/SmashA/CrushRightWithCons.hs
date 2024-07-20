{-# OPTIONS_GHC -fglasgow-exts  #-}

-- WTree folding

-- This a variant of the CrushRight module where the function
-- explicitly mentions the constructors of the datatype (not
-- allowed in the test).

module CrushRightWithCons (sizeListTree, flattenListTree, sumListTree) where

import TreeDatatype
import TreeDats

import Syb4A

-- count the leaves
sizeListTree t =  gapp (TL_red_ctr prop) HNil t
 where
 prop "Leaf" ws = (1::Int)
 prop _ ws = sum ws

{-
size_any_ds_with_tree tt t =  gapp (TL_red sum) (prop tt :+: HNil) t
 where
 prop :: WTree a b -> WTree a b -> Int -> Int
 prop _ (Leaf _) _ = (1::Int)
 prop _ _ d = d

-}

-- collect the Int values from the leaves of the tree
-- We don't just collect all Ints (because the tree may have Ints not only
-- at the leaves
collectIntLeaves inj red t = 
    gapp (TL_red_ctr prop)
	 ((\(x::Int) _ -> inj x) :+: HNil) t
 where
 prop "WithWeight" [w,weight] = w -- disregard weight
 prop _ ws = red ws


flattenListTree t = collectIntLeaves (:[]) concat t
sumListTree t = collectIntLeaves id sum t

example :: [WTree Int Int]
example = [WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
          ,WithWeight (Leaf 25 `Fork` Leaf 48) 2]

test1 = sizeListTree example
test2 = flattenListTree example
test3 = sumListTree example

-- Demonstrating that flattenListTree, sumListTree and sizeListTree
-- apply, AS THEY ARE, not only to [WTree Int Int] but to *ANY* data 
-- that includes WTree a b 

-- Note: this is possible in all approaches that support implicit
-- representations such as: EMGM, SYB, SYB3, Uniplate. --Alexey

cmex1:: Maybe (Either (WTree Int Int) (WTree Int Bool))
cmex1 = Just (Right (Leaf 1 `Fork` WithWeight (Leaf 2) True))
test_cmex1 = (sizeListTree cmex1, flattenListTree cmex1, sumListTree cmex1)
-- (2,[1,2],3)

cmex2:: (WTree Int Int, WTree (WTree Int Int) (WTree Int Int))
cmex2 = (mytree, Leaf mytree2 `Fork` WithWeight (Leaf mytree3) mytree4)
test_cmex2 = (sizeListTree cmex2, flattenListTree cmex2, sumListTree cmex2)
-- (7,[42,88,37,42,88,37,42,88],464)

