{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- WTree folding. Trying to reproduce the approach by RepLib, etc.

-- Actually this is not RepLib and others style, because this strategy
-- is specific to *->*-datatypes. If a definition of crushright for
-- other kinds is needed, we would have to use a different strategy than
-- TL_red_cr2. In RepLib and company, the function definition stays the
-- same. --Alexey

module CrushRight (sizeListTree, flattenListTree, sumListTree) where

import TreeDatatype
import TreeDats

import Syb4A

data TL_red_cr2 w a b = TL_red_cr2 ([w]->w) (a->w) (b->w)


instance (GAPP (TL_red_cr2 w a b) spec (WTree a b) w w)
    => LDat (TL_red_cr2 w a b) spec (WTree a b) w where
  gin tlab@(TL_red_cr2 f fa fb) spec (Leaf a)  = f [fa a]
  gin tlab@(TL_red_cr2 f fa fb) spec (Fork tl tr) = 
      f [gapp tlab spec tl,
	 gapp tlab spec tr]
  gin tlab@(TL_red_cr2 f fa fb) spec (WithWeight t w)  = 
      f [gapp tlab spec t,
	 fb w]

data TL_red_cr1 w a = TL_red_cr1 ([w]->w) (a->w)

instance (GAPP (TL_red_cr1 w a) spec [a] w w)
    => LDat (TL_red_cr1 w a) spec [a] w where
  gin tlab@(TL_red_cr1 f fa) spec []  = f []
  gin tlab@(TL_red_cr1 f fa) spec (h:t) = 
      f [fa h, gapp tlab spec t]

-- count the leaves
sizeListTree t = gapp (TL_red_cr1 sum tree_red) HNil t  
 where
 tree_red :: WTree Int Int -> Int
 tree_red t = gapp (TL_red_cr2 sum (\ (x::Int) -> (1::Int))
		                   (\ (x::Int) -> (0::Int)))
	      HNil t


-- collect the Int values from the leaves of the tree
-- We don't just collect all Ints (because the tree may have Ints not only
-- at the leaves
collectIntLeaves :: forall a b w.
    (a -> w) -> w -> ([w] -> w) -> [WTree a b] -> w
collectIntLeaves inj nul red t = 
    gapp (TL_red_cr1 red tree_red) HNil t  
 where
 tree_red :: WTree a b -> w
 tree_red t = gapp (TL_red_cr2 red (\ (x::a) -> inj x)
		                   (\ (x::b) -> nul))
	      HNil t

flattenListTree t = collectIntLeaves (:[]) [] concat t
sumListTree t = collectIntLeaves id 0 sum t

example :: [WTree Int Int]
example = [WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
          ,WithWeight (Leaf 25 `Fork` Leaf 48) 2]

test1 = sizeListTree example
-- 4
test2 = flattenListTree example
-- [38,42,25,48]
test3 = sumListTree example
-- 153
