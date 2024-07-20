{-# LANGUAGE PatternSignatures, EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- WTree folding. X marks the spot -- only in the typechecker

module CrushRightX where -- (sizeListTree, flattenListTree, sumListTree) where

import TreeDatatype
import TreeDats

import Syb4A


-- Traversal with a type pattern
-- This is similar to SYB's trick of `X marks the spot'
-- The pattern is purely type level, and marks the places where
-- reduction should be performed.
newtype CoupleT ta a = CoupleT a
coupleT :: ta -> a -> CoupleT ta a
coupleT _ = CoupleT

data X -- X marks the spot
data Skip

instance LDat (TL_red w) spec (CoupleT X a) w where
    gin (TL_red f) spec x = f []

instance LDat (TL_red w) spec (CoupleT Skip a) w where
    gin (TL_red f) spec x = f []


instance (GAPP (TL_red w) spec (CoupleT ta a) w w, 
	  GAPP (TL_red w) spec (CoupleT tb b) w w)
    => LDat (TL_red w) spec (CoupleT (ta,tb) (a,b)) w where
    gin tlab@(TL_red f) spec (CoupleT (x,y)) = 
	f [gapp tlab spec (coupleT (undefined::ta) x), 
	   gapp tlab spec (coupleT (undefined::tb) y)]


instance (GAPP (TL_red w) spec (CoupleT [ta] [a]) w w, 
	  GAPP (TL_red w) spec (CoupleT ta a) w w)
    => LDat (TL_red w) spec (CoupleT [ta] [a]) w where
    gin tlab@(TL_red f) spec (CoupleT [])     = f []
    gin tlab@(TL_red f) spec (CoupleT (x:xs)) = 
	f [gapp tlab spec (coupleT (undefined::ta) x), 
	   gapp tlab spec (coupleT (undefined::[ta]) xs)]


-- Given list of pairs, reduce over the first components of the pairs
ex_fold1 :: forall a b w. (a -> w -> w) -> w -> [(a,b)] -> w
ex_fold1 red z t = gapp (TL_red ((foldr (.) id) :: [w->w]->(w->w)))
		        ((\(CoupleT x::CoupleT X a) _ -> red x) :+: HNil)
			(coupleT (undefined::[(X,Skip)]) t)
 			z

test_ex_fold1 = ex_fold1 (+) 0 [(1,2),(3,4)]
-- 4

instance (GAPP (TL_red w) spec (CoupleT ta a) w w, 
	  GAPP (TL_red w) spec (CoupleT tb b) w w,
	  GAPP (TL_red w) spec (CoupleT (WTree ta tb) (WTree a b)) w w)
    => LDat (TL_red w) spec (CoupleT (WTree ta tb) (WTree a b)) w where
  gin tlab@(TL_red f) spec (CoupleT (Leaf a))  = 
      f [gapp tlab spec (coupleT (undefined::ta) a)]
  gin tlab@(TL_red f) spec (CoupleT (Fork tl tr)) = 
      f [gapp tlab spec (coupleT (undefined::(WTree ta tb)) tl),
         gapp tlab spec (coupleT (undefined::(WTree ta tb)) tr)]
  gin tlab@(TL_red f) spec (CoupleT (WithWeight t w))  = 
      f [gapp tlab spec (coupleT (undefined::(WTree ta tb)) t),
	 gapp tlab spec (coupleT (undefined::tb) w)]

-- We define the following just for the heck of it. We don't need
-- it actually
crush1 :: forall f a t w. 
	  (LDat
           (TL_red (w -> w))
           (HCons (CoupleT X a -> t -> w -> w) HNil)
           (CoupleT (f X) (f a))
           (w -> w)) =>
	  (a -> w -> w) -> w -> f a -> w
crush1 red z t = gapp (TL_red ((foldr (.) id) :: [w->w]->(w->w)))
		        ((\(CoupleT x::CoupleT X a) (_::t) -> red x) :+: HNil)
			(coupleT (undefined::f X) t)
 			z
-- count the leaves
sizeListTree t = gapp (TL_red (foldr (.) id))
		      ((\(CoupleT x::CoupleT X Int) _ -> (+ 1)) :+: HNil)
		      (coupleT (undefined::[WTree X Skip]) t)
		      0

-- collect the Int values from the leaves of the tree
-- We don't just collect all Ints (because the tree may have Ints not only
-- at the leaves
collectIntLeaves inj red t = 
    gapp (TL_red red)
	 ((\(CoupleT x::CoupleT X Int) _ -> (inj x)) :+: HNil)
         (coupleT (undefined::[WTree X Skip]) t)

flattenListTree t = collectIntLeaves (:[]) concat t
sumListTree t = collectIntLeaves id sum t

example :: [WTree Int Int]
example = [WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
          ,WithWeight (Leaf 25 `Fork` Leaf 48) 2]

test1 = sizeListTree example
-- 4
test2 = flattenListTree example
-- [38,42,25,48]
test3 = sumListTree example
-- 153

