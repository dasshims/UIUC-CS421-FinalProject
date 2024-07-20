{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- A parameterised datatype for higher-ranked trees with data at the leafs
-- Very nested GRose datatype

module NGRoseDats where

import Syb4A

import NGRoseDatatype

-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually

-- Syb4A already has instances for Int, Bool, Char, any array and a pair

-- See the comments in PerfectDats.hs as to why we have to define a function
-- like red below

red:: TL_red_lockstep w
      -> Couple (NGRose f a)
      -> (Couple a -> w)
      -> (forall b. (Couple b -> w) -> Couple (f b) -> w)
      -> w
red tlab@(TL_red_lockstep _ h) (Couple (NGRose a ba) (NGRose b bb)) ra rf
      = h [ra (Couple a b), 
	   rf rb (Couple ba bb)]
 where
 -- rb :: Couple (NGRose (Comp f f) a) -> w
 rb x = red tlab x ra rf'
 -- rf' :: (Couple b -> w) -> Couple ((Comp f f) b) -> w
 rf' rb (Couple (Comp a) (Comp b)) = rf2 rb (Couple a b)
 -- rf2 :: (Couple b -> w) -> Couple (f (f b)) -> w
 rf2 ra x = rf (rf ra) x

-- Now, _implicitly_ NGRose has a constraint on 'f' being `functorial'
-- So, we need a way to traverse (f a) FORALL a. It seems impossible
-- to specify unversal constraints in a type class. Still, there is a way.

data ANY = ANY				-- Intended meaning: forall a

newtype Bar f w = Bar{unBar:: forall b. ((Couple b) -> w) -> Couple (f b) -> w}

data TL_red_lockstepH w = TL_red_lockstepH w ([w] -> w)

instance (GAPP (TL_red_lockstepH w) spec (Couple [ANY]) (Bar [] w) (Bar [] w))
    => LDat (TL_red_lockstepH w) spec (Couple [ANY]) (Bar [] w) where
    gin tlab@(TL_red_lockstepH d f) spec template = Bar bar
     where
     bar rb (Couple [] [])  = f []
     bar rb (Couple (x:xs) (y:ys))
	= f [rb (Couple x y),
	     unBar (gapp tlab spec template) rb (Couple xs ys)]
     bar _ _ = d

instance (GAPP (TL_red_lockstep w) spec (Couple a) w w,
	  GAPP (TL_red_lockstepH w) spec (Couple (f ANY)) (Bar f w) (Bar f w))
    => LDat (TL_red_lockstep w) spec (Couple (NGRose f a)) w where
  gin tlab@(TL_red_lockstep d f) spec x 
      = red tlab x (gapp tlab spec) 
	           (unBar $ gapp (TL_red_lockstepH d f) spec 
		                 (undefined::Couple (f ANY)))
