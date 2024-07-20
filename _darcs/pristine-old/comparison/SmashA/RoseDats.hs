{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- A parameterised datatype for binary trees with data at the leafs

module SmashA.RoseDats where

import SmashA.Syb4A

import GRoseDatatype

-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually

-- Syb4A already has instances for Int, Bool, Char, any array and a pair

-- Again, cut and paste these constraints from the GHCi error message 
instance (GAPP (TL_red w) spec a w w,
	  GAPP (TL_red w) spec (f (GRose f a)) w w)
    => LDat (TL_red w) spec (GRose f a) w where
  gin tlab@(TL_red f) spec (GRose a body)  
      = f [gapp tlab spec a, gapp tlab spec body]


instance (GAPP (TL_red_lockstep w) spec (Couple a) w w,
	  GAPP (TL_red_lockstep w) spec (Couple (f (GRose f a))) w w)
    => LDat (TL_red_lockstep w) spec (Couple (GRose f a)) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (GRose a ba) (GRose b bb))
      = f [gapp tlab spec (Couple a b), 
	   gapp tlab spec (Couple ba bb)]
