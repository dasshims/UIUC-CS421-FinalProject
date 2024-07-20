{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- A parameterised datatype for binary trees with data at the leafs

module TreeDats where

import Syb4A

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import TreeDatatype


-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually. The derivation is the straightforward
-- function of the definition of the WTree data type.

-- Syb4A already has instances for Int, Bool, Char, any array and a pair

instance (GAPP (TL_red w) spec a w w,
	  GAPP (TL_red w) spec b w w,
	  GAPP (TL_red w) spec (WTree a b) w w)
    => LDat (TL_red w) spec (WTree a b) w where
  gin tlab@(TL_red f) spec (Leaf a)  = f [gapp tlab spec a]
  gin tlab@(TL_red f) spec (Fork tl tr)  = f [gapp tlab spec tl,
					      gapp tlab spec tr]
  gin tlab@(TL_red f) spec (WithWeight t w)  = f [gapp tlab spec t,
						  gapp tlab spec w]

instance (GAPP TL_recon spec a da wa,
	  GAPP TL_recon spec b db wb,
	  GAPP TL_recon spec (WTree a b) (WTree wa wb) (WTree wa wb))
    => LDat TL_recon spec (WTree a b) (WTree wa wb) where
  gin tlab spec (Leaf a)     = Leaf (gapp tlab spec a)
  gin tlab spec (Fork tl tr) = Fork (gapp tlab spec tl) (gapp tlab spec tr)
  gin tlab spec (WithWeight t w)  = WithWeight (gapp tlab spec t)
				               (gapp tlab spec w)


instance (GAPP (TL_red_ctr w) spec a w w,
	  GAPP (TL_red_ctr w) spec b w w,
	  GAPP (TL_red_ctr w) spec (WTree a b) w w)
    => LDat (TL_red_ctr w) spec (WTree a b) w where
  gin tlab@(TL_red_ctr f) spec (Leaf a)  
      = f "Leaf" [gapp tlab spec a]
  gin tlab@(TL_red_ctr f) spec (Fork tl tr)
      = f "Fork" [gapp tlab spec tl,
		 gapp tlab spec tr]
  gin tlab@(TL_red_ctr f) spec (WithWeight t w)
      = f "WithWeight" [gapp tlab spec t,
			gapp tlab spec w]

