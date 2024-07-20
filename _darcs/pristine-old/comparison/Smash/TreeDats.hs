{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- A parameterised datatype for binary trees with data at the leafs

module Smash.TreeDats where

import Smash.Syb4

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import TreeDatatype


-- we do need however the instances for our version of Data (which
-- we call Dat). Unlike Data, Dat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually

-- Syb4 already has instances for Int, Bool, Char, any array and a pair

-- Again, cut and paste these constraints from the GHCi error message 
instance (SApply spec (Tree a w), SApply spec a, SApply spec w,
	 Dat spec a, Dat spec w)
    => Dat spec (Tree a w) where
  genmapq spec reducer (Leaf a)  = reducer [gmapq spec reducer a]
  genmapq spec reducer (Fork tl tr)  = reducer [gmapq spec reducer tl,
						gmapq spec reducer tr]
  genmapq spec reducer (WithWeight t w)  = reducer [gmapq spec reducer t,
						    gmapq spec reducer w]


