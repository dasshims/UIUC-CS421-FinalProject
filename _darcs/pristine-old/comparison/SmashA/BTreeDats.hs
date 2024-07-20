{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- A parameterised datatype for binary trees with data at the leafs

module SmashA.BTreeDats where

import SmashA.Syb4A

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import BinTreeDatatype

-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually. The derivation is the straightforward
-- function of the definition of the BinTree data type.

-- Syb4A already has instances for Int, Bool, Char, any array and a pair

instance (GAPP TL_recon spec a dfa b,
	  GAPP TL_recon spec (BinTree a) (BinTree b) (BinTree b))
    => LDat TL_recon spec (BinTree a) (BinTree b) where
  gin tlab spec (Leaf a)  = Leaf (gapp tlab spec a)
  gin tlab spec (Bin l r) = Bin (gapp tlab spec l)
			        (gapp tlab spec r)

