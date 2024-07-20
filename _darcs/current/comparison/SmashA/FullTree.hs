{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- Generating trees and lists with sample data
-- We use the same strategy as in Syb4ABuild.hs -- traversing
-- undefined. The only change is that our traversal strategy parameterized
-- by the number that controls the choice in a sum

-- Of course a more general approach is to make the TL_reconM strategy lazy
-- (or, to make TLrecon_lazy monadic). We could use that strategy for 
-- complete deserialization, of both structure and content. 

-- **TO FIX**
-- This traversal is too specific to the task at hand, namely the
-- generator. Indeed, this is not a real generic function, it is
-- just a non-generic generator encoded in Smash style.
-- A generic function must be based on a basic traversal.
-- --Alexey


module FullTree (genBinTree, genList) where

import Syb4A

import BTreeDats
import BinTreeDatatype


genBinTree :: Int -> BinTree Char
genBinTree = gendepth

genList :: Int -> [Char]
genList = gendepth

__ = error "nonexistent"
gendepth depth = r 
  where r = gapp (TL_recon_lazy_depth depth) lst (__ `asTypeOf` r)
	-- Generated elements for atomic or other such data
        lst = k (0::Int) :+: k False :+: k '0' :+: k (0.0::Float)
	      :+: HNil
        k:: t -> t -> t -> t
	k x _ _ = x

-- Depth influences the choice of the left or right branch in a sum
-- Depth is decremented for products
newtype TL_recon_lazy_depth = TL_recon_lazy_depth Int

instance LDat TL_recon_lazy_depth spec Int Int where
    gin _ spec x = x
instance LDat TL_recon_lazy_depth spec Char Char where
    gin _ spec x = x
instance LDat TL_recon_lazy_depth spec Bool Bool where
    gin _ spec x = x

instance (GAPP TL_recon_lazy_depth spec a dfa wa, 
	  GAPP TL_recon_lazy_depth spec b dfb wb)
    => LDat TL_recon_lazy_depth spec (a,b) (wa,wb) where
    gin (TL_recon_lazy_depth n) spec ~(x,y) =
	let tlab = TL_recon_lazy_depth (pred n)
	in (gapp tlab spec x, gapp tlab spec y)

-- (semi-)sums
instance (GAPP TL_recon_lazy_depth spec a df w)
    => LDat TL_recon_lazy_depth spec (Maybe a) (Maybe w) where
    gin tlab@(TL_recon_lazy_depth n) spec v =
	if n > 0
	   then let ~(Just x) = v in Just (gapp tlab spec x)
	   else Nothing

instance (GAPP TL_recon_lazy_depth spec [a] [w] [w], 
	  GAPP TL_recon_lazy_depth spec a dfa w)
    => LDat TL_recon_lazy_depth spec [a] [w] where
    gin (TL_recon_lazy_depth n) spec x =
	if n > 0
	   then let tlab = TL_recon_lazy_depth (pred n)	
		in (gapp tlab spec (head x)):(gapp tlab spec (tail x))
	   else []

instance (GAPP TL_recon_lazy_depth spec a dfa b,
	  GAPP TL_recon_lazy_depth spec (BinTree a) (BinTree b) (BinTree b))
    => LDat TL_recon_lazy_depth spec (BinTree a) (BinTree b) where
    gin (TL_recon_lazy_depth n) spec v =
        if n > 0 
	   then Bin (gapp tlab spec l) (gapp tlab spec r)
	   else Leaf (gapp tlab spec a)
	where
	tlab = TL_recon_lazy_depth (pred n)
	Bin l r = v
	Leaf a  = v

