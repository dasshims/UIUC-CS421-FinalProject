> module RmWeights where

> import GL(GRep, over)
> import GL(GenericList)
> import TreeDatatype-- (WTree(..))
> import TreeReps(GenericWTree(wtree), rwtree)
> import GidDef(Gid(Gid,gid))

> instance GenericList  Gid -- default
> instance GenericWTree Gid where -- override
>   wtree a w = Gid (removeWeights a w)

> removeWeights :: Gid a -> Gid w -> WTree a w -> WTree a w
> removeWeights a w (WithWeight t _) = removeWeights a w t
> removeWeights a w t                = gid (rwtree a w) t

The last arm handles remaining constructor generically by
converting the top constructor to standard representation.

> rmWeights :: GRep Gid a => a -> a
> rmWeights = gid over

> rmWeightsWTree :: (GRep Gid w, GRep Gid a) => WTree a w -> WTree a w
> rmWeightsWTree = rmWeights

> rmWeightsListWTree :: [WTree Int Int] -> [WTree Int Int]
> rmWeightsListWTree = rmWeights
