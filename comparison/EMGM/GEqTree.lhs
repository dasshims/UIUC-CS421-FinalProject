> {-# OPTIONS_GHC -fallow-overlapping-instances #-}

> module GEqTree (equalWTree) where
> import GL hiding (Tree,tree,Fork)
> import GEq
> import TreeDatatype
> import TreeReps

Extension of generic equality for |WTree|s

> equalWTree :: WTree Int Int -> WTree Int Int -> Bool
> equalWTree = geq' over

> instance (GRep Geq a, GRep Geq w) => GRep Geq (WTree a w) where
>     over = Geq geqTree

I would like to write this so that it handles WithWeight only
and the rest remains generic, this is a constructor ad-hoc definition.
But the |tree| representation calls itself recursively.
To make it work the |tree| recursive calls should be
replaced to some form of |over|.

 geqTree :: Over (Geq a) => WTree a w -> WTree a w -> Bool
 geqTree t1 t2 = geq' gen_eq (unWeight t1) (unWeight t2)
   where
     gen_eq = tree over undefined
     unWeight (WithWeight t c) = t
     unWeight t = t

And now the plain code:

> geqTree :: GRep Geq a => WTree a w -> WTree a w -> Bool
> geqTree (WithWeight t1 _) t2 = geqTree t1 t2
> geqTree t1 (WithWeight t2 _) = geqTree t1 t2
> geqTree (Fork t1 t1') (Fork t2 t2') = geqTree t1 t2 && geqTree t1' t2'
> geqTree (Leaf x) (Leaf y) = geq' over x y
> geqTree _ _ = False
