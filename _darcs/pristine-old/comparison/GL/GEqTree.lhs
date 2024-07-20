> {-# OPTIONS -fallow-overlapping-instances #-}

> module GEqTree (equalTree) where
> import GL hiding (Tree,tree,Fork)
> import GEq
> import TreeDatatype
> import TreeReps

Extension of generic equality for |Tree|s

> equalTree :: Tree Int Int -> Tree Int Int -> Bool
> equalTree = geq' over

> instance (GRep Geq a, GRep Geq w) => GRep Geq (Tree a w) where
>     over = Geq geqTree

I would like to write this so that it handles WithWeight only
and the rest remains generic, this is a constructor ad-hoc definition.
But the |tree| representation calls itself recursively.
To make it work the |tree| recursive calls should be
replaced to some form of |over|.

 geqTree :: Over (Geq a) => Tree a w -> Tree a w -> Bool
 geqTree t1 t2 = geq' gen_eq (unWeight t1) (unWeight t2)
   where
     gen_eq = tree over undefined
     unWeight (WithWeight t c) = t
     unWeight t = t

And now the plain code:

> geqTree :: GRep Geq a => Tree a w -> Tree a w -> Bool
> geqTree (WithWeight t1 _) t2 = geqTree t1 t2
> geqTree t1 (WithWeight t2 _) = geqTree t1 t2
> geqTree (Fork t1 t1') (Fork t2 t2') = geqTree t1 t2 && geqTree t1' t2'
> geqTree (Leaf x) (Leaf y) = geq' over x y
> geqTree _ _ = False
