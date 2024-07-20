> {-# OPTIONS -fallow-overlapping-instances #-}

> module GEqTree (equalTree) where
> import PolyLib.Prelude
> import PolyLib.Equal
> import GEq
> import TreeDatatype

Extension of generic equality for |Tree|s

If we use the same definition as in GEq, then the comparison will look
at weights which is not the intended result.

> equalTree :: Tree Int Int -> Tree Int Int -> Bool
> equalTree = pequal (==)

The instance definition expresses that (Tree a) is a Regular,
one-parameter datatype (the parameter being the label type). If the
type arguments to Tree had been in the opposite order, we could
perhaps have used out to throw away the weight, but that would have
been an ugly hack (not working for gmap, for example).

> instance FunctorOf (SumF (ConstF a)
>                          (SumF (ProdF RecF RecF)
>                                (ProdF RecF ParF)))
>                    (Tree a) where
>   inn (InL (ConstF a))                  = Leaf a
>   inn (InR (InL (RecF l :*: RecF r)))   = Fork l r
>   inn (InR (InR (RecF t :*: ParF w)))   = WithWeight t w
>   out (Leaf a        ) = InL (ConstF a)
>   out (Fork l r      ) = InR (InL (RecF l :*: RecF r))
>   out (WithWeight t w) = InR (InR (RecF t :*: ParF w))
>   constructorFixity = const defaultFixity
>   constructorName (Leaf _)         = "Leaf"
>   constructorName (Fork _ _)       = "Fork"
>   constructorName (WithWeight t w) = "WithWeight"
>   datatypeName _ = "Tree a"

geqTree :: GRep Geq a => Tree a w -> Tree a w -> Bool
geqTree (WithWeight t1 _) t2 = geqTree t1 t2
geqTree t1 (WithWeight t2 _) = geqTree t1 t2
geqTree (Fork t1 t1') (Fork t2 t2') = geqTree t1 t2 && geqTree t1' t2'
geqTree (Leaf x) (Leaf y) = geq' over x y
geqTree _ _ = False
