> module GEqTree (equalWTree) where
> import LIGD
> import GEq
> import TreeDatatype
> import TreeReps

Extension of generic equality for |WTree|s

By defining a "forgetting embedding" of trees with weights onto trees
without weights we can implement the desired function. (But note that
the embedding-projection pair is not an iso.)

> equalWTree :: WTree Int Int -> WTree Int Int -> Bool
> equalWTree = geq (rTreeF rInt rInt)

> rTreeF :: Rep a -> Rep b -> Rep (WTree a b)
> rTreeF ra rb = RType (App "WTree" [term ra, term rb])
>                      (rSum (RCon "Leaf" ra)
>                            (RCon "Fork" (rPair (rTreeF ra rb) (rTreeF ra rb))))
>                      (EP fromTreeF toTreeF)

> fromTreeF (Leaf a)            = Inl a
> fromTreeF (Fork l r)          = Inr (l :*: r)
> fromTreeF (WithWeight t w)    = fromTreeF t
>
> toTreeF (Inl a)         = Leaf a
> toTreeF (Inr (l :*: r)) = Fork l r

Another variant ignoring the values of the weights. But this variant
still requires the structures to match (thus fails the test).

> equalWTree' :: WTree Int Int -> WTree Int Int -> Bool
> equalWTree' = geq (rTree rInt rForgetInt)

> rForgetInt :: Rep Int
> rForgetInt = RInt $ EP (const 0) undefined




