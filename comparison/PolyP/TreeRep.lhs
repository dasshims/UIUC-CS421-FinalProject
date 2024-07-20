> {-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}
> module TreeRep where
> import PolyLib.Prelude
> import TreeDatatype(WTree(..))

The instance definition expresses that (WTree a) is a Regular,
one-parameter datatype (the parameter being the label type). If the
type arguments to WTree had been in the opposite order, we could
perhaps have used out to throw away the weight, but that would have
been an ugly hack (not working for gmap, for example).

> instance FunctorOf (SumF (ConstF a)
>                          (SumF (ProdF RecF RecF)
>                                (ProdF RecF ParF)))
>                    (WTree a) where
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
>   datatypeName _ = "WTree a"
