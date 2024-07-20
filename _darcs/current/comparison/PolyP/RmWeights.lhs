> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module RmWeights where
> import PolyLib.Prelude
> import PolyLib.Equal
> import PolyLib.Base
> import TreeDatatype
> import TreeRep

> mapChildren ::  (FunctorOf a b, P_fmap2 a) => (b c -> b c) -> b c -> b c
> mapChildren f = inn . fmap2 id f . out

Note that there is no rmWeights function, this one is specific to WTree's only.
However, it uses generic functionality to traverse the other constructors.
So, while it does not offer the full functionality, it is useful in practice when
there is a single regular datatype involved.

> rmWeightsWTree :: (FunctorOf a (WTree x), P_fmap2 a) => WTree x y -> WTree x y
> rmWeightsWTree (WithWeight t' w) = rmWeightsWTree t'
> rmWeightsWTree t                 = mapChildren rmWeightsWTree t


