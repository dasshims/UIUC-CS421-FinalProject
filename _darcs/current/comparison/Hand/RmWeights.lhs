> module RmWeights where
> import TreeDatatype

> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree (WithWeight t w) = rmWeightsWTree t
> rmWeightsWTree (Leaf x)         = Leaf x
> rmWeightsWTree (Fork l r)       = Fork (rmWeightsWTree l) (rmWeightsWTree r)
