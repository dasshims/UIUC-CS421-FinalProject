> module RmWeights where
> import TreeDatatype
> -- import TreeReps
> -- import Data.Generics.PlateData
> import Data.Generics.PlateDirect

> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree = transform f
>   where
>   f (WithWeight t w) = t
>   f x                = x

> {-
> rmWeightsListWTree :: [WTree Int Int] -> [WTree Int Int] 
> rmWeightsListWTree = transformBi f
> f :: WTree Int Int -> WTree Int Int
> f (WithWeight t w) = t
> f x                = x
> -}

> instance PlateOne (WTree a w) where
>     plateOne (WithWeight t w) = plate WithWeight |* t |- w
>     plateOne (Leaf x        ) = plate Leaf       |- x
>     plateOne (Fork l r      ) = plate Fork       |* l |* r
