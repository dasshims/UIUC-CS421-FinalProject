> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances -ddump-splices #-}

> module RmWeights where

> import RepLib(Rep, gmapT, mkT)
> import TreeDatatype(WTree(WithWeight))
> import TreeReps()

> rmWeights :: Rep a => a -> a
> rmWeights = gmapT (mkT rmAdhoc)
>   where
>   rmAdhoc :: WTree Int Int -> WTree Int Int
>   rmAdhoc (WithWeight t w) = rmAdhoc t
>     -- Constructor ad-hoc case
>   rmAdhoc t                = rmWeights t
>     -- Generic case

> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree = rmWeights
