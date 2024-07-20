> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

> module Nested where

> import RepLib 
> import PerfectDatatype
> import Language.Haskell.TH
> import GEq

> $(derive 
>     [''Perfect,
>        ''Fork
>     ])

> instance (Eq a, Rep a) => Eq (Perfect a) where
>   (==) = eqR1 rep1
> instance (Eq a, Rep a) => Eq (Fork a) where
>   (==) = eqR1 rep1

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect = (==)
