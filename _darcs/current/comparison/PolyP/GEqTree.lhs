{-# OPTIONS_GHC -fallow-overlapping-instances #-}

> module GEqTree (equalWTree) where
> import PolyLib.Prelude
> import PolyLib.Equal
> import GEq
> import TreeDatatype
> import TreeRep()

Extension of generic equality for |WTree|s

If we use the same definition as in GEq, then the comparison will look
at weights which is not the intended result.

> equalWTree :: WTree Int Int -> WTree Int Int -> Bool
> equalWTree = pequal (==)

