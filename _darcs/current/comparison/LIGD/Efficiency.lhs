> module Efficiency where

> import LIGD
> import GEq
> import BinTreeReps
> import BinTreeDatatype
> import FullTree

> bigeq :: Int -> Bool
> bigeq n = equalBinTreeChar t t
>   where t = genBinTree n

> equalBinTree      :: Rep a -> BinTree a -> BinTree a -> Bool
> equalBinTree      = geq . rBinTree

> equalBinTreeChar  :: BinTree Char -> BinTree Char -> Bool
> equalBinTreeChar  = equalBinTree rChar
