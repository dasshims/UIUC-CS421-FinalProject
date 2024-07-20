> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Efficiency where
> import GL
> import GEq
> import BinTreeReps
> import BinTreeDatatype
> import Data.Generics hiding (Generic)
> import FullTree


> bigeq :: Int -> Bool
> bigeq n = equalBinTreeChar t t
>   where t = genBinTree n

> equalBinTreeChar  :: BinTree Char -> BinTree Char -> Bool
> equalBinTreeChar  = geq' over

