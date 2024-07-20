> module FullTree where

> import Data.Generics
> import BinTreeDatatype

> errorMsg = error "Uniplate can't handle generic producer functions."

> genBinTree :: Int -> BinTree Char
> genBinTree = errorMsg

> genList :: Int -> [Char]
> genList = errorMsg