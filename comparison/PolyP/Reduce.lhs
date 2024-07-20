> module Reduce where

Two ways to fold ints from the tree defined in

> import TreeDatatype

> errorMsg = error "PolyP: Reduce test fails for WTree a w datatype."

> sizeListTree :: [WTree a w] -> Int
> sizeListTree = errorMsg

> collectListTree :: [WTree a w] -> [a]
> collectListTree = errorMsg

> sumListTree :: [WTree Int w] -> Int
> sumListTree = errorMsg

But note that folding is supported for Regular datatypes.
