> module Reduce where

Two ways to fold ints from the tree defined in

> import TreeDatatype

> errorMsg = error "PolyP: Reduce test fails for Tree a w datatype."

> sizeListTree :: [Tree a w] -> Int
> sizeListTree = errorMsg

> collectListTree :: [Tree a w] -> [a]
> collectListTree = errorMsg

> sumListTree :: [Tree Int w] -> Int
> sumListTree = errorMsg

But note that folding is supported for Regular datatypes.
