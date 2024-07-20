> module Reduce where

> import Data.Generics
> import Data.Generics.PlateData
> import TreeDatatype


> sizeListTree :: Data w => [WTree Int w] -> Int
> sizeListTree = length . collectListTree

Type restriction is required as Leaf y is of type Leaf (::y) ?, and the ?
means its not a single type traversal. In reality it is, so we can fix
this with an explicit asTypeOf.

> collectListTree :: (Data a, Data w) => [WTree a w] -> [a]
> collectListTree x = [y | Leaf y <- asTypeOf (universeBi x) x]

> sumListTree :: Data w => [WTree Int w] -> Int
> sumListTree = sum . collectListTree

