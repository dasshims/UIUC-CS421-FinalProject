> module Reduce where

> import Data.Generics
> import Debug.Trace

Two ways to fold ints from the tree defined in

> import BinTreeDatatype
> import TreeDatatype
> import CompanyDatatypes

Unfortunately, the function below would also (incorrectly)
sum the elements in the weights part if these contained trees.

> {-
> sumElements :: forall b.(Data b) => Tree Int b -> Int
> sumElements = everything (+) ((0::Int) `mkQ` fromLeaf)
>   where
>     -- Horribly confusing error if the type signature is omitted
>     fromLeaf :: Tree Int b -> Int
>     fromLeaf (Leaf x) = x
>     fromLeaf _ = 0

> foldTree :: forall a c.(Data a,Data c)
>          => (c -> c -> c) -> c -> Tree c a -> c
> foldTree op b = everything op (b `mkQ` fromLeaf)
>   where
>     fromLeaf :: Tree c a -> c
>     fromLeaf (Leaf x) = x
>     fromLeaf _ = b
> -}

Cheating a bit, the other functions don't require a type class context!!
How to fix it? force the functions to have more specific types?

The function below is wrong because if we write "sizeListWrong [[1],[2,3]]"
it will return 5. That is because now "a" is "[Int]", this means that it
will count all values of this type, including all cons *and* nil.

> sizeListWrong :: forall a.(Data a) => [a] -> Int
> sizeListWrong = everything (+) (0 `mkQ` from)
>   where
>     from (x::a) = 1

Generic collect function: it counts the occurrences of 'a' values inside
a 'b' value. It does so by counting the number of 'a's at the top-level
and then it recurses over the non-'a' values. The result is the sum
of this toplevel (with recursion into non-'a's) query map.

> gen_collect :: forall a b.(Data a,Data b) => a -> b -> [a]
> gen_collect type_proxy
>   = concat . gmapQ (gen_collect type_proxy `extQ` collectElem)
>   where
>     collectElem :: a -> [a]
>     collectElem x = [x]


> collectListTree :: forall a w.(Data a,Data w) => [Tree a w] -> [a]
> collectListTree = gen_collect (undefined :: a)

> sizeListTree :: forall a w.(Data a,Data w) => [Tree a w] -> Int
> sizeListTree = length . collectListTree

> sumListTree :: forall w.(Data w) => [Tree Int w] -> Int
> sumListTree = sum . collectListTree
