> import Reduce
> import TreeDatatype

> example :: [Tree Int Int]
> example = [WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
>           ,WithWeight (Leaf 25 `Fork` Leaf 48) 2]

> main = print ( sizeListTree example
>              , collectListTree example
>              , sumListTree example
>              )
