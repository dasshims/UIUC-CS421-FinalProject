> import CrushRight(sizeListTree, flattenListTree, sumListTree)
> import TreeDatatype


The rules for this test:

* The generic crushRight function is purely datatype generic,
  that is, it relies solely on datatype structure,
  and contains no datatype-specific cases. So no WTree-specific case
  is allowed.
* Note that if the rule above is relaxed then SYB and SYB3 would pass the
  tests. But we are testing here whether we can choose the type constructor
  argument on which we perform some action. Here we do things on the |a|-values
  that are stored in |WTree a w|-values.


> example :: [WTree Int Int]
> example =  [ WithWeight (Leaf 38) 1 `Fork` WithWeight (Leaf 42) 2
>            , WithWeight (Leaf 25 `Fork` Leaf 48) 2]

> main = print ( sizeListTree    example
>              , flattenListTree example
>              , sumListTree     example
>              )
