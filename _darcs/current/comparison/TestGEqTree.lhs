Here we test extensibility of generic functions.

The test must use the generic equality defined in GEq
and extend it with a case for |WTree|s.

> import GEqTree (equalWTree)
> import TreeDatatype

----------------------------------------------------------------------------------

> main = print ( equalWTree mytree mytree2
>              , equalWTree mytree mytree3
>              )

