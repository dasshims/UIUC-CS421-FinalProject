> import GEq (equalNGRoseListInt)
> import NGRoseDatatype

----------------------------------------------------------------------------------

GEqGRose tests:
 * Multiple arguments (equality has two).
 * Support for datatypes with nesting in their higher kinded arguments

> main = print ( equalNGRoseListInt ngrose1 ngrose1
>              , equalNGRoseListInt ngrose1 ngrose2
>             )

