> import GRoseDatatype
> import GEq (equalGRoseListInt)

----------------------------------------------------------------------------------


GEqGRose tests:
 * Multiple arguments (equality has two).
 * Support for higher kinded datatypes

> gRoseEx1, gRoseEx2 :: GRose [] Int
> gRoseEx1 = GRose 2 [GRose 1 [],GRose 2 []]
> gRoseEx2 = GRose 2 [GRose 2 [],GRose 1 []]

> main = print ( equalGRoseListInt gRoseEx1 gRoseEx2
>              , equalGRoseListInt gRoseEx1 gRoseEx1
>             )

