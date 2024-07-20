> module GMap where

> import LIGD
> import BinTreeDatatype
> import BinTreeReps
> import CompanyDatatypes
> import CompanyReps

Generic map transforms the elements of a given type constructor.
The location of the elements in the type are given by RVar2 constructors
from the type representations. The transforming functions that are embedded
in those constructors might also change the element type.

The third type is a non-generic type variable that we don't need, so
we just don't mention it.

> newtype GMap a b c = GMap { gMap :: a -> b }

> mapG :: Rep2 GMap a b c -> GMap a b c

> mapG (RVar2 f) = f
> mapG (RSum2 rA rB ep1 ep2)
>   = GMap (\ t ->
>           case from ep1 t of
>           Inl t' -> to ep2 (Inl (gMap (mapG rA) t'))
>           Inr t' -> to ep2 (Inr (gMap (mapG rB) t'))
>          )


> mapG (RPair2 rA rB ep1 ep2)
>   = GMap (\ t ->
>           case from ep1 t of
>           x :*: y -> to ep2 $
>                      gMap (mapG rA) x
>                      :*:
>                      gMap (mapG rB) y
>          )
> mapG (RType2 e rA ep1 ep2)
>   = GMap (\ t -> to ep2 (gMap (mapG rA) (from ep1 t)))
> mapG (RCon2 nm rA)
>   = mapG rA

The remaining cases could be improved: written in just one arm.

> mapG (RInt2 ep1 ep2)
>   = GMap (to ep2 . from ep1)
> mapG (RFloat2 ep1 ep2)
>   = GMap (to ep2 . from ep1)
> mapG (RChar2 ep1 ep2)
>   = GMap (to ep2 . from ep1)
> mapG (RDynamic2 ep1 ep2)
>   = GMap (to ep2 . from ep1)
> mapG (RUnit2 ep1 ep2)
>   = GMap (to ep2 . from ep1)

> mapGRep :: (Rep2 GMap a b e -> Rep2 GMap c d e) -> (a -> b) -> c -> d
> mapGRep rep f = gMap (mapG (rep (RVar2 (GMap f))))

> mapList :: (a -> b) -> [a] -> [b]
> mapList = mapGRep rList2

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree = mapGRep (rList2 . rBinTree2)

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList = mapGRep (rList2 . rBinTree2 . rList2)
