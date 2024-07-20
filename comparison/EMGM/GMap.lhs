> module GMap where

> import GL2(rList, functorRep)
> import BinTreeDatatype
> import BinTreeReps2
> import GMapDef(Gmap(Gmap,applyGmap))

> mapList            :: (a -> b) -> [a] -> [b]
> mapList f          = applyGmap (functorRep (Gmap f))

> mapListBTree       :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f     = applyGmap (rList (bintree (Gmap f)))

> mapListBTreeList   :: (a -> b) -> [BinTree [a]] -> [BinTree [b]] 
> mapListBTreeList f = applyGmap (rList (bintree (rList (Gmap f))))
