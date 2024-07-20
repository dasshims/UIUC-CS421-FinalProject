> module GMap where
> import LIGD(rList2)
> import GMapDef(mapGRep)
> import BinTreeDatatype
> import BinTreeReps(rBinTree2)

> mapList          :: (a -> b) -> [a] -> [b]
> mapList          =  mapGRep rList2

> mapListBTree     :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree     =  mapGRep (rList2 . rBinTree2)

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList =  mapGRep (rList2 . rBinTree2 . rList2)

