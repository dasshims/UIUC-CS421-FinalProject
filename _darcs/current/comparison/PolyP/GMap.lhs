> module GMap where
> import PolyLib.Base(pmap)
> import BinTreeDatatype
> import BinTreeRep()

> mapList :: (a -> b) -> [a] -> [b]
> mapList f = pmap f

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f = pmap (pmap f)

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList f = pmap (pmap (pmap f))
