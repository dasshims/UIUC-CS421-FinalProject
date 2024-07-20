> module GMap (mapList,mapListBTree,mapListBTreeList) where

> import BinTreeDatatype

> errorNotSupported = error "syb3.GMap: gmap cannot be implemented in SYB3"

> mapList :: (a -> b) -> [a] -> [b]
> mapList = errorNotSupported

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree = errorNotSupported

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList = errorNotSupported


