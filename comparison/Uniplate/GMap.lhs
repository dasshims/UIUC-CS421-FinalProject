%----------------------------------------------------------------------------
%
%  Title       :  GMap.lhs
%  Author(s)   :  Patrik Jansson
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> module GMap where
> import BinTreeDatatype

> errorNotSupported  = error "Uniplate.GMap: gmap not supported in Uniplate"

> mapList            :: (a -> b) -> [a] -> [b]
> mapList f          = errorNotSupported

> mapListBTree       :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f     = errorNotSupported

> mapListBTreeList   :: (a -> b) -> [BinTree [a]] -> [BinTree [b]] 
> mapListBTreeList f = errorNotSupported
