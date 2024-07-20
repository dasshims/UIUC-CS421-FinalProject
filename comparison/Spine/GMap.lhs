%----------------------------------------------------------------------------
%
%  Title       :  GMap.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  These tests cheat separate compilation. The BinTree 
%                 datatype is added to the type rep., so recompilation is 
%                 required
%
%----------------------------------------------------------------------------

> module GMap (mapList,mapListBTree,mapListBTreeList) where

> import SYB1(Type'(ListR'))
> import BinTreeDatatype
> import qualified SYB1 as Spine
> import SYB1 hiding (mapList)

> mapList :: (a -> b) -> [a] -> [b]
> mapList = Spine.map ListR'

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f = Spine.map ListR' $ Spine.map BinTreeR' f

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList f = Spine.map ListR' $ Spine.map BinTreeR' $ Spine.map ListR' f


