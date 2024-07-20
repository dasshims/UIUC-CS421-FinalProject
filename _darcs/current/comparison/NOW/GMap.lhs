{-#  OPTIONS -fglasgow-exts  #-}

> module GMap where

> import qualified NOW
> import BinTreeDatatype

> instance Functor BinTree where
>   fmap f (Leaf x)   =  Leaf (f x)
>   fmap f (Bin l r)  =  Bin (fmap f l) (fmap f r)

> mapList = NOW.mapList

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree = fmap . fmap

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]] 
> mapListBTreeList = fmap . fmap . fmap
