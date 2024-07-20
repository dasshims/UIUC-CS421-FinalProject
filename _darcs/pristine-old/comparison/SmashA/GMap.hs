{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

-- Generic map transforms the elements of a given type constructor.

module SmashA.GMap (mapList, mapListBTree, mapListBTreeList) where

import BinTreeDatatype
import SmashA.BTreeDats

import SmashA.Syb4A hiding (geq)

import Data.Char

-- This is the genric map
gmap f x = gapp TL_recon (f :+: HNil) x

-- it's specializations. We use partial signatures
mapList_sig ::(a -> b) -> [a] -> [b]
mapList_sig = undefined

mapList f x | False = mapList_sig f x
mapList f x = gmap f x

mapListBTree_sig :: (a -> b) -> [BinTree a] -> [BinTree b]
mapListBTree_sig = undefined

mapListBTree f x | False = mapListBTree_sig f x
mapListBTree f x = gmap f x

mapListBTreeList_sig :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
mapListBTreeList_sig = undefined

mapListBTreeList f x | False = mapListBTreeList_sig f x
mapListBTreeList f x = gmap f x


example1 = [1,2,7,3,4]
example2 = [Leaf 1 `Bin` Leaf 7,Leaf 3 `Bin` Leaf 4]
example3 = [Leaf [1] `Bin` Leaf [2,7],Leaf [3] `Bin` Leaf [4]]

test1 = mapList toChar example1
test2 = mapListBTree toChar example2
test3 = mapListBTreeList toChar example3


toChar i = chr (i + ord 'A')