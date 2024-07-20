{-# OPTIONS_GHC -fglasgow-exts  #-}

-- Generic map transforms the elements of a given type constructor.

module GMap (mapList, mapListBTree, mapListBTreeList) where

import BinTreeDatatype
import BTreeDats

import Syb4A hiding (geq)

import Data.Char

-- This is the generic map
gmap f x = gapp TL_recon (const . f :+: HNil) x

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

example2b = [Leaf True `Bin` Leaf False,Leaf True `Bin` Leaf False]
test2b = gmap (\(x::Bool) -> fromEnum x) example2b

-- Test of the _exclusion_ of data types from the domain of a generic function
--
-- To exclude a particular instance of a polymorphic data type, e.g.,
-- [a], we can define an (overlapping) instance for that particular list,
-- referring to an unsatisfiable constraint. It is less straightforward
-- how to exclude a base data type from the domain of a generic function.
-- It is possible, still. The following shows an example of excluding Booleans
-- from the domain of a generic function, gmap in this example. 
-- The function gmapNoBool works as gmap on data structures that contain
-- no booleans. However, at attempt of generic processing on the data structure
-- with booleans gives a type error (Bool cannot be unified with Vetoed).

data Vetoed = Vetoed
gmapNoBool f x = gapp TL_recon ((\ (x::Bool) Vetoed -> Vetoed) :+:
				const . f :+: HNil) x
testnb1 = gmapNoBool toChar example1
testnb2 = gmapNoBool toChar example2
testnb3 = gmapNoBool toChar example3

-- The following give the type error
-- test2b4 = gmapNoBool (\(x::Bool) -> fromEnum x) example2b
-- test2b5 = gmapNoBool toChar example2b

