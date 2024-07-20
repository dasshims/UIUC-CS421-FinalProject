{-# OPTIONS -fglasgow-exts #-}

module BinTreeDatatype where

import Data.Generics

data BinTree a = Leaf a | Bin (BinTree a) (BinTree a)
                 deriving (Show, Typeable, Data)


mytree :: BinTree Int
mytree = Bin (Bin (Leaf 32) (Leaf 3)) (Leaf 4)

