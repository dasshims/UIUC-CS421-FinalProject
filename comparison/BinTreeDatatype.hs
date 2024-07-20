{-# OPTIONS_GHC -fglasgow-exts #-}

module BinTreeDatatype where

import Data.Generics

data BinTree a = Leaf a | Bin (BinTree a) (BinTree a)
                 deriving Show


mytree :: BinTree Int
mytree = Bin (Bin (Leaf 32) (Leaf 3)) (Leaf 4)

mytree2 :: BinTree Int
mytree2 = Leaf 2

-- Constructors for larger BinTrees

mkBTree :: Int -> BinTree Int
mkBTree n | n <= 1    = Leaf n
          | even n    = Bin (Leaf n) (mkBTree (div n 2)) 
          | otherwise = Bin (mkBTree (3*n+1)) (mkBTree (n-1))

powerOfTwo :: Int -> BinTree a -> BinTree a
powerOfTwo 0     t = t
powerOfTwo (n+1) t = Bin (powerOfTwo n t) (powerOfTwo n t)

onebtree = mkBTree 15
bigbtree n = powerOfTwo n onebtree

sizeBTree (Leaf _)   = 1
sizeBTree (Bin l r)  = sizeBTree l + sizeBTree r

deepSeqBTree (Leaf x)  = seq x
deepSeqBTree (Bin l r) = deepSeqBTree l . deepSeqBTree r

eqBTree (Leaf x)  (Leaf y)     =  x == y
eqBTree (Bin l r) (Bin l' r')  =  eqBTree l l' && eqBTree r r'
eqBTree _         _            =  False

tweakRightmost :: BinTree Int -> BinTree Int
tweakRightmost (Leaf n)  = Leaf (n+1-1)
tweakRightmost (Bin l r) = Bin l (tweakRightmost r)

