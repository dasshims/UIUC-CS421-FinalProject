{-# OPTIONS_GHC -fglasgow-exts #-}
module BinTreeReps2 where

import GL2
import BinTreeDatatype
import Data.Generics hiding (Generic)

bintree :: Generic g => g t t1 -> g (BinTree t) (BinTree t1)
bintree a =  view isoBinTree isoBinTree (a <|> bintree a <*> bintree a)

isoBinTree :: Iso (BinTree t) (t :+: (BinTree t :*: BinTree t))
isoBinTree = Iso fromBinTree toBinTree

fromBinTree (Leaf x)              =  Inl x
fromBinTree (Bin l r)             =  Inr (l :*: r)

toBinTree (Inl x)                 =  Leaf x
toBinTree (Inr (l :*: r))         =  Bin l r

instance FunctorRep BinTree where
   functorRep   =  bintree

