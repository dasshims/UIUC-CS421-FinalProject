{-# OPTIONS_GHC -fglasgow-exts #-}
module BinTreeReps where

import GL
import BinTreeDatatype
import Data.Generics hiding (Generic)

bintree a =  view isoBinTree (constr "Leaf" 1 a <|> 
                              constr "Bin" 2 (bintree a <*> bintree a))

isoBinTree = Iso fromBinTree toBinTree

fromBinTree (Leaf x)              =  Inl x
fromBinTree (Bin l r)             =  Inr (l :*: r)

toBinTree (Inl x)                 =  Leaf x
toBinTree (Inr (l :*: r))         =  Bin l r

instance FunctorRep BinTree where
   functorRep   =  bintree

instance (Generic g, GRep g a) => GRep g (BinTree a) where
  over = bintree over
