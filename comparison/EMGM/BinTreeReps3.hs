{-# OPTIONS_GHC -fglasgow-exts #-}
module BinTreeReps3 where

import GL3
import BinTreeDatatype
import Data.Generics hiding (Generic)

bintree :: Generic g => g t t1 t2 -> g (BinTree t) (BinTree t1) (BinTree t2)
bintree a =  view isoBinTree isoBinTree isoBinTree (a <|> bintree a <*> bintree a)

isoBinTree :: Iso (BinTree t) (t :+: (BinTree t :*: BinTree t))
isoBinTree = Iso fromBinTree toBinTree

fromBinTree (Leaf x)              =  Inl x
fromBinTree (Bin l r)             =  Inr (l :*: r)

toBinTree (Inl x)                 =  Leaf x
toBinTree (Inr (l :*: r))         =  Bin l r

instance Generic g => FunctorRep g BinTree where
   functorRep   =  bintree


