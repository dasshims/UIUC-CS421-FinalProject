module BinTreeReps where

import GL2
import BinTreeDatatype
import Data.Generics hiding (Generic)

bintree a =  view isoBinTree isoBinTree (a <|> bintree a <*> bintree a)

isoBinTree = Iso fromBinTree toBinTree

fromBinTree (Leaf x)              =  Inl x
fromBinTree (Bin l r)             =  Inr (l :*: r)

toBinTree (Inl x)                 =  Leaf x
toBinTree (Inr (l :*: r))         =  Bin l r

instance FunctorRep BinTree where
   functorRep   =  bintree
