module BinTreeReps where

import GM hiding  (Leaf)
import BinTreeDatatype


bintree                           :: (Generic g) => g a1 a2 -> g (BinTree a1) (BinTree a2)
bintree a                         =  datatype  (Iso fromBinTree toBinTree) (Iso fromBinTree toBinTree)
                                            (a <+> bintree a <*> bintree a)

instance (TypeRep a) => TypeRep (BinTree a) where 
    typeRep = bintree typeRep

instance FunctorRep BinTree where
    functorRep =  bintree

fromBinTree (Leaf x)              =  Inl x
fromBinTree (Bin l r)             =  Inr (Pair l r)

toBinTree (Inl x)                 =  Leaf x
toBinTree (Inr (Pair l r))        =  Bin l r
