module BinTreeReps where

import LIGD
import BinTreeDatatype

-- Representation for the tree

rBinTree :: Rep a -> Rep (BinTree a)
rBinTree ra = RType (App "BinTree" [term ra])
                         (rSum (RCon "Leaf" ra)
                               (RCon "Bin" (rPair (rBinTree ra) (rBinTree ra))))
						  (EP fromBinTree toBinTree)

rBinTree2 :: Rep2 f a b c -> Rep2 f (BinTree a) (BinTree b) c
rBinTree2 ra = RType2 (App "BinTree" [term2 ra])
                   (rSum2 (RCon2 "Leaf" ra)
                          (RCon2 "Bin" (rPair2 (rBinTree2 ra) (rBinTree2 ra))))
                   (EP fromBinTree toBinTree)
                   (EP fromBinTree toBinTree)


fromBinTree (Leaf a)        = Inl a
fromBinTree (Bin a b)       = Inr (a :*: b)

toBinTree   (Inl a)         = Leaf a
toBinTree   (Inr (a :*: b)) = Bin a b
