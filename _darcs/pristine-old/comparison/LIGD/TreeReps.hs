module TreeReps where

import LIGD
import TreeDatatype

-- Representation for the tree

rTree :: Rep a -> Rep b -> Rep (Tree a b)
rTree ra rb = RType (App "Tree" [term ra, term rb])
                    (rSum (RCon "Leaf" ra)
                          (rSum (RCon "Fork" (rPair (rTree ra rb) (rTree ra rb)))
                                (RCon "WithWeight" (rPair (rTree ra rb) rb))))
                    (EP fromTree toTree)

rTree2 :: Rep2 f a b c -> Rep2 f a' b' c -> Rep2 f (Tree a a') (Tree b b') c
rTree2 ra rb
    = RType2 (App "Tree" [term2 ra])
             (rSum2 (RCon2 "Leaf" ra)
                    (rSum2 (RCon2 "Fork" (rPair2 (rTree2 ra rb) (rTree2 ra rb)))
                           (RCon2 "WithWeight" (rPair2 (rTree2 ra rb) rb))))
             (EP fromTree toTree)
             (EP fromTree toTree)


fromTree (Leaf a)            = Inl a
fromTree (Fork a b)          = Inr (Inl (a :*: b))
fromTree (WithWeight a b)    = Inr (Inr (a :*: b))

toTree (Inl a)               = Leaf a
toTree (Inr (Inl (a :*: b))) = Fork a b
toTree (Inr (Inr (a :*: b))) = WithWeight a b

