module TreeReps where

import LIGD
import TreeDatatype

-- Representation for the tree

-- General representation, with recursion abstracted
rTreeG :: (Rep a -> Rep b -> Rep (WTree a b)) ->
          Rep a -> Rep b -> Rep (WTree a b)
rTreeG rTree ra rb = RType (App "WTree" [term ra, term rb])
                    (rSum (RCon "Leaf" ra)
                          (rSum (RCon "Fork" (rPair (rTree ra rb) (rTree ra rb)))
                                (RCon "WithWeight" (rPair (rTree ra rb) rb))))
                    (EP fromTree toTree)

-- Normal representation, recursive point reveals structure (RType)
rTree = rTreeG rTree

-- Representation allowing ad-hoc definitions:
rTreeAdHoc x y = RWTree x y (EP id id)

-- Representation with RWTree at the recursive points
rTreeRec = rTreeG rTreeAdHoc

rTree2 :: Rep2 f a b -> Rep2 f a' b' -> Rep2 f (WTree a a') (WTree b b')
rTree2 ra rb
    = RType2 (App "WTree" [term2 ra])
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

