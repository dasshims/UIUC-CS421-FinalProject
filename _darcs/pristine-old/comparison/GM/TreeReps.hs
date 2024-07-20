module TreeReps where

import GM hiding (Tree,tree,fromTree,toTree,Leaf,Fork)
import TreeDatatype

-- Representation for the tree

instance (TypeRep a, TypeRep w) => TypeRep (Tree a w) where 
    typeRep = tree typeRep typeRep

fromTree (Leaf x)              =  Inl x
fromTree (Fork l r)            =  Inr (Inl (Pair l r))
fromTree (WithWeight a b)      =  Inr (Inr (Pair a b))

toTree (Inl x)                 =  Leaf x
toTree (Inr (Inl (Pair l r)))  =  Fork l r
toTree (Inr (Inr (Pair a b)))  =  WithWeight a b

tree                           :: (Generic g) => g a1 a2 -> g b1 b2 -> g (Tree a1 b1) (Tree a2 b2)
tree a b                       =  datatype  (Iso fromTree toTree) (Iso fromTree toTree)
                                         (a <+> tree a b <*> tree a b <+> tree a b <*> b)
