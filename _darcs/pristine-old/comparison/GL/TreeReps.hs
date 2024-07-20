{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module TreeReps where

import GL hiding (Tree, fromTree, toTree, Fork, tree, isoTree)
import TreeDatatype
import Data.Generics hiding (Generic)
import qualified Data.Generics as DG

-- Representation for the tree

instance (Generic g, GRep g a, GRep g w) => GRep g (Tree a w) where 
    over = tree over over

tree a w = view isoTree (constr "Leaf" 1 a <|> 
                         constr "Fork" 2 (tree a w <*> tree a w) <|>
                         constr "WithWeight" 2 (tree a w <*> w))

isoTree = Iso fromTree toTree

fromTree (Leaf x)              =  Inl x
fromTree (Fork l r)            =  Inr (Inl (l :*: r))
fromTree (WithWeight a b)      =  Inr (Inr (a :*: b))

toTree (Inl x)                 =  Leaf x
toTree (Inr (Inl (l :*: r)))   =  Fork l r
toTree (Inr (Inr (a :*: b)))   =  WithWeight a b