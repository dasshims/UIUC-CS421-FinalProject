module TreeReps2 where

import GL2
import TreeDatatype
import Data.Generics hiding (Generic)
import qualified Data.Generics as DG

-- Representation for the tree

tree a w = view isoTree
                isoTree
                (a <|>
                 (tree a w <*> tree a w) <|>
                 (tree a w <*> w))

isoTree = Iso fromTree toTree

fromTree (Leaf x)              =  Inl x
fromTree (Fork l r)            =  Inr (Inl (l :*: r))
fromTree (WithWeight a b)      =  Inr (Inr (a :*: b))

toTree (Inl x)                 =  Leaf x
toTree (Inr (Inl (l :*: r)))   =  Fork l r
toTree (Inr (Inr (a :*: b)))   =  WithWeight a b
