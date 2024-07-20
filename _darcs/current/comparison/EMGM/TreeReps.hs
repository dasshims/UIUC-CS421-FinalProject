{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module TreeReps where

import GL hiding (Tree, fromTree, toTree, Fork, tree, isoTree, GenericTree(..))
import TreeDatatype
import Data.Generics hiding (Generic)
import qualified Data.Generics as DG

-- Representation for the tree

-- This instance allows the definition of ad-hoc cases
class Generic g => GenericWTree g where
  wtree :: g a -> g w -> g (WTree a w)
  wtree a w = rwtree a w

instance (GenericWTree g, GRep g a, GRep g w) => GRep g (WTree a w) where
    over = wtree over over

-- note that the representation must use "wtree" rather than "rwtree"
-- so that the children of the represented constructor uses the ad-hoc
-- case on them. In this way we can write a constructor ad-hoc case,
-- see RmWeights.lhs
rwtree :: GenericWTree g => g a -> g w -> g (WTree a w)
rwtree a w = view isoTree (constr "Leaf" 1 a <|>
                           constr "Fork" 2 (wtree a w <*> wtree a w) <|>
                           constr "WithWeight" 2 (wtree a w <*> w))

isoTree = Iso fromTree toTree

fromTree (Leaf x)              =  Inl x
fromTree (Fork l r)            =  Inr (Inl (l :*: r))
fromTree (WithWeight a b)      =  Inr (Inr (a :*: b))

toTree (Inl x)                 =  Leaf x
toTree (Inr (Inl (l :*: r)))   =  Fork l r
toTree (Inr (Inr (a :*: b)))   =  WithWeight a b
