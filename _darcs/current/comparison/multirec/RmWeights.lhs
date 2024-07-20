\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module RmWeights where

import Generics.MultiRec
import TreeDatatype
import TreeReps

-- It is possible to generalize to arbitrary type vars instead of Int's
-- but one has to juggle a bit to make the type-checker happy.
rmWeightsWTree :: WTree Int Int -> WTree Int Int
rmWeightsWTree = go WTree
  where
    go :: Ix (WTreeU Int Int) t => (WTreeU Int Int) t -> t -> t
    go WTree (WithWeight t w) = go WTree t
    go _ x = compos go x

\end{code}
