> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module RmWeights where

> import Data.Generics.SYB.WithClass.Basics
> import Traversals

> import TreeDatatype
> import TreeReps


> class CaseTree a where
>   caseTree :: a -> a

> instance CaseTree (WTree a b) where
>   caseTree (WithWeight t _) = t
>   caseTree t                = t

> data CaseTreeD a = CaseTreeD { caseTreeD :: a -> a }

> instance CaseTree a => Sat (CaseTreeD a)
>   where dict = CaseTreeD caseTree

> instance Data CaseTreeD a => CaseTree a where
>     caseTree x = x

> caseTreeCtx = undefined :: Proxy CaseTreeD


> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree = everywhere caseTreeCtx (caseTreeD dict)

