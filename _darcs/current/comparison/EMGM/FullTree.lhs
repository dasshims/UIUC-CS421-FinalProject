> {-# OPTIONS_GHC -fglasgow-exts #-}

> module FullTree where

> import GL
> import BinTreeDatatype
> import BinTreeReps
> import TreeDatatype
> import TreeReps
> import Data.Generics hiding (Generic)

Implementation of gfulltree

> newtype FullTree a =  FullTree { fulltree' :: Int -> [a]  }

> instance Generic FullTree where
>   unit            =  FullTree (const [Unit])
>   char            =  FullTree (consumeDepth ['0'])
>   int             =  FullTree (consumeDepth [0])
>   float           =  FullTree (consumeDepth [0.0])
>   plus a b        =  FullTree (\ d -> map Inl (fulltree' a d) ++
>                                       map Inr (fulltree' b d))
>   prod a b        =  FullTree (\ d -> [ l :*: r
>                                       | l <-  fulltree' a d
>                                       , r <-  fulltree' b d])
>   constr _ _ a    =  FullTree (\ d -> case d of
>                                       0 -> []
>                                       _ -> fulltree' a (d-1) )
>   view iso a      =  FullTree (\ d -> map (to iso) (fulltree' a d))

Primitive types consume one depth unit
This hack would not be needed with a list-like view.

> consumeDepth ls 0 = []
> consumeDepth ls _ = ls


> instance GenericList  FullTree
> instance GenericWTree FullTree

> fulltree :: (GRep FullTree a) => Int -> [a]
> fulltree = fulltree' over

> genBinTree :: Int -> [BinTree Char]
> genBinTree = fulltree

> genList :: Int -> [[Char]]
> genList = fulltree

> genWTree :: Int -> [WTree Int Int]
> genWTree = fulltree
