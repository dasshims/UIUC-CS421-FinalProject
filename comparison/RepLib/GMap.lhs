> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances -ddump-splices #-}

{----------------------------------------------------------------------------

 Module      :  GMap
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD
 
 This test implements the generic map function.

----------------------------------------------------------------------------}

> module GMap where

> import RepLib --hiding (rConsEmb)
> import BinTreeDatatype
> import BinTreeReps
> import Language.Haskell.TH
> import R2

Definition of generic map function

> mapR2 :: R2 (->) a b -> a -> b
> mapR2 (Data2 rdt cons) = \ x ->
>                               let loop (Con2 rcd1 rcd2 ps : rest) = case from rcd1 x of
>                                     Just a -> to rcd2 (mapRL2 ps a)
>                                     Nothing -> loop rest
>                               in loop cons
> mapR2 Int2 = id
> mapR2 Char2 = id
> mapRL2 :: MTup2 (->) l1 l2 -> l1 -> l2
> mapRL2 MNil2 Nil = Nil
> mapRL2 (f :**: rs) (a :*: l) = f a :*: mapRL2 rs l

> instance Functor BinTree where
>   fmap f = mapR2 (rBinTree2 f (fmap f))

Some test functions

> mapList :: (a -> b) -> [a] -> [b]
> mapList = fmap

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree = fmap . fmap

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList f = fmap $ fmap $ fmap f
