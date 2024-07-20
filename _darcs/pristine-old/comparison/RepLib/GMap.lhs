> {-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances -ddump-splices #-}

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
> import Language.Haskell.TH

Definition of arity 2 GADT type representation

> data R2 c a b where
>   Int2   :: R2 c Int Int
>   Char2  :: R2 c Char Char
>   Data2  :: String -> [Con2 c a b] -> R2 c a b

> data Con2 c a b    = forall l1 l2. Con2 (Emb l1 a) (Emb l2 b) (MTup2 c l1 l2)
> data MTup2 c l1 l2 where
>   MNil2  :: MTup2 c Nil Nil
>   (:**:) :: c a b -> MTup2 c l1 l2 -> MTup2 c (a :*: l1) (b :*: l2)
> infixr 7 :**:

Type rep of a binary tree

> rBinTree2 :: forall a b c.c a b -> c (BinTree a) (BinTree b) 
>                                 -> R2 c (BinTree a) (BinTree b)
> rBinTree2 a t = Data2 "BinTree" [Con2 rLeafEmb rLeafEmb (a :**: MNil2),
>                                  Con2 rBinEmb rBinEmb (t :**: t :**: MNil2)]
> rLeafEmb :: Emb (a :*: Nil) (BinTree a)
> rLeafEmb = Emb { to   = \ (a :*: Nil) -> (Leaf a),
>                  from = \ x -> case x of 
>                           Leaf a -> Just (a :*: Nil)
>                           _      -> Nothing }
> rBinEmb :: Emb (BinTree a :*: BinTree a :*: Nil) (BinTree a)
> rBinEmb = Emb { to   = \ (l :*: r :*: Nil) -> (Bin l r),
>                 from = \ x -> case x of
>                          Bin l r -> Just (l :*: r :*: Nil)
>                          _       -> Nothing}

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
