> {-# OPTIONS -fglasgow-exts #-}

> module GMap where

> import GL3
> -- import BinTreeDatatype
> -- import BinTreeReps
> import Data.Generics hiding (Generic)

> newtype Gmap a b c            =  Gmap { applyGmap :: a -> b } -- c is not used

> instance Generic Gmap where
>   unit                        =  Gmap (\ x -> x)
>   plus a b                    =  Gmap (\ x -> case x of
>                                                 Inl l -> Inl (applyGmap a l)
>                                                 Inr r -> Inr (applyGmap b r))
>   prod a b                    =  Gmap (\ x -> (applyGmap a (outl x)) :*: (applyGmap b (outr x)))
>   view iso1 iso2 _ a          =  Gmap (\ x -> to iso2 (applyGmap a (from iso1 x)))
>   char                        =  Gmap (\ x -> x)
>   int                         =  Gmap (\ x -> x)
>   float                       =  Gmap (\ x -> x)

> mapFunctor :: FunctorRep Gmap f => (a -> b) -> f a -> f b
> mapFunctor f = applyGmap (functorRep (Gmap f))

> {-
> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f = applyGmap (rList (bintree (Gmap f)))

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]] 
> mapListBTreeList f = applyGmap (rList (bintree (rList (Gmap f))))
> -}
