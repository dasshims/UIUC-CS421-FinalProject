> module GMap where

> import GM
> import BinTreeDatatype
> import BinTreeReps

> newtype Gmap a b              =  Gmap { applyGmap :: a -> b }

> instance Generic Gmap where
>   unit                        =  Gmap (\ x -> x)
>   plus a b                    =  Gmap (\ x -> case x of
>                                                 Inl l -> Inl (applyGmap a l)
>                                                 Inr r -> Inr (applyGmap b r))
>   pair a b                    =  Gmap (\ x -> Pair (applyGmap a (outl x)) (applyGmap b (outr x)))
>   datatype iso1 iso2 a        =  Gmap (\ x -> toData iso2 (applyGmap a (fromData iso1 x)))
>   char                        =  Gmap (\ x -> x)
>   int                         =  Gmap (\ x -> x)
>   float                       =  Gmap (\ x -> x)

> mapList :: (a -> b) -> [a] -> [b]
> mapList f = applyGmap (functorRep (Gmap f))

> mapListBTree :: (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f = applyGmap (list (bintree (Gmap f)))

> mapListBTreeList :: (a -> b) -> [BinTree [a]] -> [BinTree [b]] 
> mapListBTreeList f = applyGmap (list (bintree (list (Gmap f))))
