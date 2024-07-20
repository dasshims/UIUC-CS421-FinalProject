> module CrushRight3 where

> import GL3
> import BinTreeDatatype
> import BinTreeReps3
> import Data.Generics hiding (Generic)

> newtype CrushRight b a x y  =  CrushRight { crushRight :: a -> b -> b }
>
> instance Generic (CrushRight b) where
>   unit                  =  CrushRight (\ _ e -> e)
>   plus a b              =  CrushRight (\ x e ->  case x of
>                                               Inl l  ->  crushRight a l e
>                                               Inr r  ->  crushRight b r e)
>   prod a b              =  CrushRight (\ x e  ->  (crushRight a (outl x)
>                                                     (crushRight b (outr x) e)
>                                                     ))
>   view iso1 iso2 iso3 a =  CrushRight (\ x e ->  crushRight a (from iso1 x) e)
>   char                  =  CrushRight (\ _ e  ->  e)
>   int                   =  CrushRight (\ _ e  ->  e)
>   float                 =  CrushRight (\ _ e  ->  e)

> crushFunctor :: FunctorRep (CrushRight b) f => (a -> b -> b) -> f a -> b -> b
> crushFunctor f = crushRight (functorRep (CrushRight f))



