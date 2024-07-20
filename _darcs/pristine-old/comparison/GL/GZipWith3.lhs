> {-# OPTIONS -fglasgow-exts #-}

> module GMap where

> import GL3
> import Data.Generics hiding (Generic)

> newtype GzipWith a b c        =  GzipWith { applyGzipWith :: a -> b -> c}

> instance Generic GzipWith where
>   unit                        =  GzipWith const
>   char                        =  GzipWith const
>   int                         =  GzipWith const
>   float                       =  GzipWith const
>   plus a b                    =  GzipWith (\x y -> case (x,y) of
>                                     (Inl l, Inl r) -> Inl (applyGzipWith a l r)
>                                     (Inr l, Inr r) -> Inr (applyGzipWith b l r)
>                                     _              -> error "ops!")
> 
>   prod a b                    =  GzipWith (\x y -> 
>                                     (applyGzipWith a (outl x) (outl y)) :*: 
>                                     (applyGzipWith b (outr x) (outr y)))
> 
>   view iso1 iso2 iso3 a       =  GzipWith (\x y -> to iso3
>                                     (applyGzipWith a (from iso1 x) (from iso2 y)))

> zipWithFunctor      :: FunctorRep GzipWith f => (a -> b -> c) -> f a -> f b -> f c
> zipWithFunctor f = applyGzipWith (functorRep (GzipWith f))
