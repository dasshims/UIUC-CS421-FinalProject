> module GMapDef where

> import GL2(Generic(..), outl, outr, Iso(from,to),FunctorRep(..))
> import Data.Generics((:+:)(Inl, Inr), (:*:)((:*:)))
> -- import Data.Generics hiding (Generic)

> newtype Gmap a b    =  Gmap { applyGmap :: a -> b }

> instance Generic Gmap where
>   unit              =  Gmap (\ x -> x)
>   plus a b          =  Gmap (\ x -> case x of
>                                       Inl l -> Inl (applyGmap a l)
>                                       Inr r -> Inr (applyGmap b r))
>   prod a b          =  Gmap (\ x -> (applyGmap a (outl x)) :*: 
>                                     (applyGmap b (outr x)))
>   view iso1 iso2 a  =  Gmap (\ x -> to iso2 (applyGmap a (from iso1 x)))
>   char              =  Gmap (\ x -> x)
>   int               =  Gmap (\ x -> x)
>   float             =  Gmap (\ x -> x)

Easy to call version:

> gmap :: forall aT bT (fT :: * -> *). (FunctorRep fT) => (aT -> bT) -> fT aT -> fT bT
> gmap f = applyGmap (functorRep (Gmap f))

