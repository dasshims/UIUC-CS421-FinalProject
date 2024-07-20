> module GidDef where
> import GL(Generic(..), outl, outr, Iso(from,to))
> import Data.Generics((:+:)(Inl, Inr), (:*:)((:*:)))

> newtype Gid a   =  Gid { gid :: a -> a }

> instance Generic Gid where
>   unit        =  Gid (\ x -> x)
>   plus a b    =  Gid (\ x -> case x of
>                                 Inl l -> Inl (gid a l)
>                                 Inr r -> Inr (gid b r))
>   prod a b    =  Gid (\ x -> (gid a (outl x)) :*: (gid b (outr x)))
>   view iso a  =  Gid (\ x -> to iso (gid a (from iso x)))
>   char        =  Gid (\ x -> x)
>   int         =  Gid (\ x -> x)
>   float       =  Gid (\ x -> x)

