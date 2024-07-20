> module UpdateSalaryDef where

> import GL(Generic(..), GenericList, outl, outr, Iso(from,to))
> import Data.Generics((:+:)(Inl, Inr), (:*:)((:*:)))

> newtype UpdateSalary a  =  UpdateSalary { updateSalary :: Float -> a -> a }

> instance Generic UpdateSalary where
>   unit        =  UpdateSalary (\ f x -> x)
>   plus a b    =  UpdateSalary (\ f x -> case x of
>                            Inl l -> Inl (updateSalary a f l)
>                            Inr r -> Inr (updateSalary b f r))
>   prod a b    =  UpdateSalary (\ f x -> (updateSalary a f (outl x))
>                                     :*: (updateSalary b f (outr x)))
>   view iso a  =  UpdateSalary (\ f x -> to iso 
>                                         (updateSalary a f (from iso x)))
>   int         =  UpdateSalary (\ f x -> x)
>   char        =  UpdateSalary (\ f x -> x)
>   float       =  UpdateSalary (\ f x -> x)
> instance GenericList UpdateSalary
