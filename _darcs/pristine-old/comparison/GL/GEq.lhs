> module GEq (equalCompany, equalGRoseListInt, Geq(Geq), geq') where
> import GL
> import CompanyDatatypes hiding (Unit)
> import CompanyReps
> import Data.Generics hiding (Generic, geq)
> import GRoseDatatype
> import GRoseReps

Implementation of generic equality

> newtype Geq a             =  Geq { geq' :: a -> a -> Bool }

> instance Generic Geq where
>   unit                    =  Geq (\ x y -> case (x, y) of 
>                                              (Unit, Unit) -> True)
>   plus a b                =  Geq (\ x y -> case (x, y) of
>                                            (Inl xl, Inl yl) -> geq' a xl yl
>                                            (Inr xr, Inr yr) -> geq' b xr yr
>                                            _                -> False)
>   prod a b                =  Geq (\ x y -> geq' a (outl x) (outl y) && geq' b (outr x) (outr y))
>   view iso a              =  Geq (\ x y -> geq' a (from iso x) (from iso y))
>   char                    =  Geq (\ x y -> x == y)
>   int                     =  Geq (\ x y -> x == y)
>   float                   =  Geq (\ x y -> x == y)
> instance GenericCompany Geq where
> instance GenericList Geq where

> equalCompany :: Company -> Company -> Bool
> equalCompany = geq' over

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = geq' over
