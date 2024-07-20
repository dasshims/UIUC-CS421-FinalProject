> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GEq where
> import GL3
> import Data.Generics hiding (Generic, geq)

Implementation of generic equality

> newtype Geq a b c         =  Geq { geq' :: a -> a -> Bool }

> instance Generic Geq where
>   unit                    =  Geq (\ x y -> case (x, y) of 
>                                              (Unit, Unit) -> True)
>   plus a b                =  Geq (\ x y -> case (x, y) of
>                                            (Inl xl, Inl yl) -> geq' a xl yl
>                                            (Inr xr, Inr yr) -> geq' b xr yr
>                                            _                -> False)
>   prod a b                =  Geq (\ x y -> geq' a (outl x) (outl y) && geq' b (outr x) (outr y))
>   view iso _ _ a          =  Geq (\ x y -> geq' a (from iso x) (from iso y))
>   char                    =  Geq (\ x y -> x == y)
>   int                     =  Geq (\ x y -> x == y)
>   float                   =  Geq (\ x y -> x == y)

> geq :: GRep Geq a => a -> a -> Bool
> geq = geq' over


> {-
> equalCompany :: Company -> Company -> Bool
> equalCompany = geq' over

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = error "Not yet implemented"
> -}
