{-

This test exercices GENERIC eq for the company
datatypes which we use a lot. The output of the program should be
"(True, False)"

-}

> module GEq (equalCompany, equalGRoseListInt) where
> import GMsec2
> import CompanyDatatypes hiding (Unit)
> import CompanyReps
> import GRoseDatatype

Implementation of generic equality

> newtype Geq a             =  Geq { applyGeq :: a -> a -> Bool }
> 
> geq                       :: (TypeRep a) => a -> a -> Bool
> geq                       =  applyGeq typeRep

> instance Generic Geq where
>   unit                        =  Geq (\ x y -> case (x, y) of 
>                                              (Unit, Unit) -> True)
>   plus                        =  Geq (\ x y -> case (x, y) of
>                                              (Inl xl, Inl yl) -> geq xl yl
>                                              (Inr xr, Inr yr) -> geq xr yr
>                                              _                -> False)
>   pair                        =  Geq (\ x y -> geq (outl x) (outl y) && geq (outr x) (outr y))
>   datatype iso                =  Geq (\ x y -> geq (fromData iso x) (fromData iso y))
>   char                        =  Geq (\ x y -> x == y)
>   int                         =  Geq (\ x y -> x == y)
>   float                       =  Geq (\ x y -> x == y)
>   catchall                    =  error "Does not work yet"

> equalCompany :: Company -> Company -> Bool
> equalCompany = geq

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = error "Not yet implemented"
