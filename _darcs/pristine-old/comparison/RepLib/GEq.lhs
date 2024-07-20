> {-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances#-}

{----------------------------------------------------------------------------

 Module      :  GEq
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD
 
 This test checks whether two companies are equal.

----------------------------------------------------------------------------}

> module GEq  where
> import RepLib
> import CompanyDatatypes
> import CompanyReps
> import GRoseDatatype


Call generic equality function from RepLib

> instance Eq Company   where
>   (==) = eqR1 rep1
> instance Eq Dept      where
>   (==) = eqR1 rep1   
> instance Eq Unit      where
>   (==) = eqR1 rep1
> instance Eq Employee  where
>   (==) = eqR1 rep1
> instance Eq Person    where
>   (==) = eqR1 rep1
> instance Eq Salary    where
>   (==) = eqR1 rep1

> equalCompany :: Company -> Company -> Bool
> equalCompany = (==)

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = error "Not yet implemented"

