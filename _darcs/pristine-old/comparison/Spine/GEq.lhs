{-# OPTIONS -fglasgow-exts #-}

> module GEq (equalCompany,equalGRoseListInt) where

> import SYB1
> import CompanyDatatypes
> import GRoseDatatype

> equalCompany :: Company -> Company -> Bool
> equalCompany x y = equal (CompanyR :> x) (CompanyR :> y)

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt x y = equal (GRoseR ListR IntR :> x)
>                               (GRoseR ListR IntR :> y)

Definition of equality taken from
"Scrap your boilerplate, reloaded"

> equal :: Typed a -> Typed b -> Bool
> equal x1 x2 = equalSpines (toSpine x1) (toSpine x2)

> equalSpines :: Spine a -> Spine b -> Bool
> equalSpines (Con c1) (Con c2) = name c1 == name c2
> equalSpines (f1 :$ x1) (f2 :$ x2) = equalSpines f1 f2 && equal x1 x2
> equalSpines _ _ = False

