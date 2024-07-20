{-#  OPTIONS_GHC -fglasgow-exts  #-}

> module GEq where
> import NOW hiding (S, equal)
> import TreeDatatype
> import CompanyDatatypes
> import GRoseDatatype

Implementation of generic equality

> equalCompany :: Company -> Company -> Bool
> equalCompany x y = equal (CompanyR :> x) (CompanyR :> y)

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt x y = equal (GRoseR ListR IntR :> x)
>                               (GRoseR ListR IntR :> y)

Definition of equality taken from
"Scrap your boilerplate, reloaded"

> equal :: Typed a -> Typed b -> Bool
> equal x y = equalSpines (toSpine x) (toSpine y)

> equalSpines :: Spine a -> Spine b -> Bool
> equalSpines (Con c1) (Con c2) = name c1 == name c2
> equalSpines (f1 :$ x1) (f2 :$ x2) = equalSpines f1 f2 && equal x1 x2
> equalSpines _ _ = False
