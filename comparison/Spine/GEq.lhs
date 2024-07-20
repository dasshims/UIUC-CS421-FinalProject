%----------------------------------------------------------------------------
%
%  Title       :  GEq.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  These tests cheat separate compilation. Several datatypes
%                 are added to the type rep., so recompilation is required
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GEq where

> import SYB1
> import CompanyDatatypes
> import GRoseDatatype
> import BinTreeDatatype
> import NGRoseDatatype

> equalCompany :: Company -> Company -> Bool
> equalCompany x y = equal (CompanyR :> x) (CompanyR :> y)

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt x y = equal (GRoseR ListR IntR :> x)
>                               (GRoseR ListR IntR :> y)

> equalNGRoseListInt :: NGRose [] Int -> NGRose [] Int -> Bool
> equalNGRoseListInt x y = equal (NGRoseR ListR IntR :> x)
>                                (NGRoseR ListR IntR :> y)

> equalBTreeInt :: BinTree Int -> BinTree Int -> Bool
> equalBTreeInt x y = equal (BinTreeR IntR :> x) (BinTreeR IntR :> y)

Definition of equality taken from
"Scrap your boilerplate, reloaded"

> equal :: Typed a -> Typed b -> Bool
> equal x1 x2 = equalSpines (toSpine x1) (toSpine x2)

> equalSpines :: Spine a -> Spine b -> Bool
> equalSpines (Con c1)   (Con c2)   = name c1 == name c2
> equalSpines (f1 :$ x1) (f2 :$ x2) = equalSpines f1 f2 && equal x1 x2
> equalSpines _          _          = False
