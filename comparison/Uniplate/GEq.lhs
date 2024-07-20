%----------------------------------------------------------------------------
%
%  Title       :  GEq.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> module GEq where

> import BinTreeDatatype
> import CompanyDatatypes
> import GRoseDatatype
> import NGRoseDatatype

> errorNotSupported  = error "Uniplate.GEq: generic equality not supported in Uniplate"

> equalCompany :: Company -> Company -> Bool
> equalCompany = errorNotSupported

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = errorNotSupported

> equalNGRoseListInt :: NGRose [] Int -> NGRose [] Int -> Bool
> equalNGRoseListInt = errorNotSupported

> equalBTreeInt :: BinTree Int -> BinTree Int -> Bool
> equalBTreeInt = errorNotSupported
