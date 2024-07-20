> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module GEq where
> import PolyLib.Prelude
> import PolyLib.Equal
> import TreeDatatype
> import CompanyDatatypes
> import GRoseDatatype
> import NGRoseDatatype
> import BinTreeDatatype
> import BinTreeRep

Implementation of generic equality

> equalCompany :: Company -> Company -> Bool
> equalCompany x y = error "PolyP: Company is not a 1-parameter Regular datatype"

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt x y = pequal (==) x y

pequal is from PolyLib

(GRose f) is Regular:

> instance Functor f => FunctorOf (ProdF ParF (CompF f RecF)) (GRose f) where
>   inn (ParF x :*: CompF cs) = GRose x (fmap unRecF cs)
>   out (GRose x cs)          = ParF x :*: CompF (fmap RecF cs)

>   datatypeName              = const "GRose []"
>   constructorName (GRose _ _)="GRose"
>   constructorFixity (GRose _ _)=defaultFixity


> equalBTreeInt :: BinTree Int -> BinTree Int -> Bool
> equalBTreeInt = pequal (==)


> equalNGRoseListInt :: NGRose [] Int -> NGRose [] Int -> Bool
> equalNGRoseListInt = error "PolyP: NGRose is not a Regular datatype"
