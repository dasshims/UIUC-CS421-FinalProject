{-# OPTIONS_GHC -fallow-undecidable-instances #-}

> module GShow where
> import PolyLib.Base(pmap)
> import PolyLib.Show(pshow)
> import CompanyDatatypes(Company)
> import BinTreeDatatype(BinTree)
> import BinTreeRep

> gshowsCompany :: Company -> String
> gshowsCompany c = error "PolyP does not handle the Company d.t. group"


Should use gmapQ instead of pmap

> gapplyShowList :: [a] -> [String]
> gapplyShowList    = error "PolyP does not support gmapQ"

> gapplyShowBinTree :: BinTree (d a) -> BinTree String
> gapplyShowBinTree = error "PolyP does not support gmapQ"

What would be its type? Something like this perhaps:

gmapQ :: FunctorOf g e => (FunctorOf f d => d a -> r) -> e a -> [r]



> gapplyShowBinTree' t = pmap pshow t
>   where _dummy = pmap (const ()) t :: BinTree ()


