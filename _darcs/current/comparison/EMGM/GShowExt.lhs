> {-# OPTIONS_GHC -fglasgow-exts #-}


> module GShowExt where
> import GL (GenericList(list))
> import CompanyDatatypes(Company)
> import CompanyReps(GenericCompany)
> import Data.List(intersperse)
> import GShowDef(Gshows(Gshows),gshows,applyGshow)
>   -- above: definition of generic show
> import Common(showSList)

> -- Enable generic show for the company datatypes
> instance GenericCompany Gshows

> -- Extend generic show with an ad-hoc case for lists
> -- So we ignore the default (generic) method for showing lists
> instance GenericList Gshows where
>   -- list :: Gshows a -> Gshows [a]
>   list a = Gshows (Common.showSList (applyGshow a))

> gshowsCompany     :: Company -> String
> gshowsCompany x   =  gshows x ""
