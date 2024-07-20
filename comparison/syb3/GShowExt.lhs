> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> 

GShow test derived from SYB3/NewShow1.hs
available at http://homepages.cwi.nl/~ralf/syb3/code.html
  -- Alexey 2008.03.03

> 
> 
> module GShowExt where
> 
> import Data.Generics.SYB.WithClass.Basics
>
> import Prelude hiding (showList)
> import GShowDef
> import CompanyDatatypes
> import CompanyReps
> import Common
> 
> ------------------------------------------------------------------------------
> 
> gshowsCompany :: Company -> String
> gshowsCompany = gshow
>


> ------------------------------------------------------------------------------
> 
> -- A type-specific case for lists (... and extends gshow imported from GShowDef!!)
> 
> instance GShow a => GShow [a] where
>   gshow as = showList gshow as
> 
> ------------------------------------------------------------------------------

> 
> ------------------------------------------------------------------------------

