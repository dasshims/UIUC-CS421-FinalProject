> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> 
> {-
> 
> (C) 2004 Ralf Laemmel
> 
> Used to loop with GHC CVS HEAD < 6 December 2004.
> See subdir LoopOfTheDay-041201.
> Thanks to SPJ for looking into this.
> 
> This encoding is meant to simulate (and parameterise) the superclass
> encoding in SuperShow.hs. One qualitative difference between
> SuperShow.hs and this file is that the Data instances need to be
> constrained for context parameterisation, whereas this is not
> necessary for hardwired superclass constraints. Also, the extra Sat
> instance triggers some recursion through Data, Sat, GShow which was
> not there in SuperShow.hs because of the absence of the Sat class.
> 
> -}

GShow test derived from SYB3/NewShow1.hs
available at http://homepages.cwi.nl/~ralf/syb3/code.html
  -- Alexey 2008.03.03

> 
> 
> module GShow where
> 
> import Data.Generics.SYB.WithClass.Basics
>
> import GShowDef
> import CompanyDatatypes
> import CompanyReps
> 
> ------------------------------------------------------------------------------
> 
> gshowsCompany :: Company -> String
> gshowsCompany = gshow
>

> gapplyShowList :: [Int] -> [String]
> gapplyShowList = gmapQ gshowCtx (gshowD dict)

Note that the function can't be |gshow| because otherwise
you need incoherent instances.

> ------------------------------------------------------------------------------

