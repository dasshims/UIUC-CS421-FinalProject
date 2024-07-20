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
> module GShowDef where
> 
> import Data.Generics.SYB.WithClass.Basics
>
> import Common
> 
> ------------------------------------------------------------------------------
> 
> -- The class for the generic show function
> 
> class GShow a
>  where gshow :: a -> String
> 
> 
> ------------------------------------------------------------------------------
> 
> -- The dictionary type for the GShow class
> 
> data GShowD a = GShowD { gshowD :: a -> String }
> 
> 
> ------------------------------------------------------------------------------
> 
> -- Instantiation of the Sat class
> 
> instance GShow a => Sat (GShowD a)
>   where  dict = GShowD { gshowD = gshow }
> 
> 
> ------------------------------------------------------------------------------
> 
> -- A more constrained generic show function
> 
> gshow' :: Data GShowD a => a -> String
> gshow' =  gshowD dict
> 
> 
> ------------------------------------------------------------------------------
> 
> -- The context for generic show
> 
> gshowCtx = undefined::Proxy GShowD
> 
> 
> ------------------------------------------------------------------------------
> 
> -- The generic instance for the default show
> 
> instance Data GShowD a => GShow a
>   where
>    gshow a =
>        paren (not (null fields)) (
>          showConstr con ++ concat fields
>        )
>      where
>        con    = toConstr gshowCtx a
>        fields = map (" "++) (gmapQ gshowCtx gshow' a) 
> 
> 
> ------------------------------------------------------------------------------
> -- A type-specific case for Char's and Float's
>
> instance GShow Char where
>   gshow c = show c
>
> instance GShow Float where
>   gshow f = show f
> 

