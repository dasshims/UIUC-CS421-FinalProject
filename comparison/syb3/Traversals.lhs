> {-# OPTIONS_GHC -fglasgow-exts #-}
> {-# OPTIONS_GHC -fallow-undecidable-instances #-}
> {-# OPTIONS_GHC -fallow-overlapping-instances #-}

> module Traversals where

> import Data.Generics.SYB.WithClass.Basics
> import BinTreeDatatype
> import BinTreeReps

These traversals were copied (and adapted) from CtxSchemes.hs.
That file is distributed in 

< http://homepages.cwi.nl/~ralf/syb3/code.html

The adaptations are necessary so that it works with SYB3 distributed in:

< darcs get http://happs.org/HAppS/syb-with-class

> -- | A type constructor for accumulation
> newtype A a c d = A { unA :: a -> (a, c d) }

> gfoldlAccum :: Data ctx d
>             => Proxy ctx
>             -> (forall d r. Data ctx d => a -> c (d -> r) -> d -> (a, c r))
>             -> (forall g. a -> g -> (a, c g))
>             -> a -> d -> (a, c d)
> 
> gfoldlAccum ctx k z a d = unA (gfoldl ctx k' z' d) a
>  where
>   k' c y = A (\a -> let (a', c') = unA c a in k a' c' y)
>   z' f   = A (\a -> z a f)

> -- | gmapQr with accumulation
> gmapAccumQr :: Data ctx d 
>             => Proxy ctx
>             -> (r' -> r -> r) 
>             -> r
>             -> (forall d. Data ctx d => a -> d -> (a,r'))
>             -> a -> d -> (a, r)
> gmapAccumQr ctx o r f a d = let (a',l) = gfoldlAccum ctx k z a d
>                             in (a',unQr l r)
>  where
>   k a (Qr c) d = let (a',r') = f a d 
>                   in (a', Qr (\r -> c (r' `o` r)))
>   z a _ = (a, Qr id)


> -- | gmapQ with accumulation
> gmapAccumQ :: Data ctx d
>            => Proxy ctx
>            -> (forall d. Data ctx d => a -> d -> (a,q))
>            -> a -> d -> (a, [q])
> gmapAccumQ ctx f = gmapAccumQr ctx (:) [] f 


Adapted from the source code of SYB 1&2 to work with syb3

> gzipWithQ :: forall ctx r . Proxy ctx -> GenericQ ctx (GenericQ ctx r) -> GenericQ ctx (GenericQ ctx [r])
> gzipWithQ ctx f x y = case gmapAccumQ ctx perkid funs y of
>                    ([], r) -> r
>                    _       -> error "gzipWithQ"
>  where
>   perkid a d = (tail a, unGQ (head a) d)
>   funs :: [GenericQ' ctx r]
>   funs = gmapQ ctx (\k -> GQ (f k)) x
> newtype GenericQ' ctx r = GQ { unGQ :: GenericQ ctx r }

Adapted from CtxSchemes.hs

> everything :: Proxy ctx -> (r -> r -> r) -> GenericQ ctx r -> GenericQ ctx r
> everything ctx k f x
>   = foldl k (f x) (gmapQ ctx (everything ctx k f) x)

> everywhere :: Proxy ctx -> GenericT ctx -> GenericT ctx
> everywhere ctx f
>   = f . gmapT ctx (everywhere ctx f)
 
