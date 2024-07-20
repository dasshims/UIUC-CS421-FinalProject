> module GMapDef where

> import LIGD(Rep2(..), EPT(from,to), ($+), ($*))

Generic map transforms the elements of a given type constructor.
The location of the elements in the type are given by RVar2 constructors
from the type representations. The transforming functions that are embedded
in those constructors might also change the element type.

{- Recoded by Patrik Jansson, Feb. 2008. -}

> newtype GMap a b = GMap { gMap :: a -> b }
> type (:->) = Rep2 GMap

> mkR :: (a->b) -> (a:->b)
> mkR = RVar2 . GMap 
> unR :: (a:->b) -> (a->b)
> unR = gMap . mapG

> mapG :: (a:->b)            -> GMap a b
> mapG (RVar2 f)              = f
> mapG (RSum2  rA rB ep1 ep2) = GMap (to ep2 . (unR rA $+ unR rB) . from ep1)
> mapG (RPair2 rA rB ep1 ep2) = GMap (to ep2 . (unR rA $* unR rB) . from ep1)
> mapG (RType2 _e rA ep1 ep2) = GMap (to ep2 . unR rA             . from ep1)
> mapG (RCon2 _nm rA)         = mapG rA
> mapG (RInt2        ep1 ep2) = GMap (to ep2 . from ep1)
> mapG (RFloat2      ep1 ep2) = GMap (to ep2 . from ep1)
> mapG (RChar2       ep1 ep2) = GMap (to ep2 . from ep1)
> mapG (RDynamic2    ep1 ep2) = GMap (to ep2 . from ep1)
> mapG (RUnit2       ep1 ep2) = GMap (to ep2 . from ep1)

> mapGRep :: ((a:->b) -> (c:->d)) -> (a -> b) -> (c -> d)
> mapGRep rep f = unR (rep (mkR f))
