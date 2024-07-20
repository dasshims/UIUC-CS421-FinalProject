> {-# OPTIONS_GHC -fglasgow-exts #-}
> {-# OPTIONS_GHC -fallow-undecidable-instances #-}
> {-# OPTIONS_GHC -fallow-overlapping-instances #-}

> module GEq where

> import Data.Generics.SYB.WithClass.Basics
> import BinTreeDatatype
> import BinTreeReps
> import CompanyDatatypes
> import CompanyReps
> import GRoseDatatype
> import GRoseReps
> import NGRoseDatatype
> import Traversals

> -- A new class for generic equality
> class GEq a
>  where
>   geq :: a -> a -> Bool
> 
> 
> -- The dictionary type
> data GEqD a
>    = GEqD { geqD :: a -> a -> Bool }
> 
> -- The Sat instance
> instance GEq a => Sat (GEqD a)
>  where
>   dict = GEqD { geqD = geq }
> 
> -- The context parameter
> geqCtx = undefined :: Proxy GEqD
>

> -- The generic default
> instance Data GEqD a => GEq a where
>   geq x y = geq' x y
>     where
>       geq' :: GenericQ GEqD (GenericQ GEqD Bool)
>       geq' x y =     (toConstr geqCtx x == toConstr geqCtx y)
>                   && and (gzipWithQ geqCtx eqWrapper x y)

Note that the argument of gzipWithQ above, must be a generic
equality with arguments of *different* type. So we define
a wrapper over geq, that does the casting of the second argument:

> eqWrapper :: GenericQ GEqD (GenericQ GEqD Bool)
> eqWrapper x y = case cast y of
>                 Just y' -> geqD dict x y'
>                 Nothing -> False

> equalBTreeInt :: BinTree Int -> BinTree Int -> Bool
> equalBTreeInt = geq

The code below is from the file geq3.hs from

< http://homepages.cwi.nl/~ralf/syb3/code.html

> {- 
> -- The generic default
> instance Data GEqD a => GEq a
>  where
>   geq x y = let ([], bools) = gmapAccumQ geqCtx geq' (gmapQ geqCtx Pack x) y
>             in and ((toConstr geqCtx x == toConstr geqCtx y) : bools)
>--    where
>--     -- Compare kids one by one
> geq' :: Data GEqD y => [Pack] -> y -> ([Pack],Bool)
> geq' (x:xs) y = (xs, maybe False (geqD dict y) (unpack x))

> -- Should go to the library, too.
> -- We could use dynamics as well.
> 
> -- Ex wrappers for twin traversal
> data Pack = forall x. Typeable x => Pack x
> unpack (Pack x) = cast x
> -}
> 

> equalCompany :: Company -> Company -> Bool
> equalCompany = geq

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = geq

> equalNGRoseListInt :: NGRose [] Int -> NGRose [] Int -> Bool
> equalNGRoseListInt = error "Not yet implemented (instance missing and derive magic fails)"




