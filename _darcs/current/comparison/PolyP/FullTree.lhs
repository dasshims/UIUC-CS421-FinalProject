> {-# LANGUAGE UndecidableInstances #-}

> module FullTree where
> import PolyLib.Prelude
> import PolyLib.BaseM
> import PolyLib.Equal(pequal)
> import BinTreeRep()
> import BinTreeDatatype

Based on gfulltree, which is defined in the Fan module.
Probably a better name for this function would be generate.

This function generates terms of type |t| whose depth is less
or equal than the given depth. Note that the generation of base
types is simple ('0' for Char), but this would be easy to change.

> type Full t = Int -> [t]
> gfulltree :: (FunctorOf f d, P_fmap2M f, P_ffulltree f) => 
>              Full a -> Full (d a)
> gfulltree fta d = anaM (ffulltree fta) d

> class P_fmap2M f => P_ffulltree f where
>   ffulltree  :: Full a -> Full (f a Int)
>   ffulltree fta = concatMap (fmap2M fta return) . ffulltree'
>   ffulltree' :: Full (f Int Int)
>   ffulltree' = ffulltree return

> instance P_ffulltree EmptyF where
>   ffulltree fta d = [EmptyF]
>   ffulltree'    d = [EmptyF]

In these cases, consumeDepth is used to check whether
depth is zero.

> instance P_ffulltree ParF where
>   ffulltree fta d = consumeDepth (map ParF (fta (d-1))) d
>   ffulltree'    d = consumeDepth [ParF (d-1)] d
> instance P_ffulltree RecF where
>   ffulltree fta d = consumeDepth [RecF (d-1)] d
>   ffulltree'    d = consumeDepth [RecF (d-1)] d

> instance (P_ffulltree f, P_ffulltree g) => P_ffulltree (SumF f g) where
>   ffulltree' d = map InL (ffulltree' d) ++
>                  map InR (ffulltree' d)
> instance (P_ffulltree f, P_ffulltree g) => P_ffulltree (ProdF f g) where
>   ffulltree' d = [ l :*: r
>                  | l <- ffulltree' d
>                  , r <- ffulltree' d
>                  ]
> instance (FunctorOf df d, P_ffulltree df, P_ffulltree e,P_fmap2M df) =>
>          P_ffulltree (CompF d e) where -- Int -> d (e a Int)
>   ffulltree fta d = map CompF (gfulltree (ffulltree fta) d)


> genBinTree :: Int -> [BinTree Char]
> genBinTree d = gfulltree (consumeDepth ['0']) d

> genList :: Int -> [[Char]]
> genList d = gfulltree (consumeDepth ['0']) d

Primitive types consume one depth unit
This hack would not be needed with a list-like view.

> consumeDepth ls 0 = []
> consumeDepth ls _ = ls


