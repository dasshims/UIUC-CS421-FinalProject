> {-# OPTIONS_GHC -fallow-undecidable-instances #-}
> module Fan where
> import PolyLib.Prelude
> import PolyLib.Base(P_fmap2(fmap2), ana, pmap)

Towards implementing fan: gfulltree is an ad-hoc tree-filler based on
the assumption that the last constructor is the "most branching" and
the first constructor is the "least branching" (a leaf).

> type Full t = Int -> t
> gfulltree :: (FunctorOf f d, P_fmap2 f, P_ffulltree f) => 
>              Full a -> Full (d a)
> gfulltree fta = ana (ffulltree fta)

> class P_fmap2 f => P_ffulltree f where
>   ffulltree  :: Full a -> Full (f a Int)
>   ffulltree fta = fmap2 fta id . ffulltree'
>   ffulltree' :: Full (f Int Int)
>   ffulltree' = ffulltree id

> instance P_ffulltree EmptyF where
>   ffulltree fta d = EmptyF
>   ffulltree'    d = EmptyF
> instance P_ffulltree ParF where
>   ffulltree fta d = ParF (fta d)
>   ffulltree'    d = ParF d
> instance P_ffulltree RecF where
>   ffulltree fta d = RecF (d-1)
>   ffulltree'    d = RecF (d-1)
> instance (P_ffulltree f, P_ffulltree g) => P_ffulltree (SumF f g) where
>   ffulltree' 0 = InL (ffulltree' 0) -- arbitrary choice
>   ffulltree' d = InR (ffulltree' d) -- arbitrary choice
> instance (P_ffulltree f, P_ffulltree g) => P_ffulltree (ProdF f g) where
>   ffulltree' d = ffulltree' d :*: ffulltree' d
> instance (FunctorOf df d, P_ffulltree df, P_ffulltree e) =>
>          P_ffulltree (CompF d e) where -- Int -> d (e a Int)
>   ffulltree fta d = CompF (gfulltree (ffulltree fta) d)


> {-
> type Fan t = Int -> [t]
> gfan :: (FunctorOf f d, P_fmap2 f, P_ffan f) => 
>              Fan a -> Fan (d a)
> gfan faM = anaM (ffan faM)

> class P_fmap2 f => P_ffan f where
>   ffan  :: Fan a -> Fan (f a Int)
>   ffan fta = fmap2 fta id . ffan'
>   ffan' :: Fan (f Int Int)
>   ffan' = ffan id
> -}
