{-#  OPTIONS -fglasgow-exts  #-}

> module GEqTree where
> import NOW hiding (S, equal)
> import TreeDatatype
> import GEq

> equalWTree :: WTree Int Int -> WTree Int Int -> Bool
> equalWTree x y = geqTree (WTreeR IntR IntR :> x) (WTreeR IntR IntR :> y)

> geqTree :: Typed a -> Typed b -> Bool
> geqTree (WTreeR a b :> WithWeight t1 _) t2 = geqTree (WTreeR a b :> t1) t2 
> geqTree t1 (WTreeR a b :> WithWeight t2 _) = geqTree t1 (WTreeR a b :> t2)
> geqTree (WTreeR a b :> Fork t1 t1') (WTreeR c d :> Fork t2 t2') = geqTree  (WTreeR a b :> t1) (WTreeR c d :> t2) && 
>                                                                 geqTree  (WTreeR a b :> t1') (WTreeR c d :> t2')
> geqTree (WTreeR a b :> Leaf x) (WTreeR c d :> Leaf y) = equal (a :> x) (c :> y)
> geqTree _ _ = False
