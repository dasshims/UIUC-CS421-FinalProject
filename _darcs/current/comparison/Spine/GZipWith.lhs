> {-#  OPTIONS_GHC -fglasgow-exts  #-}

> module GZipWith where

> import SYB1 
> import BinTreeDatatype

> gZipWith :: Type' f -> (a -> b -> c) -> f a -> f b -> f c
> gZipWith IdR           m x y  =  InId (m (outId x) (outId y))
> gZipWith (SpineR' a')  m x y  =  gZipWithSpine m x y
> gZipWith a'            m x y  =  case spineView a' of
>                                    View'C b' from to -> to (gZipWith b' m (from x) (from y))

> gZipWithSpine ::  (a -> b -> c) -> Spine' f a -> Spine' f b -> Spine' f c
> gZipWithSpine m (Con' x) (Con' y)                      =  Con' x
> gZipWithSpine m (f1 :$$ (a ::> x)) (f2 :$$ (b ::> y))  =  gZipWithSpine m f1 f2 :$$ (a ::> gZipWith a m x y)
f1 :: Spine' (fx :+> f) a
f2 :: Spine' (fy :+> f) b