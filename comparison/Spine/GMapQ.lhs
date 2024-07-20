%----------------------------------------------------------------------------
%
%  Title       :  GMapQ.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GMapQ  where

> import SYB1 hiding (Tree)
> import TreeDatatype
> import CompanyDatatypes
> import GRoseDatatype

Traversals, from "Scrap your boilerplate, Reloaded"

> type Query r = forall a. Type a -> a -> r

> mapQ :: Query r -> Query [r]
> mapQ q t x = mapQ' q $ toSpine (t :> x)

> mapQ' :: Query r -> (forall a. Spine a -> [r])
> mapQ' q (Con c)          =  []
> mapQ' q (f :$ (t :> x))  =  mapQ' q f ++ [q t x]

Just a wrapper

> gmapQ :: forall r . (forall a.Typed a -> r) -> Query [r]
> gmapQ q = mapQ q'
>  where
>    q' :: Query r
>    q' r v = q (r :> v)


