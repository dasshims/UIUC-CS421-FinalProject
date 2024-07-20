%----------------------------------------------------------------------------
%
%  Title       :  CrushRight.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module CrushRight where

Two ways to fold ints from the tree defined in

> import TreeDatatype
> import SYB1

> errorMsg = error "Spine: can't handle data types of *->*->* in lifted spine view"

> sizeListTree :: [WTree a w] -> Int
> sizeListTree = errorMsg

> flattenListTree :: [WTree a w] -> [a]
> flattenListTree = errorMsg

> sumListTree :: [WTree Int w] -> Int
> sumListTree = errorMsg

> flatten :: Type' f -> f a -> [a]
> flatten IdR           x  =  [outId x]
> flatten (SpineR' a')  x  =  flattenSpine x
> flatten a'            x  =  case spineView a' of
>                                     View'C b' from to -> flatten b' (from x)

> flattenSpine :: Spine' f a -> [a]
> flattenSpine (Con' x)            =  []
> flattenSpine (f :$$ (a' ::> x))  =  flattenSpine f ++ flatten a' x
