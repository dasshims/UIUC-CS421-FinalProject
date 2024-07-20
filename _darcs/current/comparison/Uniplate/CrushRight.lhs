%----------------------------------------------------------------------------
%
%  Title       :  CrushRight.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}
> module CrushRight where

> import Data.Generics.PlateData
> import Data.Generics
> import Data.Maybe
> import TreeReps
> import TreeDatatype

Uniplate can only query/transform elements of the same type,
so if a and w are both of the same type, the test fails.

> flattenListTree :: (Data a, Data w) => [WTree a w] -> [a]
> flattenListTree = error "Uniplate can't do crushright"

Below is a cheat, that removes the weights prior to the collect.

> flattenListTree' :: (Data a, Data w) => [WTree a w] -> [a]
> flattenListTree' = universeBi . map (rewrite f)
>   where f (WithWeight t w) = Just t
>         f _ = Nothing

> sizeListTree :: (Data a,Data w) => [WTree a w] -> Int
> sizeListTree = length . flattenListTree

> sumListTree :: (Data w) => [WTree Int w] -> Int
> sumListTree = sum . flattenListTree



