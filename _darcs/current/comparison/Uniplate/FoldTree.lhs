%----------------------------------------------------------------------------
%
%  Title       :  FoldTree.lhs
%  Author(s)   :  Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module FoldTree where

> import Data.Generics.PlateData

> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps
> import PerfectDatatype
> import PerfectReps
> import BinTreeDatatype
> import BinTreeReps
> import GRoseDatatype
> import GRoseReps

> selectIntWTree :: WTree Int Int -> [Int]
> selectIntWTree = universeBi

> selectIntPerfect :: Perfect Int -> [Int]
> selectIntPerfect = universeBi

> selectIntBinTree :: BinTree Int -> [Int]
> selectIntBinTree = universeBi

> selectIntGRose :: GRose [] Int -> [Int]
> selectIntGRose = universeBi

> selectSalary :: Company -> [Salary]
> selectSalary = universeBi

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = error "Uniplate can't implement gmapQ"
