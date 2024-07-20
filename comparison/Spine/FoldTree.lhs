%----------------------------------------------------------------------------
%
%  Title       :  FoldTree.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  These tests cheat separate compilation. The Company data
%                 type is added to the repr. GADT.
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module FoldTree  where

> import SYB1 hiding (Perfect)
> import TreeDatatype
> import CompanyDatatypes
> import PerfectDatatype
> import GMapQ(Query, mapQ)

> everything' :: Query r -> Query [r]
> everything' q t x = [q t x] ++ concat (mapQ (everything' q) t x)

> everything :: (r -> r -> r) -> Query r -> Query r
> everything op q t x = foldl1 op ([q t x] ++ mapQ (everything op q) t x)

> selectIntWTree :: WTree Int Int -> [Int]
> selectIntWTree = everything (++) ([] `mkQ` (:[])) (TreeWR IntR IntR)
>   where
>     mkQ :: [Int] -> (Int -> [Int]) -> Type a -> a -> [Int]
>     mkQ zero lift IntR i = lift i
>     mkQ zero lift _ _    = zero

> selectIntPerfect :: Perfect Int -> [Int]
> selectIntPerfect = everything (++) ([] `mkQ` (:[])) (PPerfectR IntR)
>   where
>     mkQ :: [Int] -> (Int -> [Int]) -> Type a -> a -> [Int]
>     mkQ zero lift IntR i = lift i
>     mkQ zero lift _ _    = zero


> selectSalary :: Company -> [Salary]
> selectSalary = error "Spine cannot add ad-hoc datatypes to the representation."  -- genSelectSalary CompanyR -- this cheats (adds constructor to repr.)

> genSelectSalary :: Type a -> a -> [Salary]
> genSelectSalary = everything (++) ([] `mkQ` (:[]))
>   where
>     mkQ :: [Salary] -> (Salary -> [Salary]) -> Type a -> a -> [Salary]
>     mkQ zero lift SalaryR i = lift i
>     mkQ zero lift _ _    = zero

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = mapQ genSelectSalary (ListR CompanyR)

