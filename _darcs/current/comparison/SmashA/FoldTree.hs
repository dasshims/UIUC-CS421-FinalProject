{-# OPTIONS_GHC -fglasgow-exts  #-}

module FoldTree (selectInt, selectIntWTree, selectIntPerfect, selectSalary,
		 gapplySelectCompanies) 
    where

import Syb4A

-- Two ways to fold ints from the tree defined in
import CompanyDatatypes
import TreeDatatype

import CompanyDats
import TreeDats
import PerfectDats

-- originally: selectInt :: WTree Int Int -> [Int]
-- inferred type: 
--   selectInt :: (STApply (HCons (Int -> [Int]) HNil) a1 wi w,
--                  LDat (TL_red [a]) (HCons (Int -> [Int]) HNil) a1 wi) =>
--          a1 -> w
-- We can handle any thing whatsoever, whether it has ints or not...
selectInt xs = 
    gapp (TL_red concat) ((\ (s::Int) _ -> [s]) :+: HNil) xs

-- Ours selectInt can handle any kind of tree, WTree or any other ...
selectIntWTree x = selectInt x
selectIntPerfect x = selectInt x

-- originally: selectSalary :: Company -> [Salary]
-- inferred type:

selectSalary xs = 
    gapp (TL_red concat) ((\ (s::Salary) _ -> [s]) :+: HNil) xs
 
-- we can use selectSalary as an ordinary higher-order function
-- (we can pass it to map)
mapselectSalary :: [Company] -> [[Salary]]
mapselectSalary xs = map selectSalary xs

-- Turn selectSalary into a highest-order generic function...
data SelectSalary = SelectSalary

instance Apply SelectSalary Company [Salary] where
  apply _ xs = selectSalary xs

instance Apply SelectSalary [Company] [Salary] where
  apply _ xs = selectSalary xs

gapplySelectCompanies :: [Company] -> [[Salary]]
gapplySelectCompanies xs = 
    gapp (TL_red_shallow SelectSalary) HNil xs


testgapplySelectCompanies = 
    gapplySelectCompanies [genCom,genCom',genCom'']

mytest = print ( selectSalary genCom
               , selectInt mytree
               )
