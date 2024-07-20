{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

module FoldTree (listifyInt, listifySalary) where

import SmashA.Syb4A

-- Two ways to fold ints from the tree defined in
import CompanyDatatypes
import TreeDatatype

import SmashA.CompanyDats
import SmashA.TreeDats


-- originally: listifyInt :: Tree Int Int -> [Int]
-- inferred type: 
--   listifyInt :: (STApply (HCons (Int -> [Int]) HNil) a1 wi w,
--                  LDat (TL_red [a]) (HCons (Int -> [Int]) HNil) a1 wi) =>
--          a1 -> w
-- We can handle any thing whatsoever, whether it has ints or not...
listifyInt xs = 
    gapp (TL_red concat) ((\ (s::Int) -> [s]) :+: HNil) xs
 

-- originally: listifySalary :: Company -> [Salary]
-- inferred type:

listifySalary xs = 
    gapp (TL_red concat) ((\ (s::Salary) -> [s]) :+: HNil) xs
 

mytest = print ( listifySalary genCom
               , listifyInt mytree
               )
