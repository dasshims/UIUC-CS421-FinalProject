{-# OPTIONS -fglasgow-exts  #-}

module FoldTree (listifyInt, listifySalary) where

import Smash.Syb4

-- Two ways to fold ints from the tree defined in
import CompanyDatatypes
import TreeDatatype

import Smash.CompanyDats
import Smash.TreeDats


-- originally: listifyInt :: Tree Int Int -> [Int]
-- inferred type: listifyInt :: (Dat (SCons Int SNil) a) => a -> [Int]
-- We can handle any thing whatsoever, whether it has ints or not...
listifyInt xs = 
    gmapq (SCons (\ (s::Int) -> [s]) SNil) (concat) xs
 

-- originally: listifySalary :: Company -> [Salary]
-- inferred type:
-- listifySalary :: (Dat (SCons Salary SNil) a) => a -> [Salary]

listifySalary xs = 
    gmapq (SCons (\ (s::Salary) -> [s]) SNil) (concat) xs
 

mytest = print ( listifySalary genCom
               , listifyInt mytree
               )