> module FoldTree where

> import Data.Generics

Two ways to fold ints from the tree defined in

> import TreeDatatype
> import CompanyDatatypes

> listifyInt :: Tree Int Int -> [Int]
> listifyInt = everything (++) ([] `mkQ` (:[]))

> listifySalary :: Company -> [Salary]
> listifySalary = everything (++) ([] `mkQ` (:[]))

Include definition of everything and mkQ?
