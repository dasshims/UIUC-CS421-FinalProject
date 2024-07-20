> module FoldTree where
> import Data.Generics

Two ways to fold ints from the tree defined in

> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps
> import PerfectDatatype
> import PerfectReps

> selectIntWTree       :: WTree Int Int -> [Int]
> selectIntWTree       =  everything (++) ([] `mkQ` (:[]))

> selectIntPerfect       :: Perfect Int -> [Int]
> selectIntPerfect       =  everything (++) ([] `mkQ` (:[]))

> genSelectSalary :: Data a => a -> [Salary]
> genSelectSalary =  everything (++) ([] `mkQ` (:[]))

> selectSalary :: Company -> [Salary]
> selectSalary = genSelectSalary

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = gmapQ genSelectSalary

Include definition of everything and mkQ?
