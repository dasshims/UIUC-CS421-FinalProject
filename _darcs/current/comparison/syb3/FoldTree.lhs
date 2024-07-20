> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module FoldTree where
> 
> import Data.Generics.SYB.WithClass.Basics
> import Traversals

> import TreeDatatype
> import TreeReps

> import PerfectDatatype
> import PerfectReps

> import CompanyDatatypes
> import CompanyReps

> class CaseInt a where
>   caseInt :: a -> [Int]

> instance CaseInt Int where
>   caseInt x = [x]

> data CaseIntD a = CaseIntD { caseIntD :: a -> [Int] }

> instance CaseInt a => Sat (CaseIntD a)
>   where dict = CaseIntD caseInt

> instance Data CaseIntD a => CaseInt a where
>     caseInt x = []

> caseIntCtx = undefined :: Proxy CaseIntD

> selectIntWTree       :: WTree Int Int -> [Int]
> selectIntWTree       =  everything caseIntCtx (++) (caseIntD dict)

> selectIntPerfect       :: Perfect Int -> [Int]
> selectIntPerfect       =  error "Context reduction loops when using generated Data instances for Perfect"
> --selectIntPerfect       =  everything caseIntCtx (++) (caseIntD dict)


---------------------------------------------------
Now, for salaries

> class CaseSalary a where
>   caseSalary :: a -> [Salary]

> instance CaseSalary Salary where
>   caseSalary x = [x]

> data CaseSalaryD a = CaseSalaryD { caseSalaryD :: a -> [Salary] }

> instance CaseSalary a => Sat (CaseSalaryD a)
>   where dict = CaseSalaryD caseSalary

> instance Data CaseSalaryD a => CaseSalary a where
>     caseSalary x = []

> caseSalaryCtx = undefined :: Proxy CaseSalaryD

> genSelectSalary       :: Data CaseSalaryD a => a -> [Salary]
> genSelectSalary       =  everything caseSalaryCtx (++) (caseSalaryD dict)

> selectSalary       :: Company -> [Salary]
> selectSalary       =  genSelectSalary


> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = gmapQ caseSalaryCtx genSelectSalary


