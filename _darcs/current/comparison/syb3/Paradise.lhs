> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Paradise where

> import Data.Generics.SYB.WithClass.Basics
> import Traversals

> import CompanyDatatypes
> import CompanyReps


> class CaseSalary a where
>   caseSalary :: Float -> a -> a

> instance CaseSalary Salary where
>   caseSalary k (S x) = S (x * (1 + k))

> data CaseSalaryD a = CaseSalaryD { caseSalaryD :: Float -> a -> a }

> instance CaseSalary a => Sat (CaseSalaryD a)
>   where dict = CaseSalaryD caseSalary

> instance Data CaseSalaryD a => CaseSalary a where
>     caseSalary _ x = x

> caseSalaryCtx = undefined :: Proxy CaseSalaryD


> increase :: Float -> Company -> Company
> increase k = everywhere caseSalaryCtx (caseSalaryD dict k)

