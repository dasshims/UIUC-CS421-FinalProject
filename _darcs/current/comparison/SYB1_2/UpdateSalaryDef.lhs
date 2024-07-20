> module UpdateSalaryDef(updateSalary) where
> import Data.Generics(Data,everywhere, mkT)
> import CompanyDatatypes(Salary(S))
> import CompanyReps

> -- Increase salary by percentage
> updateSalary :: Data a => Float -> a -> a
> updateSalary k = everywhere (mkT (incS k))

> -- "interesting" code for updateSalary
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
