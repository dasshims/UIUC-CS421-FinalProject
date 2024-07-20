> module UpdateSalaryDef(updateSalary) where
> import RepLib(Rep, everywhere, mkT)
> import CompanyDatatypes(Salary(S))
> import CompanyReps()

> -- Increase salary by percentage
> updateSalary :: Rep a => Float -> a -> a
> updateSalary k = everywhere (mkT (incS k))

> -- "interesting" code for updateSalary
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
