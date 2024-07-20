> module Paradise (increase) where
> import CompanyDatatypes
> import CompanyReps(rCompany)
> import UpdateSalaryDef(updateSalary)

> increase :: Float -> Company -> Company
> increase = error "LIGD does not support ad-hoc cases."
>   -- This means it should match the S constructor and not do RCon tricks.
> increase' :: Float -> Company -> Company
> increase' f c = updateSalary f rCompany c
>   -- But the functionality can be implemented
