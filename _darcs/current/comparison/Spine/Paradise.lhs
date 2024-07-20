> module Paradise  where
> import UpdateSalaryDef(updateSalary)
> import CompanyDatatypes(Company)
> -- import CompanyReps(rCompany)
> import SYB1(Type(CompanyR)) -- cheating

> increase :: Float -> Company -> Company
> increase = error "Spine does not support ad-hoc cases."
> --increase = updateSalary CompanyR

To support them, Spine must add a representation constructor
and this requires recompilation!
