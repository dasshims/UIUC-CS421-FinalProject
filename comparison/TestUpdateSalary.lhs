> import CompanyDatatypes
> import Paradise (increase)

A test case for ad-hoc cases and transformation functions.
It increases the salaries of a company by 10%.

Ad-hoc cases mean that we are able to supply a case for a
datatype (Salary) and able to pattern match its constructors
(S).

> main = print $ increase 0.1 genCom

