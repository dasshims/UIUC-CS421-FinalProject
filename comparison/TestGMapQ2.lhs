> import CompanyDatatypes
> import FoldTree (gapplySelectCompanies)

Here we test higher order generic functions. 

  see TestHigherOrder.lhs

This test has a twist: the function that is passed to gmapQ has an ad-hoc
case: it collects salaries in Salary cases.

This test shows that Libraries that allow ad-hoc cases, such as EMGM,
may require a more involved definition of gmapQ.

> main = print $ (gapplySelectCompanies [genCom,genCom',genCom'']
>                )
