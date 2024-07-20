

This test exercices GENERIC read, show, and eq for the company
datatypes which we use a lot. The output of the program should be
"True" which means that "gread" reads what "gshow" shows while the
read term is equal to the original term in terms of "geq".


> import GEq (equalCompany)
> import CompanyDatatypes

----------------------------------------------------------------------------------

> main = print ( equalCompany genCom genCom
>              , equalCompany genCom genCom'
>              , equalCompany genCom genCom''
>             )

