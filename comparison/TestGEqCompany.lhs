> import GEq (equalCompany)
> import CompanyDatatypes

GEqCompany tests:
 * Multiple arguments (equality has two).
 * Support for mutual recursive datatypes (universe size): Company
   (fails for PolyP - see GEqBTree for a simpler test)
----------------------------------------------------------------------------------

> main = print ( equalCompany genCom genCom
>              , equalCompany genCom genCom'
>              , equalCompany genCom genCom''
>             )
