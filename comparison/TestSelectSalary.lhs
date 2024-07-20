> import CompanyDatatypes
> import TreeDatatype
> import FoldTree (selectSalary)

Read the explanation of this test in TestSelectInt .

The approach should allow for ad-hoc definitions for this
test, the following approaches are cheating:

Spine
NOW

LIGD does it in a very ugly way: it matches a constructor
named "S" and then for the representation of a Float.

Grading for Ad-hoc:
++ We can provide an ad-hoc arm that can access the values
   of the datatype using pattern matching on its constructors.
   And not thus the painful matching from LIGD. That is
   looking at the name of the Salary constructor (S) and then
   looking for the Float value.
-- Cannot do it.

> main = print ( selectSalary genCom
>              )
