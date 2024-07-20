> module FoldTree where
> import CompanyDatatypes
> import TreeDatatype

> listifySalary :: Company -> [Salary]
> listifySalary = error "PolyP does not handle the Company datatype"

> listifyInt :: Tree a w -> [w]
> listifyInt = error "PolyP does not handle the Tree a w datatype"

On the other hand, PolyP can do _real_ folds (catas) on Regular
datatypes.
