> module FoldTree where
> import CompanyDatatypes
> import TreeDatatype
> import PerfectDatatype
> import TreeRep()
> import PolyLib.Flatten(flatten)

> selectSalary :: Company -> [Salary]
> selectSalary = error "PolyP does not handle the Company datatype"

On the other hand, PolyP can do _real_ folds (catas) on Regular
datatypes.

> selectWeight :: WTree a w -> [w]
> selectWeight = flatten

> selectIntWTree :: WTree a Int -> [Int]
> selectIntWTree = selectWeight

> selectIntPerfect :: Perfect Int -> [Int]
> selectIntPerfect = error "PolyP does not handle nested datatypes"

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = error "PolyP does not handle the Company datatype"
