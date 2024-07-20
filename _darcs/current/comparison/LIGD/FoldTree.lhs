> module FoldTree where

> import LIGD(Rep(..), from, Sum(Inl,Inr), Pair((:*:)), rInt, rList)
> import TreeDatatype
> import TreeReps(rTree)
> import CompanyDatatypes
> import CompanyReps(rCompany)
> import PerfectDatatype
> import PerfectReps
> import GMapQ(gmapQ)

Accumulate all Ints in a datastructure.
Just a simple type-indexed function. Not protected by a type class.

> selectIntG :: Rep a -> a -> [Int]
> selectIntG (RInt ep) i       = [from ep i]
> selectIntG (RSum rA rB ep) t = case from ep t of
>                                Inl a -> selectIntG rA a
>                                Inr b -> selectIntG rB b
> selectIntG (RPair rA rB ep) t= case from ep t of
>                                 (a :*: b) -> selectIntG rA a ++ selectIntG rB b
> selectIntG (RType e rA ep) t = selectIntG rA (from ep t)
> selectIntG (RCon s rA)   t   = selectIntG rA t
> selectIntG _             t   = []

> selectIntWTree :: WTree Int Int -> [Int]
> selectIntWTree = selectIntG (rTree rInt rInt)

> selectIntPerfect :: Perfect Int -> [Int]
> selectIntPerfect = selectIntG (rPerfect rInt)

> selectSalaryG :: Rep a -> a -> [Salary]
> selectSalaryG (RInt ep) i = []
> selectSalaryG (RSum rA rB ep) t = case from ep t of
>                                   Inl a -> selectSalaryG rA a
>                                   Inr b -> selectSalaryG rB b
> selectSalaryG (RPair rA rB ep) t = case from ep t of
>                                    (a :*: b) -> selectSalaryG rA a ++ selectSalaryG rB b
> selectSalaryG (RType e rA ep) t = selectSalaryG rA (from ep t)

LIGD does not allow for a nice specification of ad-hoc cases,
so we are forced to make the hack below.

> selectSalaryG (RCon "S" a) t = case a of
>                                 RFloat ep -> [S (from ep t)]

> selectSalaryG (RCon s rA)   t = selectSalaryG rA t
> selectSalaryG _             t = []

> selectSalary :: Company -> [Salary]
> selectSalary | True = error ("LIGD does not allow ad-hoc cases that " ++
>                              "pattern match the original constructors.")
>              | otherwise = selectSalaryG rCompany

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies 
>   | True      = error ("LIGD does not allow ad-hoc cases that " ++
>                        "pattern match the original constructors.")
>   | otherwise = gmapQ selectSalaryG (rList rCompany)
