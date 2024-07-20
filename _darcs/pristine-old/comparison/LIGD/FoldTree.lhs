> module FoldTree where

> import LIGD
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps

Accumulate all Ints in a datastructure.
Just a simple type-indexed function. Not protected by a type class.

> listifyIntG :: Rep a -> a -> [Int]
> listifyIntG (RInt ep) i       = [from ep i]
> listifyIntG (RSum rA rB ep) t = case from ep t of
>                                Inl a -> listifyIntG rA a
>                                Inr b -> listifyIntG rB b
> listifyIntG (RPair rA rB ep) t= case from ep t of
>                                 (a :*: b) -> listifyIntG rA a ++ listifyIntG rB b
> listifyIntG (RType e rA ep) t = listifyIntG rA (from ep t)
> listifyIntG (RCon s rA)   t   = listifyIntG rA t
> listifyIntG _             t   = []

> listifyInt :: Tree Int Int -> [Int]
> listifyInt = listifyIntG (rTree rInt rInt)

> listifySalaryG :: Rep a -> a -> [Salary]
> listifySalaryG (RInt ep) i = []
> listifySalaryG (RSum rA rB ep) t = case from ep t of
>                                   Inl a -> listifySalaryG rA a
>                                   Inr b -> listifySalaryG rB b
> listifySalaryG (RPair rA rB ep) t = case from ep t of
>                                    (a :*: b) -> listifySalaryG rA a ++ listifySalaryG rB b
> listifySalaryG (RType e rA ep) t = listifySalaryG rA (from ep t)

LIGD does not allow for a nice specification of ad-hoc cases,
so we are forced to make the hack below.

> listifySalaryG (RCon "S" a) t = case a of
>                                 RFloat ep -> [S (from ep t)]

> listifySalaryG (RCon s rA)   t = listifySalaryG rA t
> listifySalaryG _             t = []

> listifySalary :: Company -> [Salary]
> listifySalary = listifySalaryG rCompany
