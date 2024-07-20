> {-# OPTIONS_GHC -fglasgow-exts -fgenerics #-}

> module FoldTree where

> import TreeDatatype
> import CompanyDatatypes
> import Data.Generics hiding (Unit) 
> import qualified Data.Generics as DG

> ouCom :: Company
> ouCom = C [  D "Research" jeuring [PU heeren, PU gerdes],
>               D "Staff" rodriguez   []]

> jeuring, rodriguez, heeren, gerdes :: Employee
> jeuring    = E (P "Jeuring"  "Utrecht")  (S 10000.0)
> rodriguez  = E (P "Joost"    "Utrecht")  (S 2000.0)
> heeren     = E (P "Marlow"   "Heerlen")  (S 8000.0)
> gerdes     = E (P "Blair"    "Emmen")    (S 500.0)

> class Select a where
>   selectInt    :: a -> [Int]
>   selectSalary :: a -> [Salary]
>
>   selectInt {| DG.Unit |}  DG.Unit    = []
>   selectInt {| a :+: b |}  (Inl x)    = selectInt x
>   selectInt {| a :+: b |}  (Inr y)    = selectInt y 
>   selectInt {| a :*: b |}  (x :*: y)  = selectInt x ++ selectInt y
>
>   selectSalary {| DG.Unit |}  DG.Unit    = []
>   selectSalary {| a :+: b |}  (Inl x)    = selectSalary x
>   selectSalary {| a :+: b |}  (Inr y)    = selectSalary y 
>   selectSalary {| a :*: b |}  (x :*: y)  = selectSalary x ++ selectSalary y


> instance Select Int where
>   selectInt     x = [x]
>   selectSalary  x = []

> instance Select Char where
>   selectInt     x = []
>   selectSalary  x = []

> instance Select Float where
>   selectInt     x = []
>   selectSalary  x = []

> instance Select a => Select [a]

> instance (Select a, Select w) => Select (WTree a w)

> instance Select Company
> instance Select Dept
> instance Select Unit
> instance Select Employee
> instance Select Person
> instance Select Salary where
>   selectInt     x = []
>   selectSalary  x = [x]

