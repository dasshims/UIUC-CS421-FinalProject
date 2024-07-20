{-#  OPTIONS -fglasgow-exts  #-}

> module FoldTree where

> import NOW hiding (S)
> import TreeDatatype
> import CompanyDatatypes

Accumulate all ints in a datastructure
just a simple type-indexed function. Not protected by a type class.

> listInt  ::  Typed a ->  [Int]
> listInt (IntR :> i)                 =   [i]
> listInt (SpineR  a :> Con c)        =   []
> listInt (SpineR  a :> (f :$ x))     =   listInt (SpineR  (typeOf x :-> a) :> f) ++ listInt x
> listInt x                           =   listInt (SpineR  (typeOf x) :> toSpine x)
> listifyInt t = listInt (TreeR IntR IntR :> t)

> listSalary  ::  Typed a ->  [Salary]
> listSalary (SalaryR :> s)              =   [s]
> listSalary (SpineR  a :> Con c)        =   []
> listSalary (SpineR  a :> (f :$ x))     =   listSalary (SpineR  (typeOf x :-> a) :> f) ++ listSalary x
> listSalary x                           =   listSalary (SpineR  (typeOf x) :> toSpine x)
> listifySalary c = listSalary (CompanyR :> c)
