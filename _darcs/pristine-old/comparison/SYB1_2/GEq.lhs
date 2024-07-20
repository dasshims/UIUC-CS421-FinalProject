{-# OPTIONS -fglasgow-exts #-}

{-

This test exercices GENERIC eq for the company
datatypes which we use a lot. The output of the program should be
"(True, False)"

-}

> module GEq (equalCompany, equalGRoseListInt) where
> import Data.Generics hiding (geq)
> import CompanyDatatypes

> import GRoseDatatype
> import GRoseInstances

> geq :: Data a => a -> a -> Bool

(Copied from GHC's libraries/base/Data/Generics/Twins.hs )

---8-<---
Testing for equality of two terms goes like this. Firstly, we
establish the equality of the two top-level datatype
constructors. Secondly, we use a twin gmap combinator, namely tgmapQ,
to compare the two lists of immediate subterms.

(Note for the experts: the type of the worker geq' is rather general
but precision is recovered via the restrictive type of the top-level
operation geq. The imprecision of geq' is caused by the type system's
unability to express the type equivalence for the corresponding
couples of immediate subterms from the two given input terms.)
---8-<---

Note the use of the |gzipWithQ| combinator.

> geq x y = geq' x y
>   where
>     geq' :: GenericQ (GenericQ Bool)
>     geq' x y =     (toConstr x == toConstr y)
>                 && and (gzipWithQ geq' x y)

----------------------------------------------------------------------------------

> equalCompany :: Company -> Company -> Bool
> equalCompany = geq

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = geq
