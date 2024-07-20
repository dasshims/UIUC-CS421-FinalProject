> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances#-}

{----------------------------------------------------------------------------

 Module      :  FoldTree
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test accumulates all Ints and Salaries in a datastructure. It uses
 two possible ways to implement the functionality. One uses the SYB
 approach, the other writes out a `full' type-indexed function.

----------------------------------------------------------------------------}

> module FoldTree where

> import RepLib
> import CompanyDatatypes
> import CompanyReps()
> import TreeDatatype
> import PerfectDatatype
> -- import Language.Haskell.TH()

> $(derive [''WTree])

> $(derive 
>     [''Perfect,
>        ''Fork
>     ])

> class Rep1 GFoldTreeD a => GFoldTree a where
>   gfoldtree :: a -> [Int]
>   gfoldtree = gfoldtreeR1 rep1

> data GFoldTreeD a = GFoldTreeD { gfoldtreeD :: a -> [Int] }

> gfoldtreeR1 :: R1 GFoldTreeD a -> a -> [Int]
> gfoldtreeR1 Int1              x  = [x]
> gfoldtreeR1 (Arrow1 r1 r2)    f  = error "urk"
> gfoldtreeR1 (Data1 dt cons)   x  =
>   case (findCon cons x) of
>       Val emb rec kids ->
>         foldl_l (\ca a b -> a ++  (gfoldtreeD ca b)) [] rec kids
> gfoldtreeR1 _                 x  = []

> instance GFoldTree a => Sat (GFoldTreeD a) where
>    dict = GFoldTreeD gfoldtree

> instance GFoldTree Float
> instance GFoldTree Int
> instance GFoldTree Bool
> instance GFoldTree ()
> instance GFoldTree Integer
> instance GFoldTree Char
> instance GFoldTree Double
> instance (GFoldTree a, GFoldTree b) => GFoldTree (a,b)
> instance (GFoldTree a) => GFoldTree [a]

> instance (GFoldTree a, GFoldTree w) => GFoldTree (WTree a w)
> instance GFoldTree a => GFoldTree (Perfect a)
> instance GFoldTree a => GFoldTree (Fork a)
> instance GFoldTree Company
> instance GFoldTree Dept
> instance GFoldTree Unit
> instance GFoldTree Employee
> instance GFoldTree Person
> instance GFoldTree Salary

Ad-hoc cases with SYB-style combinators that use casting

> selectIntWTree :: Rep a => a -> [Int]
> selectIntWTree = everything (++) ([] `mkQ` (:[]))

> selectIntPerfect :: Rep a => a -> [Int]
> selectIntPerfect = everything (++) ([] `mkQ` (:[]))

Ad-hoc cases using type classes and real higher-order generic functions
--------------------------

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = gmapQ1 selectSalaryD

Note that that selectSalary's default method implementation could
use the function everything. I tried to define it but it was
too difficult, so I just inlined it.

> class Rep1 SelectSalaryD a => SelectSalary a where
>   selectSalary :: a -> [Salary]
>   selectSalary x = concat (gmapQ1 selectSalaryD x)

> instance SelectSalary Salary where
>   selectSalary x = [x]

> data SelectSalaryD a = SelectSalaryD { selectSalaryD :: a -> [Salary] }

> instance SelectSalary a => Sat (SelectSalaryD a) where
>   dict = SelectSalaryD selectSalary

Common instances

> instance SelectSalary Bool
> instance SelectSalary Float
> instance (SelectSalary a,Rep a) => SelectSalary [a]
> instance SelectSalary Company
> instance SelectSalary Dept
> instance SelectSalary Unit
> instance SelectSalary Employee
> instance SelectSalary Char
> instance SelectSalary Person



