> {-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances#-}

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
> import CompanyReps
> import TreeDatatype
> import Language.Haskell.TH

> $(derive [''Tree])

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

> instance (GFoldTree a, GFoldTree w) => GFoldTree (Tree a w)
> instance GFoldTree Company
> instance GFoldTree Dept
> instance GFoldTree Unit
> instance GFoldTree Employee
> instance GFoldTree Person
> instance GFoldTree Salary

> listifyInt :: (GFoldTree a) => a -> [Int]
> listifyInt = gfoldtree

> listifySalary :: Company -> [Salary]
> listifySalary = everything (++) ([] `mkQ` (:[]))
