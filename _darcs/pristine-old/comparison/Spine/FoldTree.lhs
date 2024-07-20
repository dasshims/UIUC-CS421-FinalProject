{-# OPTIONS -fglasgow-exts #-}

> module FoldTree  where

> import SYB1 hiding (Tree)
> import TreeDatatype
> import CompanyDatatypes
> import GRoseDatatype

Traversals, from "Scrap your boilerpalte, Reloaded"

> type Query r = forall a. Type a -> a -> r

> mapQ :: Query r -> Query [r]
> mapQ q t x = mapQ' q $ toSpine (t :> x)

> mapQ' :: Query r -> (forall a. Spine a -> [r])
> mapQ' q (Con c)          =  []
> mapQ' q (f :$ (t :> x))  =  mapQ' q f ++ [q t x]

> everything' :: Query r -> Query [r]
> everything' q t x = [q t x] ++ concat (mapQ (everything' q) t x)

> everything :: (r -> r -> r) -> Query r -> Query r
> everything op q t x = foldl1 op ([q t x] ++ mapQ (everything op q) t x)

> listifyInt :: Tree Int Int -> [Int]
> listifyInt = everything (++) ([] `mkQ` (:[])) (TreeWR IntR IntR)
>   where
>     mkQ :: [Int] -> (Int -> [Int]) -> Type a -> a -> [Int]
>     mkQ zero lift IntR i = lift i
>     mkQ zero lift _ _    = zero

> listifySalary :: Company -> [Salary]
> listifySalary = everything (++) ([] `mkQ` (:[])) CompanyR
>   where
>     mkQ :: [Salary] -> (Salary -> [Salary]) -> Type a -> a -> [Salary]
>     mkQ zero lift SalaryR i = lift i
>     mkQ zero lift _ _    = zero
