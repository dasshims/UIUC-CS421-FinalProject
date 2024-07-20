{-# OPTIONS -fglasgow-exts #-}

> module Paradise  where

> import SYB1
> import CompanyDatatypes
> import GRoseDatatype

Traversals, from "Scrap your boilerpalte, Reloaded"

> type Traversal = forall a. Type a -> a -> a

> mapT :: Traversal -> Traversal
> mapT h t x = fromSpine $ mapT' h $ toSpine (t :> x)

> mapT' :: Traversal -> (forall a. Spine a -> Spine a)
> mapT' h (Con c)           =  Con c
> mapT' h (f :$ (t :> x))   =  mapT' h f :$ (t :> h t x)

> everywhere_bu :: Traversal -> Traversal
> everywhere_bu f t = f t . mapT (everywhere_bu f) t

> everywhere_td :: Traversal -> Traversal
> everywhere_td f t = mapT (everywhere_td f) t . f t

Paradise functions:

> -- Increase salary by percentage
> increase :: Float -> Company -> Company
> increase k = everywhere_bu (mkT (incS k)) CompanyR
>   where
>     mkT :: (Salary -> Salary) -> Type a -> a -> a
>     mkT f SalaryR salary = f salary
>     mkT f _ non_salary    = non_salary

> -- "interesting" code for increase
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
