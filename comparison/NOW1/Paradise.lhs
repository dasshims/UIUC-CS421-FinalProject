{-# OPTIONS -fglasgow-exts #-}

> module Paradise  where

> import NOW hiding (S)
> import CompanyDatatypes
> import GRoseDatatype

> inc :: Float -> Typed a -> Typed a
> inc i x = (typeOf x :> fromSpine (incSpines i (toSpine x)))

> incSpines :: Float -> Spine a -> Spine a
> incSpines i (f :$ (SalaryR :> s)) = incSpines i f :$ (SalaryR :> incS i s)
> incSpines i (f :$ x) = incSpines i f :$ x
> incSpines _ (Con c) = (Con c)

> increase :: Float -> Company -> Company
> increase f c = val (inc f (CompanyR :> c))

> -- "interesting" code for increase
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
