{-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances#-}

{----------------------------------------------------------------------------

 Module      :  Paradise
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD
 
 This test runs the infamous PARADISE benchmark,
 which is the HELLO WORLD example of generic programming,
 i.e., the "increase salary" function is applied to
 a typical company just as shown in the boilerplate paper.

----------------------------------------------------------------------------}

> module Paradise (increase) where

> import RepLib
> import CompanyDatatypes
> import CompanyReps

Increase salary by percentage

> increase :: Float -> Company -> Company
> increase k = everywhere (mkT (incS k))

"interesting" code for increase

> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
