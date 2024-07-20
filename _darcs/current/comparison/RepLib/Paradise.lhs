{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances#-}

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
> import CompanyDatatypes(Company)
> import UpdateSalaryDef(updateSalary)

> increase :: Float -> Company -> Company
> increase = updateSalary
