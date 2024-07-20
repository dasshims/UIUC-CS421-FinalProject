{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

{-

This test runs the infamous PARADISE benchmark,
which is the HELLO WORLD example of generic programming,
i.e., the "increase salary" function is applied to
a typical company just as shown in the boilerplate paper.

-}

module Paradise(increase) where

import CompanyDatatypes
import CompanyDats
import Syb4A


-- term1 = ([1::Int,2], (True,('2',[(3::Int,4::Int)])))

-- Increase salary by percentage
increase k x = gapp TL_recon (incS k :+: HNil) x

 -- "interesting" code for increase
 -- This is identical to the incS function from SYB1_2
incS :: Float -> Salary -> Salary -> Salary
incS k (S s) _ = S (s * (1+k))


test = increase 0.1 genCom
