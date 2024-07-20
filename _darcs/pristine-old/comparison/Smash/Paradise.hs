{-# OPTIONS -fglasgow-exts #-}

{-

This test runs the infamous PARADISE benchmark,
which is the HELLO WORLD example of generic programming,
i.e., the "increase salary" function is applied to
a typical company just as shown in the boilerplate paper.

-}

module Smash.Paradise(increase) where

import CompanyDatatypes
import Smash.CompanyTDats
import Smash.Syb4


term1 = ([1::Int,2], (True,('2',[(3::Int,4::Int)])))

-- Increase salary by percentage
increase k x = gtmapq (incS k :+: HNil) x

 -- "interesting" code for increase
 -- This is identical to the incS function from SYB1_2
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))


test = increase 0.1 genCom
