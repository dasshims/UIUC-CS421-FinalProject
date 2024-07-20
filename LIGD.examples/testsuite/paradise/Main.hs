module Main where

import LIGD
import CompanyDatatypes
import CompanyReps

increase :: Float -> Rep a -> a -> a 
increase f (RSum a b   ep)  t  =  case from ep t of 
                                    Inl x ->  to ep (Inl (increase f a x))
                                    Inr y ->  to ep (Inr (increase f b y))
increase f (RPair a b  ep)  t  =  case from ep t of
                                    x :*: y -> to ep (increase f a x :*: increase f b y)
increase f (RType e a  ep)  t  =  to ep (increase f a (from ep t))
increase f (RCon "S" a)     t  =  case a of
										      RFloat ep -> to ep (incS f (from ep t))
increase f (RCon s a)       t  =  increase f a t
increase f _                t  =  t


-- Specific case for salaries
incS :: Float -> Float -> Float
incS f s = (s * (1+f))

main = print $ increase 0.1 rCompany genCom
