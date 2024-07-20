> module Paradise (increase) where

> import LIGD
> import CompanyDatatypes
> import CompanyReps

> rIncrease :: Float -> Rep a -> a -> a
> rIncrease f (RSum a b   ep)  t  =  case from ep t of
>                                     Inl x ->  to ep (Inl (rIncrease f a x))
>                                     Inr y ->  to ep (Inr (rIncrease f b y))
> rIncrease f (RPair a b  ep)  t  =  case from ep t of
>                                     x :*: y -> to ep (rIncrease f a x :*: rIncrease f b y)
> rIncrease f (RType e a  ep)  t  =  to ep (rIncrease f a (from ep t))
> rIncrease f (RCon "S" a)     t  =  case a of
>                                   RFloat ep -> to ep (incS f (from ep t))
> rIncrease f (RCon s a)       t  =  rIncrease f a t
> rIncrease f _                t  =  t


> -- Specific case for salaries
> incS :: Float -> Float -> Float
> incS f s = (s * (1+f))

> increase :: Float -> Company -> Company
> increase f c = rIncrease f rCompany c
