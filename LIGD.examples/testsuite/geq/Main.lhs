{-# OPTIONS  #-}

{-


This test exercices GENERIC read, show, and eq for the company
datatypes which we use a lot. The output of the program should be
"True" which means that "gread" reads what "gshow" shows while the
read term is equal to the original term in terms of "geq".


-}

> module Main where

> import LIGD
> import CompanyDatatypes
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function eq}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> rEqual :: Rep tT -> tT -> tT -> Bool
> rEqual (RInt ep) t1 t2        =  from ep t1 == from ep t2
> rEqual (RChar ep) t1 t2       =  from ep t1 == from ep t2
> rEqual (RFloat ep) t1 t2      =  from ep t1 == from ep t2
> rEqual (RUnit ep) t1 t2       =  case (from ep t1, from ep t2) of
>                                    (Unit, Unit) -> True
> rEqual (RSum rA rB ep) t1 t2  =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> rEqual rA a1 a2
>                                    (Inr b1, Inr b2) -> rEqual rB b1 b2
>                                    _            -> False
> rEqual (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) -> 
>                                      rEqual rA a1 a2 && rEqual rB b1 b2
> rEqual (RType e rA  ep) t1 t2 =  rEqual rA (from ep t1) (from ep t2)
> rEqual (RCon s rA) t1 t2      =  rEqual rA t1 t2

----------------------------------------------------------------------------------

> main = print ( rEqual rCompany genCom genCom
>              , rEqual rCompany genCom genCom'
>             )

