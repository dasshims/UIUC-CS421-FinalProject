> module GEq (equalCompany, equalGRoseListInt, geq) where
> import LIGD

> import CompanyDatatypes
> import CompanyReps

> import GRoseDatatype
> import GRoseReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function geq}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> geq :: Rep tT -> tT -> tT  -> Bool
> geq (RInt        ep) t1 t2 =  from ep t1 == from ep t2
> geq (RChar       ep) t1 t2 =  from ep t1 == from ep t2
> geq (RFloat      ep) t1 t2 =  from ep t1 == from ep t2
> geq (RUnit       ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (Unit, Unit) -> True
> geq (RSum  rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> geq rA a1 a2
>                                    (Inr b1, Inr b2) -> geq rB b1 b2
>                                    _                -> False
> geq (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) ->
>                                      geq rA a1 a2 && geq rB b1 b2
> geq (RType e rA  ep) t1 t2 =  geq rA (from ep t1) (from ep t2)
> geq (RCon s rA)      t1 t2 =  geq rA t1 t2

----------------------------------------------------------------------------------

> equalCompany      :: Company -> Company -> Bool
> equalCompany      = geq rCompany

> equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
> equalGRoseListInt = geq (rGRose rList rInt)
