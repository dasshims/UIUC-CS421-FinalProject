> module Paradise (increase) where

> import GMsec2
> import CompanyDatatypes hiding (Unit)
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> newtype Gincrease a             =  Gincrease { applyGincrease :: Float -> a -> a }

> gincrease                       :: (TypeRep a) => Float -> a -> a
> gincrease                       =  applyGincrease typeRep

> instance Generic Gincrease where
>   unit                        =  Gincrease (\ f x -> x)
>   plus                        =  Gincrease (\ f x -> case x of
>                                                 Inl l -> Inl (gincrease f l)
>                                                 Inr r -> Inr (gincrease f r))
>   pair                        =  Gincrease (\ f x -> Pair (gincrease f (outl x)) (gincrease f (outr x)))
>   datatype iso                =  Gincrease (\ f x -> toData iso (gincrease f (fromData iso x)))
>   constr                      =  Gincrease (\ f x -> Constr (name x) (arity x) (gincrease f (arg x)))
> --  salary                      =  Gincrease (\ f x -> incS f x)
>   catchall                    =  Gincrease (\ f x -> x)

> -- "interesting" code for increase
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))

> increase :: Float -> Company -> Company
> increase = error "Not possible to implement using GM library"


