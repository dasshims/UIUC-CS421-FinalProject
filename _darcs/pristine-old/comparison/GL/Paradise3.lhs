{-# OPTIONS #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow. 
Instead, it produces the same output as deriving Show would.

-}

> module Paradise3 (increase) where

> import GL3
> import CompanyDatatypes
> import CompanyReps3
> import Data.Generics hiding (Generic)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> newtype Gincrease a b c       =  Gincrease { applyGincrease :: Float -> a -> a }

> instance Generic Gincrease where
>   unit                        =  Gincrease (\ f x -> x)
>   plus a b                    =  Gincrease (\ f x -> case x of
>                                                 Inl l -> Inl (applyGincrease a f l)
>                                                 Inr r -> Inr (applyGincrease b f r))
>   prod a b                    =  Gincrease (\ f x -> (applyGincrease a f (outl x)) :*: (applyGincrease b f (outr x)))
>   view iso _ _ a              =  Gincrease (\ f x -> to iso (applyGincrease a f (from iso x)))
>   int                         =  Gincrease (\ f x -> x)
>   char                        =  Gincrease (\ f x -> x)
>   float                       =  Gincrease (\ f x -> x)
> instance GenericCompany Gincrease where
>   salary                      =  Gincrease (\ f x -> incS f x)


> -- "interesting" code for increase
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))

> increase :: Float -> Company -> Company
> increase = applyGincrease over


