{-# OPTIONS #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow.
Instead, it produces the same output as deriving Show would.

-}

> module GShow (gshowsCompany) where

> import GMsec2
> import CompanyDatatypes hiding (Unit)
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> newtype Gshows a             =  Gshows { applyGshow :: a -> ShowS }

> gshows                       :: (TypeRep a) => a -> ShowS
> gshows                       =  applyGshow typeRep

> instance Generic Gshows where
>   unit                        =  Gshows (\ _x -> showString "")
>   plus                        =  Gshows (\ x -> case x of
>                                                 Inl l -> gshows l
>                                                 Inr r -> gshows r)
>   pair                        =  Gshows (\ x -> gshows (outl x) . showString " " . gshows (outr x))
>   datatype iso                =  Gshows (\ x -> gshows (fromData iso x))
>   char                        =  Gshows (\ x -> shows x)
>   int                         =  Gshows (\ x -> shows x)
>   float                       =  Gshows (\ x -> shows x)
>   constr                      =  Gshows (\ x -> if arity x == 0 then
>                                                     showString (name x)
>                                                 else
>                                                     showChar '(' . showString (name x) . showChar ' '
>                                                         . gshows (arg x) . showChar ')')
>   catchall                    =  Gshows (\ x -> showString "")

> gshowsCompany :: Company -> String
> gshowsCompany x = gshows x ""


