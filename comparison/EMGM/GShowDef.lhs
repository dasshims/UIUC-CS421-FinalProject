> {-# OPTIONS_GHC -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow.
Instead, it produces the same output as deriving Show would.

-}

> module GShowDef where

> import GL
> import CompanyDatatypes
> import CompanyReps
> import Data.Generics hiding (Generic, gmapQ)
> import GMapQ
> import Data.List

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> newtype Gshows a             =  Gshows { applyGshow :: a -> ShowS }

> instance Generic Gshows where
>   unit                        =  Gshows (\ _x -> showString "")
>   plus a b                    =  Gshows (\ x -> case x of
>                                                 Inl l -> applyGshow a l
>                                                 Inr r -> applyGshow b r)
>   prod a b                    =  Gshows (\ x -> applyGshow a (outl x) . showString " " . applyGshow b (outr x))
>   view iso a                  =  Gshows (\ x -> applyGshow a (from iso x))
>   char                        =  Gshows (\ x -> shows x)
>   int                         =  Gshows (\ x -> shows x)
>   float                       =  Gshows (\ x -> shows x)
>   constr n ar a               =  Gshows (\ x -> if ar == 0 then
>                                                     showString (n)
>                                                 else
>                                                     showChar '(' . showString (n) . showChar ' '
>                                                         . (applyGshow a (x)) . showChar ')')

> gshows                       :: GRep Gshows a => a -> ShowS
> gshows                       =  applyGshow over
