{-# OPTIONS #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow. 
Instead, it produces the same output as deriving Show would.

-}

> module Main where

> import LIGD
> import CompanyDatatypes
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> rShows :: Rep tT -> tT -> ShowS
> rShows (RInt         ep) t    =  shows (from ep t)
> rShows (RChar        ep) t    =  shows (from ep t)
> rShows (RUnit        ep) t    =  showString ""
> rShows (RFloat       ep) t    =  shows (from ep t)
> rShows (RSum rA rB   ep) t    =  case from ep t of
>                                    Inl a -> rShows rA a
>                                    Inr b -> rShows rB b
> rShows (RPair rA rB  ep) t    =  case from ep t of
>                                    (a :*: b) -> rShows rA a `o` showString " " `o` rShows rB b
> rShows (RType e rA   ep) t    =  rShows rA (from ep t)
> rShows (RCon  s (RUnit ep)) t =  showString s
> rShows (RCon  s rA)      t    =  showChar '(' `o` showString s `o` showChar ' '
>                                  `o` rShows rA t `o` showChar ')'

> main = print $ rShows rCompany genCom ""

