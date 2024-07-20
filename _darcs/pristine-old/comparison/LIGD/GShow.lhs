{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow.
Instead, it produces the same output as deriving Show would.

-}

> module GShow (gshowsCompany) where

> import LIGD
> import CompanyDatatypes
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> gShows :: Rep tT -> tT -> ShowS
> gShows (RInt         ep) t    =  shows (from ep t)
> gShows (RChar        ep) t    =  shows (from ep t)
> gShows (RUnit        ep) t    =  showString ""
> gShows (RFloat       ep) t    =  shows (from ep t)
> gShows (RSum rA rB   ep) t    =  case from ep t of
>                                    Inl a -> gShows rA a
>                                    Inr b -> gShows rB b
> gShows (RPair rA rB  ep) t    =  case from ep t of
>                                    (a :*: b) -> gShows rA a `o` showString " " `o` gShows rB b
> gShows (RType e rA   ep) t    =  gShows rA (from ep t)
> gShows (RCon  s (RUnit ep)) t =  showString s
> gShows (RCon  s rA)      t    =  showChar '(' `o` showString s `o` showChar ' '
>                                  `o` gShows rA t `o` showChar ')'

> gshowsCompany :: Company -> String
> gshowsCompany x = gShows rCompany x ""
