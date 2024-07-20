> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

{----------------------------------------------------------------------------

 Module      :  GEq
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test exercices GENERIC show for the infamous company datatypes. The
 output of the program should be some representation of the infamous
 "genCom" company.

 AG: build arity check, perhaps extend the constructor record with an
 arity :: Int field and adapt the `derive' function accordingly.

----------------------------------------------------------------------------}

> module GShowDef  where
> import RepLib

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> class Rep1 GShowD a => GShow a where
>   gshow :: a -> String
>   gshow = gshowR1 rep1

> data GShowD a = GShowD { gshowD :: a -> String }

> gshowR1 :: R1 GShowD a -> a -> String
> gshowR1 (Data1 (DT str reps) cons) x = showCons cons
>     where showCons (Con emb reps : rest) = case (from emb x) of
>               Just kids -> if name emb == ":" then
>                                "((" ++ (name emb) ++ ")" ++ (gshowR1l reps kids) ++ ")"
>                            else if name emb == "[]" then -- should be arity check
>                                "[]" ++ (gshowR1l reps kids)
>                            else
>                                "(" ++ (name emb) ++ (gshowR1l reps kids) ++ ")"
>               Nothing   -> showCons rest
> gshowR1 Int1 x  = show x
> gshowR1 Char1 x  = show x
> gshowR1 Float1 x  = show x

> gshowR1l :: MTup GShowD l -> l -> String
> gshowR1l MNil Nil = ""
> gshowR1l (r :+: rs) (a :*: l) = " " ++ gshowD r a ++ gshowR1l rs l

> instance GShow a => Sat (GShowD a) where
>    dict = GShowD gshow


