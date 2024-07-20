> {-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances #-}

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

> module GShow  where
> import RepLib
> import CompanyDatatypes
> import CompanyReps

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> class Rep a => GShow a where
>   gshow :: a -> String
>   gshow = gshowR rep

> data GShowD a = GShowD { gshowD :: a -> String }

> gshowR :: R a -> a -> String
> gshowR (Data (DT str reps) cons) x = showCons cons
>     where showCons (Con emb reps : rest) = case (from emb x) of
>               Just kids -> if name emb == ":" then
>                                "((" ++ (name emb) ++ ")" ++ (gshowRl reps kids) ++ ")"
>                            else if name emb == "[]" then -- should be arity check
>                                "[]" ++ (gshowRl reps kids)
>                            else
>                                "(" ++ (name emb) ++ (gshowRl reps kids) ++ ")"
>               Nothing   -> showCons rest
> gshowR Int x  = show x
> gshowR Char x  = show x
> gshowR Float x  = show x

> gshowRl :: MTup R l -> l -> String
> gshowRl MNil Nil = ""
> gshowRl (r :+: rs) (a :*: l) = " " ++ gshowR r a ++ gshowRl rs l

> instance GShow a => Sat (GShowD a) where
>    dict = GShowD gshow

> instance GShow Float
> instance GShow Int
> instance GShow Bool
> instance GShow ()
> instance GShow Integer
> instance GShow Char
> instance GShow Double
> instance (GShow a, GShow b) => GShow (a,b)
> instance (GShow a) => GShow [a]

> instance GShow Company
> instance GShow Dept
> instance GShow Unit
> instance GShow Employee
> instance GShow Person
> instance GShow Salary

> gshowsCompany :: Company -> String
> gshowsCompany = gshow
