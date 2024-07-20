> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GShow (gshowsCompany, gshowExpr) where

> import NOW hiding (show, Expr)
> import CompanyDatatypes
> import GADT

> gshow :: Typed a -> String
> gshow (IntR :> i) = show i
> gshow (CharR :> c) = show c
> gshow (FloatR :> f) = show f
> gshow (ListR a :> []) = "[]" -- very nasty, due to the fact that spines are traversed from back to front, so you don't know whether it is an arity zero constructor. In which case you should leave out the parentheses.
> gshow x = gshowSpines (toSpine x)  ++ ")"

> gshowSpines :: Spine a -> String
> gshowSpines (f :$ x) = gshowSpines f ++ " " ++ gshow x
> gshowSpines (Con c) = if arity c == 0 then
>                           name c
>                       else
>                           "(" ++ name c

> gshowsCompany :: Company -> String
> gshowsCompany c = gshow (CompanyR :> c)

> gshowExpr :: Expr Int -> String
> gshowExpr e = gshow (ExprR' IntR:> e)