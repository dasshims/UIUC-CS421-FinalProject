{-# OPTIONS -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

> module GShow (gshowsCompany) where
> import Data.Generics
> import CompanyDatatypes

Here is another exercise:
The following function gshow' is a completely generic variation on gshow.
It would print strings as follows:

*Main> gshow' "abc"
"((:) ('a') ((:) ('b') ((:) ('c') ([]))))"

The original gshow does a better job because it is customised for strings:

*Main> gshow "foo"
"\"foo\""

In fact, this is what Haskell's normal show would also do:

*Main> show "foo"
"\"foo\""

Generic show

> gshow' :: Data a => a -> String
> gshow' t =  paren (length fields) (showConstr (toConstr t) ++ concat fields)
>     where
>       fields = gmapQ ((++) " " . tgshow) t
>       paren nfields str = if nfields == 0 then str else "(" ++ str ++ ")"

Generic show extended with specific cases

> tgshow :: Data a => a -> String
> tgshow = gshow' `extQ` showChar
>     where
>     showChar :: Char -> String
>     showChar c = '\'':c:'\'':[]

> gshowsCompany :: Company -> String
> gshowsCompany = gshow'
