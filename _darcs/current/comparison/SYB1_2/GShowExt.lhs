{-# OPTIONS_GHC -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

> module GShowExt (gshowsCompany) where
> import Data.Generics(Data, showConstr, toConstr, gmapQ, extQ, ext1Q)
> import CompanyDatatypes(Company)
> import BinTreeDatatype(BinTree)
> import qualified Common(showList)

Generic show can be made extensible if the recursion occurrences
are abstracted. The user can tie the recursive knot and add more cases
as in gshowExt. Although this test does not allow it, it is still useful
to have as an alternative implementation of extensibility. 

> gshow' :: Data a => (forall b . Data b => b -> String) -> a -> String
> gshow' gshowRec = gshow'' `extQ` showChar
>   where
>     showChar :: Char -> String
>     showChar c = '\'':c:'\'':[]
>     gshow'' :: Data a => a -> String
>     gshow'' t = paren (length fields) (showConstr (toConstr t) ++ concat fields)
>       where  
>         fields = gmapQ ((++) " " . gshowRec) t
>         paren nfields str = if nfields == 0 then str else "(" ++ str ++ ")"

> gshowExt :: Data a => a -> String
> gshowExt = gshow' gshowExt `ext1Q` listCase

> listCase :: Data a => [a] -> String
> listCase = Common.showList gshowExt


> gshowsCompany :: Company -> String
> gshowsCompany = error "Support for extension in SYB only works for functions with an untied recursive knot (not allowed for the test: big burden for users)."
> -- gshowsCompany = gshowExt



