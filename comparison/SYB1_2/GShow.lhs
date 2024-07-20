{-# OPTIONS_GHC -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

> module GShow (gshowsCompany, gapplyShowBinTree, gapplyShowList) where
> import Data.Generics(Data, showConstr, toConstr, gmapQ, extQ)
> import CompanyDatatypes(Company)
> import CompanyReps
> import BinTreeDatatype(BinTree)
> import BinTreeReps
> import Common

Generic show

> gshow' :: Data a => a -> String
> gshow' = gshow'' `extQ` showChar
>   where
>     showChar :: Char -> String
>     showChar c = '\'':c:'\'':[]
>     gshow'' :: Data a => a -> String
>     gshow'' t = paren (length fields /= 0) (showConstr (toConstr t) ++ concat fields)
>       where  
>         fields = map (" "++) (gmapQ gshow' t)

> gshowsCompany :: Company -> String
> gshowsCompany = gshow'

> gshowsListInt :: [Int] -> [String]
> gshowsListInt = gmapQ gshow'

> gapplyShowBinTree :: BinTree Int -> [String]
> gapplyShowBinTree = gmapQ gshow'

> gapplyShowList :: [Int] -> [String]
> gapplyShowList = gmapQ gshow'

