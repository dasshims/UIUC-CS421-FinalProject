> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

{----------------------------------------------------------------------------

 Module      :  GShowExt
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test exercices GENERIC show for the infamous company datatypes. The
 output of the program should be some representation of the infamous
 "genCom" company.

----------------------------------------------------------------------------}

> module GShowExt  where
> import CompanyDatatypes
> import CompanyReps()
> import GShowDef(GShow(gshow))
> import Common(showList)
> import Prelude hiding (showList)

> instance GShow Float
> instance GShow Int
> instance GShow Bool
> instance GShow ()
> instance GShow Integer
> instance GShow Char
> instance GShow Double
> instance (GShow a, GShow b) => GShow (a,b)

> instance (GShow a) => GShow [a] where
>   gshow = showList gshow -- ad hoc case for lists

> instance GShow Company
> instance GShow Dept
> instance GShow Unit
> instance GShow Employee
> instance GShow Person
> instance GShow Salary

> gshowsCompany :: Company -> String
> gshowsCompany = gshow

