> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

{----------------------------------------------------------------------------

 Module      :  GShow
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test exercices GENERIC show for the infamous company datatypes. The
 output of the program should be some representation of the infamous
 "genCom" company.

----------------------------------------------------------------------------}

> module GShow where

> import RepLib

> import CompanyDatatypes
> import CompanyReps()
> import GShowDef

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

|gmapQ1| is defined in RepLib.RepAux

Note that to call generic show, we are passing as an argument
|gshowD|, which is a function that takes a dictionary and produces
the function that gmapQ should apply.

Also gmapQ1 has a rather simple implementation. Probably it
would become more tricky if the traversal were deep.

> gapplyShowList :: [Int] -> [String]
> gapplyShowList = gmapQ1 gshowD


