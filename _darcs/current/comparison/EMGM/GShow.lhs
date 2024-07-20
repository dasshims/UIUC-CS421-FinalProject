> {-# OPTIONS_GHC -fglasgow-exts #-}

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

> module GShow where
> import GL (GenericList(list))
> import CompanyDatatypes(Company)
> import CompanyReps(GenericCompany)
> import BinTreeDatatype(BinTree)
> import BinTreeReps()
> import GMapQ(gmapQ)
> import GShowDef(Gshows,gshows,applyGshow)

> instance GenericCompany Gshows -- default instance
> instance GenericList Gshows    -- default instance

> gshowsCompany     :: Company -> String
> gshowsCompany x   =  gshows x ""

> gapplyShowBinTree :: BinTree Int -> [String]
> gapplyShowBinTree =  gmapQ (\rep val -> applyGshow rep val "")

> gapplyShowList    :: [Int] -> [String]
> gapplyShowList    =  gmapQ (\rep val -> applyGshow rep val "")
