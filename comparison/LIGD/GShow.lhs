This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

> module GShow (gshowsCompany, gapplyShowBinTree, gapplyShowList) where

> import LIGD(rInt, rList)
> import GShowDef(gShows)
> import CompanyDatatypes(Company)
> import CompanyReps(rCompany)
> import BinTreeDatatype(BinTree)
> import BinTreeReps(rBinTree)
> import GMapQ(gmapQ)

> gshowsCompany     :: Company -> String
> gshowsCompany x   =  gShows rCompany x ""

> gapplyShowBinTree :: BinTree Int -> [String]
> gapplyShowBinTree =  gmapQ (\ rep val -> gShows rep val "") (rBinTree rInt)

> gapplyShowList    :: [Int] -> [String]
> gapplyShowList    =  gmapQ (\ rep val -> gShows rep val "") (rList rInt)
