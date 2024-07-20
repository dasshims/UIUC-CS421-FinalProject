{-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances #-}

-- The organisational structure of a company

module CompanyReps where

import RepLib hiding (rUnit)
import CompanyDatatypes
import Language.Haskell.TH hiding (Name)


$(derive 
    [''Company, 
     ''Dept, 
     ''Unit, 
     ''Employee, 
 	  ''Person, 
 	  ''Salary])

rName :: R Name
rName = rep

rAddress :: R Address
rAddress = rep

rManager :: R Manager
rManager = rep
