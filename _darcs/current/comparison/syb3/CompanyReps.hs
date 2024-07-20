{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}
module CompanyReps where

import CompanyDatatypes
--import Derive
  --The generator in this directory makes broken Data instances for
  --Company. So we use the default one below:
import Data.Generics.SYB.WithClass.Derive
import Data.Generics.SYB.WithClass.Instances
import Language.Haskell.TH()

$(derive [''Company,''Dept,''Unit,''Employee,''Person,''Salary])



