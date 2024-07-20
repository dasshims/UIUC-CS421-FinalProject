{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}
module TreeReps where

import TreeDatatype
import Derive
import Data.Generics.SYB.WithClass.Instances
import Language.Haskell.TH()

$(derive [''WTree])
