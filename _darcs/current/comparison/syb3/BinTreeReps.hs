{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}
module BinTreeReps where

import BinTreeDatatype
import Data.Generics.SYB.WithClass.Basics
import Derive
import Data.Generics.SYB.WithClass.Instances
import Language.Haskell.TH()
import Data.Typeable

$(derive [''BinTree])


