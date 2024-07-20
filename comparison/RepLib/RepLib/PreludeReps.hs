{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  RepLib.PreludeReps
-- Copyright   :  (c) The University of Pennsylvania, 2006
-- License     :  BSD
--
-- Maintainer  :  sweirich@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- Automatically derive representations for prelude types
--
-----------------------------------------------------------------------------
module RepLib.PreludeReps where

import RepLib.R
import RepLib.R1
import RepLib.Derive
import Language.Haskell.TH

$(derive [''Bool,
          ''Maybe,
          ''Either,
          ''Ordering,
          tupleTypeName 3,
          tupleTypeName 4,
          tupleTypeName 5,
          tupleTypeName 6,
          tupleTypeName 7])


