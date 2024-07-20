-- The test of efficiency

module Efficiency (bigeq) where

import GEq
import FullTree

bigeq :: Int -> Bool
bigeq n = geq t t
   where t = genBinTree n


