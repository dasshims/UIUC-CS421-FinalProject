> module Nested where

> import Data.Generics hiding (geq)
> import PerfectDatatype
> import PerfectReps
> import GEq

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect = geq
