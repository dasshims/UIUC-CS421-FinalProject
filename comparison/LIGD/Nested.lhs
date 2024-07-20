> module Nested where
> import LIGD
> import PerfectDatatype (Perfect)
> import PerfectReps (rPerfect)
> import GEq (geq)
> --import Data.Generics hiding (Generic)
>
> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect = geq (rPerfect rInt)
