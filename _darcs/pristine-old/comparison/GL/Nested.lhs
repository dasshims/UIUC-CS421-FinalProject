> module Nested where

> import GL2
> import PerfectDatatype
> import PerfectReps
> import Reduce
> --import Data.Generics hiding (Generic)
>
> collectPerfect :: Perfect a -> [a]
> collectPerfect = collectG perfecttree
