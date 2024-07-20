> module Nested where
> import PerfectDatatype
> import Reduce
> --import Data.Generics hiding (Generic)
>
> collectPerfect :: Perfect a -> [a]
> collectPerfect = error "PolyP cannot handle nested datatypes"

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect = error "PolyP cannot handle nested datatypes"