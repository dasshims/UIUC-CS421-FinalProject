> module Nested where
> import LIGD
> import PerfectDatatype
> import PerfectReps
> import Reduce
> --import Data.Generics hiding (Generic)
>
> collectPerfect :: Perfect a -> [a]
> collectPerfect = collectG rPerfect2
