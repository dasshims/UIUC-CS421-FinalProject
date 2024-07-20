> module Nested where

> import NOW hiding (Perfect, perfect)
> import PerfectDatatype
> import Reduce

> collectPerfect :: Perfect a -> [a]
> collectPerfect = pcollect (\ a -> PPerfectR a)
