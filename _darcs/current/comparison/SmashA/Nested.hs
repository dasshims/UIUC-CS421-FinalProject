{-# OPTIONS -fglasgow-exts  #-}

-- Tree folding over nested data type and teh comparison

module Nested (collectPerfect, equalPerfect) where

import PerfectDatatype
import PerfectDats

import Syb4A

-- collect all leaf values in a list

-- Inferred signature
collectPerfect1 t = gapp (TL_red concat) ((\ (x::Int) _ ->[x]) :+: HNil) t
test1 = collectPerfect1 example

example :: Perfect Int
example = Succ (Succ (Succ (Zero (Fork (Fork (Fork 2 3)
                                             (Fork 5 7))
                                 (Fork (Fork 11 13)
                                             (Fork 17 19))))))

-- Now, try to really reproduce desired signature for collectPerfect
-- Handling of local type variables in GHC 6.6 makes things unnecessarily
-- difficult. With GHC 6.4, the unP workaround would not be needed.

collectPerfect_sig :: Perfect a -> [a]
collectPerfect_sig = undefined

unP :: Perfect a -> a; unP = undefined

collectPerfect t | False = collectPerfect_sig t
collectPerfect t = 
    gapp (TL_red concat) ((\x _ ->[x `asTypeOf` (unP t)]) :+: HNil) t

test = collectPerfect example

equalPerfect t = geq t

main = print ( equalPerfect perfect perfect
             , equalPerfect perfect perfect2
             )
