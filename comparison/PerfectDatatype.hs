{-# OPTIONS_GHC -fallow-undecidable-instances -fglasgow-exts #-}

module PerfectDatatype where

import Data.Generics

data Perfect a = Zero a | Succ (Perfect (Fork a)) deriving Show
data Fork a = Fork a a deriving Show

perfect :: Perfect Int
perfect = Succ (Succ (Succ (Zero (Fork (Fork (Fork 2 3)
                                             (Fork 5 7))
                                       (Fork (Fork 11 13)
                                             (Fork 17 19))))))
perfect2 :: Perfect Int
perfect2 = Succ (Zero (Fork 1 2))
