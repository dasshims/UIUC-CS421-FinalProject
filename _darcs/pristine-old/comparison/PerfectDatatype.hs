{-# OPTIONS -fallow-undecidable-instances #-}

module PerfectDatatype where

data Perfect a = Zero a | Succ (Perfect (Fork a)) deriving (Show)
data Fork a = Fork a a deriving (Show)

perfect :: Perfect Int
perfect = Succ (Succ (Succ (Zero (Fork (Fork (Fork 2 3)
                                             (Fork 5 7))
                                       (Fork (Fork 11 13)
                                             (Fork 17 19))))))
