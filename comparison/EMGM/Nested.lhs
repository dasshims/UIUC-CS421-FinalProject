 {-# OPTIONS_GHC -fglasgow-exts #-}

> module Nested where

> import GL
> import PerfectDatatype
> import PerfectReps
> import GEq

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect = geq' over

