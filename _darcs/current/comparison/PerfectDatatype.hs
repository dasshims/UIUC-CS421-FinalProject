--{-# OPTIONS_GHC -fallow-undecidable-instances -fglasgow-exts #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}


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
