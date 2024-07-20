{-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving #-}
-- {-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving -XTemplateHaskell #-}

module TreeReps where

import Data.Generics hiding (Unit)
import Data.Generics.Basics

import TreeDatatype

-- import Data.DeriveTH
-- import Data.Derive.Data

-- The instances for Data and Typeable are no longer derived
-- with the datatype definition. Now they are derived separetely
-- using stand-alone deriving. Unfortunately, this will only work
-- well with GHC 6.8.2 . There are bugs in 6.8.1 that break stand-alone
-- deriving for Typeable and Data
--
-- Why do we do this? Because other libraries (such as SYB3) might
-- use typeclasses with the same name but with different functionality.

deriving instance Typeable2 WTree

-- deriving instance (Data a, Typeable a,Data b, Typeable b) => Data (WTree a b)
-- ghc-6.8.2 panics when compiling the stand-alone deriving instance

-- The following code was generated from 
-- $( derive makeData ''WTree )
-- and -ddump-splices + some hand editing
--   ghc -c -fglasgow-exts  -iSYB1_2 -ddump-splices SYB1_2/TreeReps.hs
--
-- To get the toConstr definitions, you enable the stand-alone instance above
-- with another GHC, and then slightly beautify the code produced by
--   -ddump-deriv

instance (Data t1, Data t2, Typeable t1, Typeable t2) =>
                 Data (WTree t1 t2) where
  gfoldl k r (Leaf x1) = k (r Leaf) x1
  gfoldl k r (Fork x1 x2) = k (k (r Fork) x1) x2
  gfoldl k r (WithWeight x1 x2) = k (k (r WithWeight) x1) x2
  gunfold k z c
    = case constrIndex c of
        1 -> k (z Leaf)
        2 -> k (k (z Fork))
        _ -> k (k (z WithWeight))
  toConstr (Leaf _)         = cLeaf
  toConstr (Fork _ _)       = cFork
  toConstr (WithWeight _ _) = cWithWeight
  dataTypeOf = error "Not implemented : dataTypeOf for WTree"

cWithWeight = mkConstr tWTree "WithWeight" [] Prefix
cLeaf = mkConstr tWTree "Leaf" [] Prefix
cFork = mkConstr tWTree "Fork" [] Prefix

tWTree = mkDataType "TreeDatatype.WTree" [cLeaf, cFork, cWithWeight]
