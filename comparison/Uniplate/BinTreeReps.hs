{-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving #-}

module BinTreeReps where


import Data.Generics hiding (Unit)
import BinTreeDatatype

-- The instances for Data and Typeable are no longer derived
-- with the datatype definition. Now they are derived separetely
-- using stand-alone deriving. Unfortunately, this will only work
-- well with GHC 6.8.2 . There are bugs in 6.8.1 that break stand-alone
-- deriving for Typeable and Data
--
-- Why do we do this? Because other libraries (such as SYB3) might
-- use typeclasses with the same name but with different functionality.

deriving instance Typeable1 BinTree

-- Panic for ghc-6.8.2
-- deriving instance (Data a, Typeable a) => Data (BinTree 

instance (Data t, Typeable t) => Data (BinTree t) where
  gfoldl k r (Leaf x1)   = k (r Leaf) x1
  gfoldl k r (Bin x1 x2) = k (k (r Bin) x1) x2
  toConstr (Leaf _)  = con_Leaf
  toConstr (Bin _ _) = con_Bin
  gunfold    = error "Not implemented"
  dataTypeOf = error "Not implemented"

con_Leaf   = mkConstr ty_BinTree "Leaf" [] Prefix
con_Bin    = mkConstr ty_BinTree "Bin"  [] Prefix
ty_BinTree = mkDataType "BinTree" [con_Leaf, con_Bin]


