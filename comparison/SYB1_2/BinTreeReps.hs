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
deriving instance (Data a, Typeable a) => Data (BinTree a)

