{-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving #-}

module TreeReps where


import Data.Generics hiding (Unit)
import TreeDatatype

{-
 -- Failed attempt at speeding up Uniplate,
 -- I got from ~11000 to ~10000 in the TestRmWeights test. 
{-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving -XTemplateHaskell -XUndecidableInstances #-}

import Data.Generics.Uniplate
import Data.Generics.PlateTypeable
import Data.DeriveTH
import Data.Derive.PlateTypeable

$(derive makePlateTypeable ''WTree)
-}

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
--
-- Note that this happens when compiling but not when interpreted
-- under ghci.

instance (Data t1, Data t2, Typeable t1, Typeable t2) =>
                 Data (WTree t1 t2) where
  gfoldl k r (Leaf x1) = k (r Leaf) x1
  gfoldl k r (Fork x1 x2) = k (k (r Fork) x1) x2
  gfoldl k r (WithWeight x1 x2) = k (k (r WithWeight) x1) x2
  gunfold    = error "Not implemented"
  toConstr   = error "Not implemented"
  dataTypeOf = error "Not implemented"


