{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.MultiRec.Show
-- Copyright   :  (c) 2008--2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic show.
--
-----------------------------------------------------------------------------

module GShow where

import Generics.MultiRec.Base
import Generics.MultiRec.HFunctor
import Generics.MultiRec.Fold

import qualified Prelude as P
import Prelude hiding (show, showsPrec)

import CompanyReps hiding (C)

import GMapQ

-- * Generic show

class HFunctor f => HShow f where
  hShowsPrecAlg :: Algebra' s f (K0 [Int -> ShowS])

instance HShow (I xi) where
  hShowsPrecAlg _ (I (K0 x)) = K0 x

-- | For constant types, we make use of the standard
-- show function.
instance Show x => HShow (K x) where
  hShowsPrecAlg _ (K x) = K0 [\ n -> P.showsPrec n x]

instance HShow U where
  hShowsPrecAlg _ U = K0 []

instance (HShow f, HShow g) => HShow (f :+: g) where
  hShowsPrecAlg ix (L x) = hShowsPrecAlg ix x
  hShowsPrecAlg ix (R y) = hShowsPrecAlg ix y

instance (HShow f, HShow g) => HShow (f :*: g) where
  hShowsPrecAlg ix (x :*: y) = K0 (unK0 (hShowsPrecAlg ix x) ++ unK0 (hShowsPrecAlg ix y))

instance HShow f => HShow (f :>: ix) where
  hShowsPrecAlg ix (Tag x) = hShowsPrecAlg ix x

instance HShow f => HShow (C c f) where
  hShowsPrecAlg ix cx@(C x) =
    if null fields then K0 [ \ n -> (conName cx ++) ] else
    case conFixity cx of
      Prefix    -> K0 [\ n -> showParen (not (null fields) && n > 10)
                                        (spaces ((conName cx ++) : map ($ 11) fields))]
      Infix a p -> K0 [\ n -> showParen (n > p)
                                        (spaces (head fields p : (conName cx ++) : map ($ p) (tail fields)))]
   where
    fields = unK0 $ hShowsPrecAlg ix x

-- | A variant of the algebra that takes an extra argument
-- to fix the system 's' the algebra works on.
hShowsPrecAlg_ :: (HShow f) => s ix -> Algebra' s f (K0 [Int -> ShowS])
hShowsPrecAlg_ _ = hShowsPrecAlg 

showsPrec :: forall s ix. (Ix s ix, HShow (PF s)) => s ix -> Int -> ix -> ShowS
showsPrec ix n x = spaces (map ($ n) (unK0 (fold (hShowsPrecAlg_ ix) x)))

show :: forall s ix. (Ix s ix, HShow (PF s)) => s ix -> ix -> String
show ix x = showsPrec ix 11 {- force outer parentheses -} x ""

gshowsCompany = show Company

-- * Utilities

spaces :: [ShowS] -> ShowS
spaces []     = id
spaces [x]    = x
spaces (x:xs) = x . (' ':) . spaces xs

-- * Test for higher-orderness:

-- The bad thing with this approach is the poor support of polymorphic
-- datatypes. There is no reuse! We have to define a representation
-- for [Int] from scratch!

data ListInt :: * -> * where
  Int     :: ListInt Int
  ListInt :: ListInt [Int]

type instance PF ListInt =
  K Int     :>: Int    :+:
  NilT      :>: [Int]  :+:
  ConsT Int :>: [Int]

instance Ix ListInt Int where
  from_ i = L (Tag (K i))
  to_ (L (Tag (K i))) = i
  index = Int

instance Ix ListInt [Int] where
  from_ [] = R (L (Tag (C U)))
  from_ (x:xs) = R (R (Tag (C (I (I0 x) :*: I (I0 xs)))))
  to_ (R (L (Tag (C U)))) = []
  to_ (R (R (Tag (C (I (I0 x) :*: I (I0 xs)))))) = (x:xs)
  index = ListInt

-- The application of gmapQ to show fails because the argument of
-- gmapQ does not provide enough type class context
gapplyShowList :: [Int] -> [String]
-- gapplyShowList = gmapQ GShow.show
gapplyShowList = error "MultiRec does not support higher-order generic functions"


