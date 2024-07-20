{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}

-- Lazy TL_recon strategy
-- Generic minimum and maximum -- finding the smallest and the
-- largest terms of a given type -- by traversing undefined.

module Syb4ABuild where

import Syb4A

newtype TL_recon_lazy = TL_recon_lazy (forall a. [a] -> a)

instance LDat TL_recon_lazy spec Int Int where
    gin _ spec x = x
instance LDat TL_recon_lazy spec Char Char where
    gin _ spec x = x
instance LDat TL_recon_lazy spec Bool Bool where
    gin _ spec x = x

instance (GAPP TL_recon_lazy spec a dfa wa, 
	  GAPP TL_recon_lazy spec b dfb wb)
    => LDat TL_recon_lazy spec (a,b) (wa,wb) where
    gin tlab@(TL_recon_lazy f) spec ~(x,y) =
	f [(gapp tlab spec x, gapp tlab spec y)]

-- (semi-)sums
instance (GAPP TL_recon_lazy spec a df w)
    => LDat TL_recon_lazy spec (Maybe a) (Maybe w) where
    gin tlab@(TL_recon_lazy f) spec v =
	f [Nothing,
	   let ~(Just x) = v in Just (gapp tlab spec x)
	  ]

instance (GAPP TL_recon_lazy spec [a] [w] [w], 
	  GAPP TL_recon_lazy spec a dfa w)
    => LDat TL_recon_lazy spec [a] [w] where
    gin tlab@(TL_recon_lazy f) spec x =
	f [[],
	   (gapp tlab spec (head x)):(gapp tlab spec (tail x))]

-- true sums
instance (GAPP TL_recon_lazy spec a dfa wa, 
	  GAPP TL_recon_lazy spec b dfb wb)
    => LDat TL_recon_lazy spec (Either a b) (Either wa wb) where
    gin tlab@(TL_recon_lazy f) spec v =
	f [Left  $ gapp tlab spec (either id undefined v), 
	   Right $ gapp tlab spec (either undefined id v)]


__ = error "nonexistent"
gminimum () = r 
  where r = gapp (TL_recon_lazy head) lst (__ `asTypeOf` r)
        lst = (mb (__::Int)) :+: (mb (__::Bool)) :+: (mb (__::Char))
	      -- default behavior for pairs suffices
              -- for Maybe: return Nothing
              -- for Either a b: return Left (gminimum :: a)
              -- for arrays: []
	      :+: HNil
        mb :: Bounded t => t -> t -> t
	mb _ _ = minBound

gmaximum () = r 
  where r = gapp (TL_recon_lazy last) lst (__ `asTypeOf` r)
        lst = (mb (__::Int)) :+: (mb (__::Bool)) :+: (mb (__::Char))
	      -- default behavior for pairs suffices
              -- for Maybe: return Nothing
              -- for Either a b: return Left (gminimum :: a)
              -- for arrays: []
	      :+: HNil
        mb :: Bounded t => t -> t -> t
	mb _ _ = maxBound

test_gmin1 = gminimum () :: (Int,Bool)
test_gmin2 = gminimum () :: (Maybe Int,Either Bool (Maybe Char))
test_gmax2 = gmaximum () :: (Maybe Int,Either Bool (Maybe Char))

test_gmin3 = gminimum () :: (Maybe Int,[Bool])

test_gmax1 = take 5 $ gmaximum () :: [Bool]
