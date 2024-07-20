{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- A parameterised datatype for the nested data type

module PerfectDats where

import Syb4A

-- The latter not only defines the data type but makes instances, 
-- which we do not use here.
import PerfectDatatype

-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to 
-- derive it manually. 

-- data Perfect a = Zero a | Succ (Perfect (Fork a)) deriving (Show)
-- data Fork a = Fork a a deriving (Show)

-- Alas, for nested data types, things are less straightforward.
-- The following naive approach does not work, due to the way
-- GHC handles polymorphic recursion in the presence of constraints.
-- If we have a function foo :: a -> Int
-- then it will apply to (Fork a) as well, because 'a' is more generic
-- than (Fork a). However, if we have a function
-- foo' :: C a => a -> Int
-- then it is not clear that the function will apply to (Fork a).
-- It is not certain that (C a) implies (C (Fork a)). GHC will try to
-- ascertain this implication by unrolling, which obviously never ends.

-- The source of our difficulties is because the commented out gapp
-- is too general. It would have permitted the user to reduce
-- a value (Perfect Int) and let him override not only Int but also
-- (Fork Int), (Fork (Fork Int)), etc. Obviously, this generality
-- is obviously not attainable with the nested data type -- and probably
-- not needed anyway.

{-
instance (GAPP (TL_red w) spec a w w)
    => LDat (TL_red w) spec (Fork a) w where
  gin tlab@(TL_red f) spec (Fork x y)  = f [gapp tlab spec x,
					    gapp tlab spec y]

instance (LDat (TL_red w) spec a w, STApply spec a w w,
	  STApply spec (Fork a) w w,
	  STApply spec (Perfect b) w w)
    => LDat (TL_red w) spec (Perfect a) w where
  gin tlab@(TL_red f) spec (Zero a)  = f [gapp tlab spec a]
  gin tlab@(TL_red f) spec (Succ b)  = f [gapp tlab spec b]
-}

-- So, we use a simpler solution. It shows, incidentally, that our
-- generic function gapp is first-class. It can be passed as an argument,
-- with no need for higher-rank types.
-- In the following, pr :: (a ->w) is the evidence of gapp. There are
-- no constraints!

redh :: TL_red w -> (a -> w) -> Perfect a -> w
redh tlab@(TL_red f) pr (Zero a) = f [pr a]
redh tlab@(TL_red f) pr (Succ b) = 
    f [redh tlab (\ (Fork x y) -> f [pr x,pr y]) b ]


instance (GAPP (TL_red w) spec a w w)
    => LDat (TL_red w) spec (Perfect a) w where
  gin tlab@(TL_red f) spec x  = redh tlab (gapp tlab spec) x


redlh :: TL_red_lockstep w -> (Couple a -> w) -> (Couple (Perfect a)) -> w
redlh tlab@(TL_red_lockstep _ f) pr (Couple (Zero a) (Zero b)) = 
    f [pr (Couple a b)]
redlh tlab@(TL_red_lockstep _ f) pr (Couple (Succ a) (Succ b)) = 
    f [redlh tlab pr' (Couple a b)]
  where 
  pr' (Couple (Fork xa ya) (Fork xb yb)) = f [pr (Couple xa xb),
					      pr (Couple ya yb)]
redlh tlab@(TL_red_lockstep d f) pr _ = d

instance (GAPP (TL_red_lockstep w) spec (Couple a) w w)
    => LDat (TL_red_lockstep w) spec (Couple (Perfect a)) w where
  gin tlab@(TL_red_lockstep _ f) spec x  = redlh tlab (gapp tlab spec) x
