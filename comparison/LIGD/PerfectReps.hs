module PerfectReps where
import LIGD
import PerfectDatatype

rPerfect :: Rep a -> Rep (Perfect a)
rFork    :: Rep a -> Rep (Fork a)

rPerfect ra = RType (App "Perfect" [term ra])
                    (rSum (RCon "Zero" ra)
                          (RCon "Succ" (rPerfect (rFork ra))))
                    (EP fromPerfect toPerfect)

rFork ra = RType (App "Fork" [term ra])
                 (RCon "Fork" (rPair ra ra))
                 (EP fromFork toFork)

fromPerfect :: Perfect a -> Sum a (Perfect (Fork a))
fromPerfect (Zero a) = (Inl a)
fromPerfect (Succ t) = (Inr t)
toPerfect :: Sum a (Perfect (Fork a)) -> Perfect a
toPerfect   (Inl a)  = (Zero a)
toPerfect   (Inr t)  = (Succ t)

fromFork :: Fork a   -> Pair a a
fromFork (Fork x y)  =  (x :*: y)
toFork   :: Pair a a -> Fork a
toFork   (x :*: y)   =  (Fork x y)

-- --------------------------------------------------------------
{-
-- Type signature required due to polymorphic recursion
rPerfect2 :: Rep2 f a b c -> Rep2 f (Perfect a) (Perfect b) c
rPerfect2 ra = RType2 (App "Perfect" [term2 ra])
                      (rSum2 (RCon2 "Zero" ra)
                             (RCon2 "Succ" (rPerfect2 (rFork2 ra))))
                      (EP fromPerfect toPerfect)
                      (EP fromPerfect toPerfect)

-- This signature is not required
rFork2 :: Rep2 f a b c -> Rep2 f (Fork a) (Fork b) c
rFork2 ra = RType2 (App "Fork" [term2 ra])
                   (RCon2 "Fork" (rPair2 ra ra))
                   (EP fromFork toFork)
                   (EP fromFork toFork)
-}