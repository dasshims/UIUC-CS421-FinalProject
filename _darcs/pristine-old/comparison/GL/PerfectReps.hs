{-# OPTIONS -fallow-undecidable-instances #-}

module PerfectReps where

import GL2
import PerfectDatatype
import Data.Generics hiding (Generic)

perfecttree :: (Generic g) => g a1 a2 -> g (Perfect a1) (Perfect a2)
perfecttree a =  view isoPerfectTree isoPerfectTree (a <|> perfecttree (fork a))

isoPerfectTree = Iso fromPerfectTree toPerfectTree

fromPerfectTree (Zero x)              =  Inl x
fromPerfectTree (Succ p)              =  Inr p

toPerfectTree (Inl x)                 =  Zero x
toPerfectTree (Inr p)                 =  Succ p

instance FunctorRep Perfect where
   functorRep   =  perfecttree


fork :: (Generic g) => g a1 a2 -> g (Fork a1) (Fork a2)
fork a = view isoFork isoFork (a <*> a)

isoFork = Iso fromFork toFork

fromFork (Fork x y)      =  x :*: y
toFork (x :*: y)         =  Fork x y

instance FunctorRep Fork where
   functorRep   =  fork