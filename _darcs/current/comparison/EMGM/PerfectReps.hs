{-# OPTIONS_GHC -fglasgow-exts #-}

module PerfectReps where

import GL hiding (Fork)
import PerfectDatatype
import Data.Generics hiding (Generic)

perfecttree :: (Generic g) => g a -> g (Perfect a)
perfecttree a =  view isoPerfectTree (a <|> perfecttree (fork a))

isoPerfectTree = Iso fromPerfectTree toPerfectTree

fromPerfectTree (Zero x)              =  Inl x
fromPerfectTree (Succ p)              =  Inr p

toPerfectTree (Inl x)                 =  Zero x
toPerfectTree (Inr p)                 =  Succ p

instance FunctorRep Perfect where
   functorRep   =  perfecttree

instance (Generic g, GRep g a) => GRep g (Perfect a) where
  over = perfecttree over


fork :: (Generic g) => g a -> g (Fork a)
fork a = view isoFork (a <*> a)

isoFork = Iso fromFork toFork

fromFork (Fork x y)      =  x :*: y
toFork (x :*: y)         =  Fork x y

instance FunctorRep Fork where
   functorRep   =  fork

instance (Generic g, GRep g a) => GRep g (Fork a) where
  over = fork over
