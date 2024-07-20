{-# OPTIONS -fglasgow-exts -fgenerics -fallow-undecidable-instances #-}

module GRoseReps where

import GL
import GRoseDatatype
import Data.Generics hiding (Generic)

grose f a =  view isoGrose (a <*> f)

isoGrose = Iso fromGrose toGrose

fromGrose (GRose x trees)  =  x :*: trees
toGrose (x :*: trees)      = GRose x trees

instance (Generic g, GRep g a, GRep g (f (GRose f a))) => GRep g (GRose f a) where
  over = grose over over
