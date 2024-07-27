--{-# OPTIONS -fglasgow-exts -fgenerics -fallow-undecidable-instances #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}


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
