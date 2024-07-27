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


module NGRoseReps where

import GL
import NGRoseDatatype
import Data.Generics hiding (Generic)

-- The structure representation is different from grose. Here we take a
-- function argument, but in GRose it is not the case. Because in GRose the
-- argument has type |g a -> g (f (GRose f a)) -> g (GRose f a)|, that is,
-- an argument per constructor argument. This makes it possible to use
-- implicit instances by means of |over|.

ngrose :: Generic g => (forall a.g a -> g (f a)) -> g a -> g (NGRose f a)
ngrose f a =  view isoNGRose (a <*> f (ngrose (comp f f) a))

isoNGRose = Iso fromNGRose toNGRose

fromNGRose (NGRose x trees)  =  x :*: trees
toNGRose (x :*: trees)      = NGRose x trees

-- It is not easy to do a GRep instance for NGRose.
-- Why? Because |over| necessarily uses ngrose in the rhs
-- but ngrose needs as an argument an encoding of *->* types.
-- However we cannot use |over| in the right hand side to produce an argument
-- because |over| can only produce representations for * datatypes.
--
-- instance (Generic g, GRep g a, GRep g (f (NGRose (Comp f f) a))) => GRep g (NGRose f a) where
--   over = ngrose over{-bogus-} over

-- An alternative is to write the following:
--
-- instance (Generic g, GRep g a, GRep g (f (NGRose (Comp f f) a))) => GRep g (NGRose f a) where
--   over = view isoNGRose (over <*> over)
--
-- But this causes the compiler to loop because the context grows when trying
-- to type check this

fromComp (Comp x) = x
toComp x = Comp x

comp f g a = view (Iso fromComp toComp) (f (g a))

-- instance (Generic g, GRep g (f (h a))) => GRep g (Comp f h a) where
--   over = view (Iso fromComp toComp) over


