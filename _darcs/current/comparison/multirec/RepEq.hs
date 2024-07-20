module RepEq where

import Generics.MultiRec

infix 4 :=:

data (:=:) :: * -> * -> * where
    Refl :: a :=: a

class Eq_ s where
  eq_ :: s ix -> s ix' -> Maybe (ix :=: ix')

