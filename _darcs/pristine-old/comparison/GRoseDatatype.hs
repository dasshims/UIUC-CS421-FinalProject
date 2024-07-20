{-# OPTIONS  -fallow-undecidable-instances #-}

module GRoseDatatype where
import Data.Generics
import Data.Typeable

data GRose f a = GRose a (f (GRose f a))
--                 deriving (Typeable,Data)

{-

Cannot derive for this datatype because of the higher kinded type
argument 'f'!

see comment in GRoseInstances.hs

-}
