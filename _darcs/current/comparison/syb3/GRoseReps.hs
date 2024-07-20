{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}
module GRoseReps where
import GRoseDatatype
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Data.Generics.SYB.WithClass.Instances
import Data.Typeable
import Language.Haskell.TH()

-- $(derive [''GRose])
-- Fails - the deriving does not work for higher kind parameters

instance (Typeable1 f,Typeable a) => Typeable (GRose f a) where
    typeOf _ = mkTyConApp (mkTyCon "GRose") [typeOf1 (undefined::f (GRose f a)),typeOf (undefined::a)]

instance (Typeable1 f,Typeable a,Data ctx a,Data ctx (f (GRose f a))
         ,Sat (ctx (GRose f a)))
  => Data ctx (GRose f a) where
    gfoldl _ ap ret (GRose x xs) = ret GRose `ap` x `ap` xs
    gunfold = undefined
    toConstr _ (GRose _ _) = groseCon
    dataTypeOf = undefined

groseDataType = mkDataType "GRose" [groseCon]
groseCon = mkConstr groseDataType "GRose" []{- no fields-} Prefix

