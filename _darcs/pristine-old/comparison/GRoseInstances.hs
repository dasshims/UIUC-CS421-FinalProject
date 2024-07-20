{-# OPTIONS  -fallow-undecidable-instances #-}

module GRoseInstances where

import Data.Generics
import Data.Typeable

import GRoseDatatype

{-

Cannot derive for this datatype because of the higher kinded type
argument 'f'!

SYB is not "fully reflexive" in the sense of "Comparing Approaches to
Generic Programming in Haskell" (see the evaluation of SYB in section
4.3 of the the paper).

The comment above assumes that all datatypes have to have derivable
instances. However we can define the instances explicitly. The other
approaches impose a similar requirement, the programmer must supply the
representation. Of course, these instances are more difficult to write,
and I haven't even filled in all the methods!

-}

instance (Typeable1 f,Typeable a) => Typeable (GRose f a) where
    typeOf _ = mkTyConApp (mkTyCon "GRose") [typeOf1 (undefined::f (GRose f a)),typeOf (undefined::a)]

instance (Typeable1 f,Typeable a,Data a,Data (f (GRose f a))) => Data (GRose f a) where
    gfoldl ap ret (GRose x xs) = ret GRose `ap` x `ap` xs
    gunfold = undefined
    toConstr (GRose _ _) = groseCon
    dataTypeOf = undefined

groseDataType = mkDataType "GRose" [groseCon] 
groseCon = mkConstr groseDataType "GRose" []{- no fields-} Prefix