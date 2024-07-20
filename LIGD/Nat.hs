-- Some Peano arithmetic encided using types and rep's.

module Nat(Z, S, rZero, rSucc,  int2rep)
where
import EquivRep

data Z = Zero
data S a = Succ a

data NatRep n = RZero (E n Z)
              | forall m. RSucc (NatRep m) (E n (S m))


instance Congruence NatRep where
  cong ep = E{t = \x -> case x of RZero epZ -> RZero (trans (symm ep) epZ);
                                  RSucc nr epS -> RSucc nr (trans (symm ep) epS),
            f = \x -> case x of RZero epZ -> RZero (trans ep epZ);
                                  RSucc nr epS -> RSucc nr (trans ep epS)}
             

rZero :: NatRep Z
rZero = RZero refl

rSucc :: NatRep a -> NatRep (S a)
rSucc nr = RSucc nr refl

data DynamicNat = forall n. DynNat Int (NatRep n)

int2rep :: Int -> DynamicNat
int2rep 0 = DynNat 0 rZero 
int2rep (n+1) = case int2rep n of 
                  DynNat n r -> DynNat (n+1) (rSucc r)
rep2int :: DynamicNat -> Int
rep2int (DynNat n _) = n

is_zero :: NatRep a -> [E a Z]
is_zero (RZero ep) = [ep]
is_zero (RSucc _ _) = []


nat_ind :: Congruence f => (f Z) -> (forall a. (f a -> f (S a))) -> (forall b. NatRep b -> f b)
nat_ind fZ fS (RZero ep) = f (cong ep) fZ
nat_ind fZ fS (RSucc nr ep) = f (cong ep) (fS (nat_ind fZ fS nr))


data Plus a b = Plus a b
instance Congruence (Plus a) where
  cong ep = E{t = \(Plus x y) -> Plus x (t ep y),
              f = \(Plus x y) -> Plus x (f ep y)}




plus_0 :: E (Plus Z b) b
plus_0 = E{t = \p -> case p of Plus x y -> y,
           f = \y -> Plus Zero y}

plus_S :: E (Plus a b) c -> E (Plus (S a) b) (S c)
plus_S ep = E{t = \x -> case x of Plus (Succ x) y -> Succ (t ep (Plus x y)),
              f = \x -> case x of 
                          Succ z -> case (f ep z) of 
                            Plus a b -> Plus (Succ a) b}

newtype Compose f g a = Comp{unComp ::f (g a)}

instance (Congruence f , Congruence g) => Congruence (Compose f g) where
  cong ep = E{t = \fga -> Comp (t (cong (cong ep)) (unComp fga)),
              f = \fgb -> Comp (f (cong (cong ep)) (unComp fgb))}



