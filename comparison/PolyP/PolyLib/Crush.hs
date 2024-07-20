{-# OPTIONS_GHC -fallow-undecidable-instances #-}
module PolyLib.Crush (crush, fcrush, P_fmap2, P_fcrush) where
import PolyLib.Prelude
import PolyLib.Base (cata, pmap, P_fmap2)
crush :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => (c -> c -> c) -> c -> b c -> c
crush op e = cata (fcrush op e)
class P_fcrush a where
  fcrush :: (b -> b -> b) -> b -> a b b -> b
instance (P_fcrush c, P_fcrush d) => P_fcrush (SumF c d) where
  fcrush = \op e -> foldSum (fcrush op e) (fcrush op e)
instance (P_fcrush f, P_fcrush g) => P_fcrush (ProdF f g) where
  fcrush = \op e (x :*: y) -> op (fcrush op e x) (fcrush op e y)
instance P_fcrush EmptyF where
  fcrush = \op e x -> e
instance P_fcrush ParF where
  fcrush = \op e -> unParF
instance P_fcrush RecF where
  fcrush = \op e -> unRecF
instance (FunctorOf functorOf_l l, P_fmap2 functorOf_l, P_fcrush functorOf_l, P_fcrush m) =>
	   P_fcrush (CompF l m) where
  fcrush = \op e -> (crush op e) . ((pmap (fcrush op e)) . unCompF)
instance P_fcrush (ConstF p) where
  fcrush = \op e x -> e
