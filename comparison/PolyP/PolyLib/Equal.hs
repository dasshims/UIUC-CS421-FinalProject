{-# OPTIONS_GHC -fglasgow-exts  -fallow-undecidable-instances #-}

module PolyLib.Equal (pequal, fequal, peq, P_fequal) where
import PolyLib.Prelude
prodequal ::
  (a b c -> d e f -> Bool) -> (g b c -> h e f -> Bool) -> ProdF a g b c -> ProdF d h e f -> Bool
prodequal f g (p :*: p') (q :*: q') = (f p q) && (g p' q')
sumequal ::
  (a b c -> d e f -> Bool) -> (g b c -> h e f -> Bool) -> SumF a g b c -> SumF d h e f -> Bool
sumequal f g a b = case (a, b) of
		     (InL x, InL v) -> f x v
		     (InR y, InR w) -> g y w
		     _ -> False
pequal :: (FunctorOf a b, P_fequal a) => (c -> d -> Bool) -> b c -> b d -> Bool
pequal eq x y = fequal eq (pequal eq) (out x) (out y)
class P_fequal a where
  fequal :: (b -> c -> Bool) -> (d -> e -> Bool) -> a b d -> a c e -> Bool
instance (P_fequal f, P_fequal g) => P_fequal (SumF f g) where
  fequal = \p r -> sumequal (fequal p r) (fequal p r)
instance (P_fequal l, P_fequal m) => P_fequal (ProdF l m) where
  fequal = \p r -> prodequal (fequal p r) (fequal p r)
instance P_fequal EmptyF where
  fequal = \p r _ _ -> True
instance P_fequal ParF where
  fequal = \p r x y -> p (unParF x) (unParF y)
instance P_fequal RecF where
  fequal = \p r x y -> r (unRecF x) (unRecF y)
instance (FunctorOf functorOf_ad ad, P_fequal functorOf_ad, P_fequal ae) =>
	   P_fequal (CompF ad ae) where
  fequal = \p r x y -> pequal (fequal p r) (unCompF x) (unCompF y)
instance Eq aj => P_fequal (ConstF aj) where
  fequal = \p r x y -> (unConstF x) == (unConstF y)
peq :: (FunctorOf a b, P_fequal a, Eq c) => b c -> b c -> Bool
peq = pequal (==)
