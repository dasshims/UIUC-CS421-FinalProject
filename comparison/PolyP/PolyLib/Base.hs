{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
module PolyLib.Base (pmap, fmap2, cata, ana, hylo, para, (-*-), (-+-), P_fmap2) where
import PolyLib.Prelude
pmap :: (FunctorOf a b, P_fmap2 a) => (c -> d) -> b c -> b d
pmap f = inn . ((fmap2 f (pmap f)) . out)
class P_fmap2 a where
  fmap2 :: (b -> c) -> (d -> e) -> a b d -> a c e
instance (P_fmap2 f, P_fmap2 g) => P_fmap2 (SumF f g) where
  fmap2 = \p r -> (fmap2 p r) -+- (fmap2 p r)
instance (P_fmap2 l, P_fmap2 m) => P_fmap2 (ProdF l m) where
  fmap2 = \p r -> (fmap2 p r) -*- (fmap2 p r)
instance P_fmap2 EmptyF where
  fmap2 = \p r EmptyF -> EmptyF
instance P_fmap2 ParF where
  fmap2 = \p r -> ParF . (p . unParF)
instance P_fmap2 RecF where
  fmap2 = \p r -> RecF . (r . unRecF)
instance (FunctorOf functorOf_ad ad, P_fmap2 functorOf_ad, P_fmap2 ae) =>
	   P_fmap2 (CompF ad ae) where
  fmap2 = \p r -> CompF . ((pmap (fmap2 p r)) . unCompF)
instance P_fmap2 (ConstF aj) where
  fmap2 = \p r -> ConstF . unConstF
cata :: (FunctorOf a b, P_fmap2 a) => (a c d -> d) -> b c -> d
cata i = i . ((fmap2 id (cata i)) . out)
ana :: (FunctorOf a b, P_fmap2 a) => (c -> a d c) -> c -> b d
ana o = inn . ((fmap2 id (ana o)) . o)
hylo :: P_fmap2 a => (a b c -> c) -> (d -> a b d) -> d -> c
hylo i o = i . ((fmap2 id (hylo i o)) . o)
para :: (FunctorOf a b, P_fmap2 a) => (b c -> a c d -> d) -> b c -> d
para i x = i x (fmap2 id (para i) (out x))
