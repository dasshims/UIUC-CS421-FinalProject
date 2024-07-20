module PolyLib.Fold (pfoldr, ffoldr, pfoldl, ffoldl, P_fmap2, P_ffoldl, P_ffoldr) where
import PolyLib.Prelude
import PolyLib.Base (cata, P_fmap2)
pfoldl :: (FunctorOf a b, P_fmap2 a, P_ffoldl a) => (c -> d -> c) -> c -> b d -> c
pfoldl f = flip $ (cata (ffoldl $ (flip f)))
class P_ffoldl a where
  ffoldl :: (b -> c -> c) -> a b (c -> c) -> c -> c
instance (P_ffoldl d, P_ffoldl e) => P_ffoldl (SumF d e) where
  ffoldl = \f -> foldSum (ffoldl f) (ffoldl f)
instance (P_ffoldl i, P_ffoldl h) => P_ffoldl (ProdF h i) where
  ffoldl = \f (x :*: y) -> (ffoldl f y) . (ffoldl f x)
instance P_ffoldl EmptyF where
  ffoldl = \f _ -> id
instance P_ffoldl ParF where
  ffoldl = \f (ParF a) -> f a
instance P_ffoldl RecF where
  ffoldl = \f (RecF b) -> b
instance P_ffoldl (ConstF r) where
  ffoldl = \f _ -> id
instance (FunctorOf functorOf_u u, P_fmap2 functorOf_u, P_ffoldl functorOf_u, P_ffoldl v) =>
	   P_ffoldl (CompF u v) where
  ffoldl = \f (CompF x) e -> pfoldl (flip $ (ffoldl f)) e x
pfoldr :: (FunctorOf a b, P_fmap2 a, P_ffoldr a) => (c -> d -> d) -> d -> b c -> d
pfoldr f = flip $ (cata (ffoldr f))
class P_ffoldr a where
  ffoldr :: (b -> c -> c) -> a b (c -> c) -> c -> c
instance (P_ffoldr d, P_ffoldr e) => P_ffoldr (SumF d e) where
  ffoldr = \f -> foldSum (ffoldr f) (ffoldr f)
instance (P_ffoldr h, P_ffoldr i) => P_ffoldr (ProdF h i) where
  ffoldr = \f (x :*: y) -> (ffoldr f x) . (ffoldr f y)
instance P_ffoldr EmptyF where
  ffoldr = \f _ -> id
instance P_ffoldr ParF where
  ffoldr = \f (ParF a) -> f a
instance P_ffoldr RecF where
  ffoldr = \f (RecF b) -> b
instance P_ffoldr (ConstF r) where
  ffoldr = \f _ -> id
instance (FunctorOf functorOf_u u, P_fmap2 functorOf_u, P_ffoldr functorOf_u, P_ffoldr v) =>
	   P_ffoldr (CompF u v) where
  ffoldr = \f (CompF x) e -> pfoldr (ffoldr f) e x
