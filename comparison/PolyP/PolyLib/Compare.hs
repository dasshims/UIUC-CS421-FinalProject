module PolyLib.Compare (pcompare', pcompare, fcompare, P_fcompare) where
import PolyLib.Prelude
ordop :: Ordering -> Ordering -> Ordering
ordop x y = case x of
	      EQ -> y
	      _ -> x
prodcompare ::
  (a b c -> d e f -> Ordering) ->
  (g b c -> h e f -> Ordering) -> ProdF a g b c -> ProdF d h e f -> Ordering
prodcompare f g (p :*: p') (q :*: q') = ordop (f p q) (g p' q')
sumcompare ::
  (a b c -> d e f -> Ordering) ->
  (g b c -> h e f -> Ordering) -> SumF a g b c -> SumF d h e f -> Ordering
sumcompare f g a b = case (a, b) of
		       (InL x, InL v) -> f x v
		       (InR y, InR w) -> g y w
		       (InL _, InR _) -> LT
		       (InR _, InL _) -> GT
c21 :: (a -> a -> b) -> (c -> a) -> c -> c -> b
c21 f g x y = f (g x) (g y)
pcompare :: (FunctorOf a b, P_fcompare a) => (c -> c -> Ordering) -> b c -> b c -> Ordering
pcompare eq x y = fcompare eq (pcompare eq) (out x) (out y)
class P_fcompare a where
  fcompare :: (b -> b -> Ordering) -> (c -> c -> Ordering) -> a b c -> a b c -> Ordering
instance (P_fcompare d, P_fcompare e) => P_fcompare (SumF d e) where
  fcompare = \p r -> sumcompare (fcompare p r) (fcompare p r)
instance (P_fcompare h, P_fcompare i) => P_fcompare (ProdF h i) where
  fcompare = \p r -> prodcompare (fcompare p r) (fcompare p r)
instance P_fcompare EmptyF where
  fcompare = \p r _ _ -> EQ
instance P_fcompare ParF where
  fcompare = \p r -> c21 p unParF
instance P_fcompare RecF where
  fcompare = \p r -> c21 r unRecF
instance (FunctorOf functorOf_r r, P_fcompare functorOf_r, P_fcompare s) =>
	   P_fcompare (CompF r s) where
  fcompare = \p r -> c21 (pcompare (fcompare p r)) unCompF
instance Ord v => P_fcompare (ConstF v) where
  fcompare = \p r -> c21 compare unConstF
pcompare' :: (FunctorOf a b, P_fcompare a, Ord c) => b c -> b c -> Ordering
pcompare' = pcompare compare
