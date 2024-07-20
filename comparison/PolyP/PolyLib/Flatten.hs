{-# OPTIONS_GHC -fallow-undecidable-instances #-}
module PolyLib.Flatten (flatten,
			fflatten,
			fl_par,
			fl_rec,
			fl_all,
			singleton,
			nil,
			substructures,
			P_fflatten,
			P_fmap2) where
import PolyLib.Prelude
import PolyLib.Base (pmap, fmap2, P_fmap2)
nil :: a -> [b]
nil x = []
singleton :: a -> [a]
singleton x = x : []
flatten :: (FunctorOf a b, P_fflatten a, P_fmap2 a) => b c -> [c]
flatten = fflatten . ((fmap2 singleton flatten) . out)
class P_fflatten a where
  fflatten :: a [b] [b] -> [b]
instance (P_fflatten c, P_fflatten d) => P_fflatten (SumF c d) where
  fflatten = foldSum fflatten fflatten
instance (P_fflatten f, P_fflatten g) => P_fflatten (ProdF f g) where
  fflatten = \(x :*: y) -> (fflatten x) ++ (fflatten y)
instance P_fflatten EmptyF where
  fflatten = nil
instance P_fflatten ParF where
  fflatten = unParF
instance P_fflatten RecF where
  fflatten = unRecF
instance (FunctorOf functorOf_l l, P_fflatten functorOf_l, P_fmap2 functorOf_l, P_fflatten m) =>
	   P_fflatten (CompF l m) where
  fflatten = concat . (flatten . ((pmap fflatten) . unCompF))
instance P_fflatten (ConstF p) where
  fflatten = nil
fl_par :: (P_fflatten a, P_fmap2 a) => a b c -> [b]
fl_par = fflatten . (fmap2 singleton nil)
fl_rec :: (P_fflatten a, P_fmap2 a) => a b c -> [c]
fl_rec = fflatten . (fmap2 nil singleton)
fl_all :: (P_fflatten a, P_fmap2 a) => a b b -> [b]
fl_all = fflatten . (fmap2 singleton singleton)
substructures :: (FunctorOf a b, P_fflatten a, P_fmap2 a) => b c -> [b c]
substructures x = x : (fflatten (fmap2 nil substructures (out x)))
