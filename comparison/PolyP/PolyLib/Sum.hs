module PolyLib.Sum (psum, fsum, size, P_fmap2, P_fsum) where
import PolyLib.Prelude
import PolyLib.Base (cata, pmap, P_fmap2)
psum :: (FunctorOf a b, P_fmap2 a, Num c, P_fsum a) => b c -> c
psum = cata fsum
class P_fsum a where
  fsum :: Num b => a b b -> b
instance (P_fsum c, P_fsum d) => P_fsum (SumF c d) where
  fsum = foldSum fsum fsum
instance (P_fsum f, P_fsum g) => P_fsum (ProdF f g) where
  fsum = \(x :*: y) -> (fsum x) + (fsum y)
instance P_fsum EmptyF where
  fsum = \x -> fromIntegral 0
instance P_fsum ParF where
  fsum = unParF
instance P_fsum RecF where
  fsum = unRecF
instance (FunctorOf functorOf_l l, P_fmap2 functorOf_l, P_fsum functorOf_l, P_fsum m) =>
	   P_fsum (CompF l m) where
  fsum = psum . ((pmap fsum) . unCompF)
instance P_fsum (ConstF p) where
  fsum = \x -> fromIntegral 0
size :: (FunctorOf a b, P_fmap2 a, P_fsum a) => b c -> Int
size = psum . (pmap (\_ -> 1))
