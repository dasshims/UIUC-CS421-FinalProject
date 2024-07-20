module PolyLib.Propagate (propagate, fprop, sumprop, prodprop, P_fmap2, P_fprop) where
import PolyLib.Prelude
import PolyLib.Base (cata, pmap, P_fmap2)
prodprop :: (Maybe (a b c), Maybe (d b c)) -> Maybe (ProdF a d b c)
prodprop p = case p of
	       (Just x, Just y) -> Just (x :*: y)
	       _ -> Nothing
sumprop :: Functor a => Either (a (b c d)) (a (e c d)) -> a (SumF b e c d)
sumprop = either (fmap InL) (fmap InR)
propagate :: (FunctorOf a b, P_fmap2 a, P_fprop a) => b (Maybe c) -> Maybe (b c)
propagate = cata ((fmap inn) . fprop)
class P_fprop a where
  fprop :: a (Maybe b) (Maybe c) -> Maybe (a b c)
instance (P_fprop d, P_fprop e) => P_fprop (SumF d e) where
  fprop = sumprop . (fprop -++- fprop)
instance (P_fprop h, P_fprop i) => P_fprop (ProdF h i) where
  fprop = prodprop . (fprop -**- fprop)
instance P_fprop EmptyF where
  fprop = \EmptyF -> Just EmptyF
instance P_fprop ParF where
  fprop = (fmap ParF) . unParF
instance P_fprop RecF where
  fprop = (fmap RecF) . unRecF
instance (FunctorOf functorOf_r r, P_fmap2 functorOf_r, P_fprop functorOf_r, P_fprop s) =>
	   P_fprop (CompF r s) where
  fprop = (fmap CompF) . (propagate . ((pmap fprop) . unCompF))
instance P_fprop (ConstF w) where
  fprop = (fmap ConstF) . (Just . unConstF)
