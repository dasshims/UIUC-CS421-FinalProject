module PolyLib.EqualVariants (pequal, peq, P_fflatten, P_fmap2, P_fprop, P_fzip) where
import PolyLib.Prelude
import PolyLib.Flatten (flatten, fl_all, P_fflatten, P_fmap2)
import PolyLib.Zip (pzip, pzipWith', P_fprop, P_fzip)
peq' :: (Eq a, FunctorOf b c, P_fflatten b, P_fmap2 b, P_fprop b, P_fzip b) => c a -> c a -> Bool
peq' x y = maybe False ((all (uncurry (==))) . flatten) (pzip (x, y))
peq :: (P_fmap2 a, FunctorOf a b, P_fzip a, P_fflatten a, Eq c) => b c -> b c -> Bool
peq l r = pzipWith' (and . fl_all) (\_ -> False) (uncurry (==)) (l, r)
pequal ::
  (P_fmap2 a, FunctorOf a b, P_fzip a, P_fflatten a) => (c -> d -> Bool) -> b c -> b d -> Bool
pequal op l r = pzipWith' (and . fl_all) (\_ -> False) (uncurry op) (l, r)
