module PolyLib.ZipVariants (pzipWith'', pzip'', pzip', P_fmap2, P_fzip, P_fflatten, P_fprop) where
import PolyLib.Prelude
import PolyLib.Zip (pzipWith, pzipWith', P_fmap2, P_fzip, P_fprop)
import PolyLib.Flatten (fl_all, P_fflatten)
pzip' :: (P_fmap2 a, FunctorOf a b, P_fzip a, P_fflatten a) => (b c, b d) -> (b (c, d), Bool)
pzip' p = (pzipWith' inn (const undefined) id p,
	   pzipWith' (and . fl_all) (const False) (const True) p)
pzipWith'' ::
  (FunctorOf a b, P_fmap2 a, P_fzip a, P_fprop a) => ((c, d) -> e) -> (b c, b d) -> Maybe (b e)
pzipWith'' op = pzipWith (return . op)
pzip'' :: (FunctorOf a b, P_fmap2 a, P_fzip a, P_fprop a) => (b c, b d) -> Maybe (b (c, d))
pzip'' = pzipWith'' id
