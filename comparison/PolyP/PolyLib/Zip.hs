module PolyLib.Zip (pzip,
		    fzip,
		    pzipWith,
		    pzipWith',
		    fzipWith,
		    punzip,
		    P_fprop,
		    P_fmap2,
		    P_fzip) where
import PolyLib.Prelude
import PolyLib.Base (pmap, fmap2, P_fmap2)
import PolyLib.Propagate (fprop, propagate, sumprop, prodprop, P_fprop)
innM :: FunctorOf a b => a c (b c) -> Maybe (b c)
innM = return . inn
zeroM :: Maybe a
zeroM = Nothing
(@@) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
g @@ f = \a -> (f a) >>= g
constzip :: Eq a => (a, a) -> Maybe a
constzip (x, y) = case x == y of
		    True -> return x
		    _ -> zeroM
prodzip :: (ProdF a b c d, ProdF e f g h) -> Maybe ((a c d, e g h), (b c d, f g h))
prodzip (a :*: b, c :*: d) = Just ((a, c), (b, d))
mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (x, y) = (f x, g y)
sumzip :: (SumF a b c d, SumF e f g h) -> Maybe (Either (a c d, e g h) (b c d, f g h))
sumzip p = case p of
	     (InL s, InL t) -> return (Left (s, t))
	     (InR s, InR t) -> return (Right (s, t))
	     _ -> zeroM
pzip :: (FunctorOf a b, P_fprop a, P_fmap2 a, P_fzip a) => (b c, b d) -> Maybe (b (c, d))
pzip = ((innM @@ (fprop . (fmap2 return pzip))) @@ fzip) . (mapPair out out)
class P_fzip a where
  fzip :: (a b c, a d e) -> Maybe (a (b, d) (c, e))
instance (P_fzip f, P_fzip g) => P_fzip (SumF f g) where
  fzip = (sumprop . (either (Left . fzip) (Right . fzip))) @@ sumzip
instance (P_fzip l, P_fzip m) => P_fzip (ProdF l m) where
  fzip = (prodprop . (mapPair fzip fzip)) @@ prodzip
instance P_fzip EmptyF where
  fzip = const (return EmptyF)
instance P_fzip ParF where
  fzip = \(ParF x, ParF y) -> return $ (ParF (x, y))
instance P_fzip RecF where
  fzip = \(RecF x, RecF y) -> return $ (RecF (x, y))
instance (P_fzip ae, FunctorOf aj ad, P_fmap2 aj, P_fprop aj, P_fzip aj) =>
	   P_fzip (CompF ad ae) where
  fzip = ((fmap CompF) . (propagate . (pmap fzip))) @@ (pzip . (mapPair unCompF unCompF))
instance Eq al => P_fzip (ConstF al) where
  fzip = (fmap ConstF) . (constzip . (mapPair unConstF unConstF))
pzipWith' ::
  (P_fmap2 a, FunctorOf a b, P_fzip a) =>
    (a c d -> d) -> ((b e, b f) -> d) -> ((e, f) -> c) -> (b e, b f) -> d
pzipWith' ins fail op (x, y) = maybe
				 (fail (x, y))
				 (ins . (fmap2 op (pzipWith' ins fail op)))
				 (fzip (out x, out y))
pzipWith ::
  (FunctorOf a b, P_fmap2 a, P_fzip a, P_fprop a) =>
    ((c, d) -> Maybe e) -> (b c, b d) -> Maybe (b e)
pzipWith = pzipWith' ((fmap inn) . fprop) (const zeroM)
fzipWith ::
  (P_fmap2 a, P_fzip a) => ((b, c) -> d) -> ((e, f) -> g) -> (a b e, a c f) -> Maybe (a d g)
fzipWith f g = (fmap (fmap2 f g)) . fzip
funzip :: P_fmap2 a => a (b, c) (d, e) -> (a b d, a c e)
funzip x = (fmap2 fst fst x, fmap2 snd snd x)
punzip :: (FunctorOf a b, P_fmap2 a) => b (c, d) -> (b c, b d)
punzip x = (pmap fst x, pmap snd x)
