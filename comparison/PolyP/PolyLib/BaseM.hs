{-# LANGUAGE UndecidableInstances #-}
module PolyLib.BaseM (pmapM,
		      pmapMr,
		      pmapMl,
		      pmapM',
		      fmap2M,
		      cataM,
		      anaM,
		      hyloM,
		      paraM,
		      innM,
		      outM,
		      idM,
		      (@@),
		      P_fmap2Mr,
		      P_fmap2M',
		      P_fmap2M) where
import PolyLib.Prelude
import PolyLib.Base
import Control.Monad
(@@) :: Monad a => (b -> a c) -> (d -> a b) -> d -> a c
f @@ g = \y -> (g y) >>= f
opM :: Monad a => Bool -> (a b, a c) -> a (b, c)
opM b p = case b of
	    True -> (fst p) >>= (\x -> (snd p) >>= (\y -> return (x, y)))
	    False -> (snd p) >>= (\y -> (fst p) >>= (\x -> return (x, y)))
idM :: Monad a => b -> a b
idM = return
innM :: (Monad a, FunctorOf b c) => b d (c d) -> a (c d)
innM = idM . inn
outM :: (Monad a, FunctorOf b c) => c d -> a (b d (c d))
outM = idM . out
prodmapMr ::
  Monad a => (b c d -> a (e f g)) -> (h c d -> a (i f g)) -> ProdF b h c d -> a (ProdF e i f g)
prodmapMr f g p' = let p = unProdF p'
		   in (g (snd p)) >>= (\y -> (f (fst p)) >>= (\x -> return (x :*: y)))
prodmapM ::
  Monad a => (b c d -> a (e f g)) -> (h c d -> a (i f g)) -> ProdF b h c d -> a (ProdF e i f g)
prodmapM f g p' = let p = unProdF p'
		  in (f (fst p)) >>= (\x -> (g (snd p)) >>= (\y -> return (x :*: y)))
summapM ::
  Monad a => (b c d -> a (e f g)) -> (h c d -> a (i f g)) -> SumF b h c d -> a (SumF e i f g)
summapM f g = foldSum ((liftM InL) . f) ((liftM InR) . g)
pmapMr :: (Monad a, FunctorOf b c, P_fmap2Mr b) => (d -> a e) -> c d -> a (c e)
pmapMr fM = (liftM inn) . ((fmap2Mr fM (pmapMr fM)) . out)
class P_fmap2Mr a where
  fmap2Mr :: Monad b => (c -> b d) -> (e -> b f) -> a c e -> b (a d f)
instance (P_fmap2Mr g, P_fmap2Mr h) => P_fmap2Mr (SumF g h) where
  fmap2Mr = \p r -> summapM (fmap2Mr p r) (fmap2Mr p r)
instance (P_fmap2Mr n, P_fmap2Mr o) => P_fmap2Mr (ProdF n o) where
  fmap2Mr = \p r -> prodmapMr (fmap2Mr p r) (fmap2Mr p r)
instance P_fmap2Mr EmptyF where
  fmap2Mr = \p r -> const $ (return EmptyF)
instance P_fmap2Mr ParF where
  fmap2Mr = \p r -> (liftM ParF) . (p . unParF)
instance P_fmap2Mr RecF where
  fmap2Mr = \p r -> (liftM RecF) . (r . unRecF)
instance (FunctorOf functorOf_aj aj, P_fmap2Mr functorOf_aj, P_fmap2Mr ak) =>
	   P_fmap2Mr (CompF aj ak) where
  fmap2Mr = \p r -> (liftM CompF) . ((pmapMr (fmap2Mr p r)) . unCompF)
instance P_fmap2Mr (ConstF aq) where
  fmap2Mr = \p r -> (liftM ConstF) . (return . unConstF)
cataMr :: (Monad a, FunctorOf b c, P_fmap2Mr b) => (b d e -> a e) -> c d -> a e
cataMr iM = (iM @@ (fmap2Mr idM (cataMr iM))) . out
anaMr :: (Monad a, FunctorOf b c, P_fmap2Mr b) => (d -> a (b e d)) -> d -> a (c e)
anaMr oM = (liftM inn) . ((fmap2Mr idM (anaMr oM)) @@ oM)
hyloMr :: (Monad a, P_fmap2Mr b) => (b c d -> a d) -> (e -> a (b c e)) -> e -> a d
hyloMr iM oM = (iM @@ (fmap2Mr idM (hyloMr iM oM))) @@ oM
pmapM' :: (Monad a, FunctorOf b c, P_fmap2M' b) => Bool -> (d -> a e) -> c d -> a (c e)
pmapM' ord fM = (liftM inn) . ((fmap2M' ord fM (pmapM' ord fM)) . out)
class P_fmap2M' a where
  fmap2M' :: Monad b => Bool -> (c -> b d) -> (e -> b f) -> a c e -> b (a d f)
instance (P_fmap2M' g, P_fmap2M' h) => P_fmap2M' (SumF g h) where
  fmap2M' = \ord p r -> summapM (fmap2M' ord p r) (fmap2M' ord p r)
instance (P_fmap2M' n, P_fmap2M' o) => P_fmap2M' (ProdF n o) where
  fmap2M' = \ord p r ->
	      (liftM (uncurry (:*:))) .
	      ((opM ord) . (foldProd (\x y -> (fmap2M' ord p r x, fmap2M' ord p r y))))
instance P_fmap2M' EmptyF where
  fmap2M' = \ord p r -> const $ (return EmptyF)
instance P_fmap2M' ParF where
  fmap2M' = \ord p r -> (liftM ParF) . (p . unParF)
instance P_fmap2M' RecF where
  fmap2M' = \ord p r -> (liftM RecF) . (r . unRecF)
instance (FunctorOf functorOf_aj aj, P_fmap2M' functorOf_aj, P_fmap2M' ak) =>
	   P_fmap2M' (CompF aj ak) where
  fmap2M' = \ord p r -> (liftM CompF) . ((pmapM' ord (fmap2M' ord p r)) . unCompF)
instance P_fmap2M' (ConstF aq) where
  fmap2M' = \ord p r -> (liftM ConstF) . (return . unConstF)
pmapM :: (Monad a, FunctorOf b c, P_fmap2M b) => (d -> a e) -> c d -> a (c e)
pmapM fM = (liftM inn) . ((fmap2M fM (pmapM fM)) . out)
class P_fmap2M a where
  fmap2M :: Monad b => (c -> b d) -> (e -> b f) -> a c e -> b (a d f)
instance (P_fmap2M g, P_fmap2M h) => P_fmap2M (SumF g h) where
  fmap2M = \p r -> summapM (fmap2M p r) (fmap2M p r)
instance (P_fmap2M n, P_fmap2M o) => P_fmap2M (ProdF n o) where
  fmap2M = \p r -> prodmapM (fmap2M p r) (fmap2M p r)
instance P_fmap2M EmptyF where
  fmap2M = \p r -> const $ (return EmptyF)
instance P_fmap2M ParF where
  fmap2M = \p r -> (liftM ParF) . (p . unParF)
instance P_fmap2M RecF where
  fmap2M = \p r -> (liftM RecF) . (r . unRecF)
instance (FunctorOf functorOf_aj aj, P_fmap2M functorOf_aj, P_fmap2M ak) =>
	   P_fmap2M (CompF aj ak) where
  fmap2M = \p r -> (liftM CompF) . ((pmapM (fmap2M p r)) . unCompF)
instance P_fmap2M (ConstF aq) where
  fmap2M = \p r -> (liftM ConstF) . (return . unConstF)
cataM :: (Monad a, FunctorOf b c, P_fmap2M b) => (b d e -> a e) -> c d -> a e
cataM iM = (iM @@ (fmap2M idM (cataM iM))) . out
anaM :: (Monad a, FunctorOf b c, P_fmap2M b) => (d -> a (b e d)) -> d -> a (c e)
anaM oM = (liftM inn) . ((fmap2M idM (anaM oM)) @@ oM)
hyloM :: (Monad a, P_fmap2M b) => (b c d -> a d) -> (e -> a (b c e)) -> e -> a d
hyloM iM oM = (iM @@ (fmap2M idM (hyloM iM oM))) @@ oM
paraM :: (Monad a, FunctorOf b c, P_fmap2M b) => (c d -> b d e -> a e) -> c d -> a e
paraM iM x = (iM x) =<< (fmap2M idM (paraM iM) (out x))
pmapMl :: (FunctorOf a b, Monad c, P_fmap2M a) => (d -> c e) -> b d -> c (b e)
pmapMl = pmapM
