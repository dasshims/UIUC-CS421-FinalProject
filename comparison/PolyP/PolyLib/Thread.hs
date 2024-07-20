module PolyLib.Thread (thread, pmapM, fthread, fmap2M, P_fmap2, P_fthread) where
import PolyLib.Prelude
import Control.Monad
import PolyLib.Base (cata, pmap, P_fmap2)
import PolyLib.BaseM (pmapM, fmap2M, (@@), P_fmap2M)
prodthread :: Monad a => (a (b c d), a (e c d)) -> a (ProdF b e c d)
prodthread (mx, my) = mx >>= (\x -> my >>= (\y -> return (x :*: y)))
sumthread :: Monad a => Either (a (b c d)) (a (e c d)) -> a (SumF b e c d)
sumthread = either (liftM InL) (liftM InR)
thread :: (FunctorOf a b, P_fmap2 a, Monad c, P_fthread a) => b (c d) -> c (b d)
thread = cata ((liftM inn) . fthread)
class P_fthread a where
  fthread :: Monad b => a (b c) (b d) -> b (a c d)
instance (P_fthread e, P_fthread f) => P_fthread (SumF e f) where
  fthread = sumthread . (fthread -++- fthread)
instance (P_fthread j, P_fthread k) => P_fthread (ProdF j k) where
  fthread = prodthread . (fthread -**- fthread)
instance P_fthread EmptyF where
  fthread = \EmptyF -> return EmptyF
instance P_fthread ParF where
  fthread = (liftM ParF) . unParF
instance P_fthread RecF where
  fthread = (liftM RecF) . unRecF
instance (FunctorOf functorOf_x x, P_fmap2 functorOf_x, P_fthread functorOf_x, P_fthread y) =>
	   P_fthread (CompF x y) where
  fthread = (liftM CompF) . (thread . ((pmap fthread) . unCompF))
instance P_fthread (ConstF ad) where
  fthread = return . (ConstF . unConstF)
