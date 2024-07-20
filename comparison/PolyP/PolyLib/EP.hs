module PolyLib.EP where
import PolyLib.Prelude
data EP a b = EP (a -> b) (b -> a) 
(<.>) :: EP a b -> EP b c -> EP a c
(EP t1 f1) <.> (EP t2 f2) = EP (t2 . t1) (f1 . f2)
epTo :: EP a b -> a -> b
epTo (EP to _) = to
epFrom :: EP a b -> b -> a
epFrom (EP _ from) = from
epSum :: EP (a b c) (d e f) -> EP (g b c) (h e f) -> EP (SumF a g b c) (SumF d h e f)
epSum epG epH = EP ((epTo epG) -+- (epTo epH)) ((epFrom epG) -+- (epFrom epH))
epProd :: EP (a b c) (d e f) -> EP (g b c) (h e f) -> EP (ProdF a g b c) (ProdF d h e f)
epProd epG epH = EP ((epTo epG) -*- (epTo epH)) ((epFrom epG) -*- (epFrom epH))
epFun :: EP (a b c) (a d e) -> EP (f b c) (f d e) -> EP (FunF a f b c) (FunF a f d e)
epFun epG epH = EP ((epFrom epG) ->- (epTo epH)) ((epTo epG) ->- (epFrom epH))
pEP :: (FunctorOf a b, P_fEP a) => EP c d -> EP (b c) (b d)
pEP ep = ((EP out inn) <.> (fEP ep (pEP ep))) <.> (EP inn out)
class P_fEP a where
  fEP :: EP b c -> EP d e -> EP (a b d) (a c e)
instance (P_fEP f, P_fEP g) => P_fEP (SumF f g) where
  fEP = \epP epR -> epSum (fEP epP epR) (fEP epP epR)
instance (P_fEP l, P_fEP m) => P_fEP (ProdF l m) where
  fEP = \epP epR -> epProd (fEP epP epR) (fEP epP epR)
instance P_fEP EmptyF where
  fEP = \epP epR -> EP (const EmptyF) (const EmptyF)
instance P_fEP ParF where
  fEP = \epP epR -> ((EP unParF ParF) <.> epP) <.> (EP ParF unParF)
instance P_fEP RecF where
  fEP = \epP epR -> ((EP unRecF RecF) <.> epR) <.> (EP RecF unRecF)
instance (P_fEP ad, P_fEP ae) => P_fEP (FunF ad ae) where
  fEP = \epP epR -> epFun (fEP epP epR) (fEP epP epR)
instance (FunctorOf functorOf_aj aj, P_fEP functorOf_aj, P_fEP ak) => P_fEP (CompF aj ak) where
  fEP = \epP epR -> ((EP unCompF CompF) <.> (pEP (fEP epP epR))) <.> (EP CompF unCompF)
instance P_fEP (ConstF ap) where
  fEP = \epP epR -> (EP unConstF ConstF) <.> (EP ConstF unConstF)
