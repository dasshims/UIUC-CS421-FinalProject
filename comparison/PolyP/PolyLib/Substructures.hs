module PolyLib.Substructures (substructures, P_fmap2, P_fflatten) where
import PolyLib.Prelude
import PolyLib.Base (para, P_fmap2)
import PolyLib.Flatten (fl_rec, P_fflatten)
substructures :: (FunctorOf a b, P_fmap2 a, P_fflatten a) => b c -> [b c]
substructures = para (\x y -> x : (concat (fl_rec y)))
