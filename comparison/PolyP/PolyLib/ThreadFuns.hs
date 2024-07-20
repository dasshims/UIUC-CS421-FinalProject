module PolyLib.ThreadFuns (propagate, cross, P_fmap2, P_fthread) where
import PolyLib.Prelude
import PolyLib.Thread (thread, P_fmap2, P_fthread)
propagate :: (FunctorOf a b, P_fmap2 a, P_fthread a) => b (Maybe c) -> Maybe (b c)
propagate = thread
cross :: (FunctorOf a b, P_fmap2 a, P_fthread a) => b [c] -> [b c]
cross = thread
