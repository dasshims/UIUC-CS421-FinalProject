module PolyLib.CrushFuns (psum,
			  prod,
			  conc,
			  pand,
			  por,
			  size,
			  flatten,
			  pall,
			  pany,
			  pelem,
			  P_fmap2,
			  P_fcrush) where
import PolyLib.Prelude
import PolyLib.Crush (crush, P_fmap2, P_fcrush)
import PolyLib.Base (pmap)
por :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b Bool -> Bool
por = crush (||) False
pany :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => (c -> Bool) -> b c -> Bool
pany p = por . (pmap p)
pelem :: (FunctorOf a b, P_fmap2 a, P_fcrush a, Eq c) => c -> b c -> Bool
pelem x = pany (\y -> x == y)
pand :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b Bool -> Bool
pand = crush (&&) True
pall :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => (c -> Bool) -> b c -> Bool
pall p = pand . (pmap p)
conc :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b [c] -> [c]
conc = crush (++) []
flatten :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b c -> [c]
flatten = conc . (pmap (\x -> x : []))
comp :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b (c -> c) -> c -> c
comp = crush (.) id
flatten' :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b c -> [c] -> [c]
flatten' = comp . (pmap (:))
prod :: (FunctorOf a b, P_fmap2 a, P_fcrush a, Num c) => b c -> c
prod = crush (*) 1
psum :: (FunctorOf a b, P_fmap2 a, P_fcrush a, Num c) => b c -> c
psum = crush (+) 0
size :: (FunctorOf a b, P_fmap2 a, P_fcrush a) => b c -> Int
size = psum . (pmap (\_ -> 1))
