{-# OPTIONS_GHC -fallow-undecidable-instances #-}
module PolyLib.Show where
import PolyLib.Prelude
import PolyLib.Base
import PolyLib.ConstructorName
isOp :: [Char] -> Bool
isOp s = case s of
	   (':' : _) -> True
	   _ -> False
mkConstrName :: (Num a, Ord a) => a -> [Char] -> [Char]
mkConstrName ar name = (case isOp name of
			  True -> "(" ++ (name ++ ")")
			  _ -> name) ++
		       (case ar > 0 of
			  True -> " "
			  _ -> "")
pshowsPrec' ::
  (FunctorOf a b, P_fconstructorArity a, P_fshowsPrec a, P_fmap2 a) =>
    (Int -> c -> [Char] -> [Char]) -> Int -> b c -> [Char] -> [Char]
pshowsPrec' s p x = let ar = constructorArity x
		    in let cons = mkConstrName ar (constructorName x)
		       in (case ar > 0 of
			     True -> showParen (p > 9)
			     _ -> id) $
			  ((showString cons) .
			   ((fshowsPrec 10) $
			    ((fmap2 (flip s) (flip $ (pshowsPrec' s))) $ (out x))))
class P_fshowsPrec a where
  fshowsPrec :: Int -> a (Int -> [Char] -> [Char]) (Int -> [Char] -> [Char]) -> [Char] -> [Char]
instance (P_fshowsPrec b, P_fshowsPrec c) => P_fshowsPrec (SumF b c) where
  fshowsPrec = \p -> foldSum (fshowsPrec p) (fshowsPrec p)
instance (P_fshowsPrec d, P_fshowsPrec e) => P_fshowsPrec (ProdF d e) where
  fshowsPrec = \p ->
		 (\(x, y) -> x . ((showString " ") . y)) . ((fshowsPrec 10) -**- (fshowsPrec 10))
instance P_fshowsPrec EmptyF where
  fshowsPrec = \p -> const id
instance P_fshowsPrec ParF where
  fshowsPrec = \p x -> unParF x p
instance P_fshowsPrec RecF where
  fshowsPrec = \p x -> unRecF x p
instance (FunctorOf functorOf_f f,
	  P_fconstructorArity functorOf_f,
	  P_fshowsPrec functorOf_f,
	  P_fmap2 functorOf_f,
	  P_fshowsPrec g) =>
	   P_fshowsPrec (CompF f g) where
  fshowsPrec = \p -> (pshowsPrec' fshowsPrec p) . unCompF
instance Show h => P_fshowsPrec (ConstF h) where
  fshowsPrec = \p -> (showsPrec p) . unConstF
pshowsPrec ::
  (FunctorOf a b, P_fconstructorArity a, P_fshowsPrec a, P_fmap2 a, Show c) =>
    Int -> b c -> [Char] -> [Char]
pshowsPrec = pshowsPrec' showsPrec
pshow :: (FunctorOf a b, Show c, P_fconstructorArity a, P_fshowsPrec a, P_fmap2 a) => b c -> [Char]
pshow x = pshowsPrec 0 x ""
