{-# OPTIONS_GHC -fallow-undecidable-instances #-}
module PolyLib.ConstructorName where
import PolyLib.Prelude
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f p = (f (fst p), snd p)
class P_fconstructor2Int a where
  fconstructor2Int :: a b c -> Int
instance P_fconstructor2Int e => P_fconstructor2Int (SumF d e) where
  fconstructor2Int = foldSum (const 0) ((\n -> 1 + n) . fconstructor2Int)
instance P_fconstructor2Int (ProdF h i) where
  fconstructor2Int = const 0
instance P_fconstructor2Int EmptyF where
  fconstructor2Int = const 0
instance P_fconstructor2Int ParF where
  fconstructor2Int = const 0
instance P_fconstructor2Int RecF where
  fconstructor2Int = const 0
instance P_fconstructor2Int (CompF r s) where
  fconstructor2Int = const 0
instance P_fconstructor2Int (ConstF v) where
  fconstructor2Int = const 0
constructor2Int :: (FunctorOf a b, P_fconstructor2Int a) => b c -> Int
constructor2Int = fconstructor2Int . out
class P_fconstructorArity a where
  fconstructorArity :: a b c -> Int
instance (P_fconstructorArity d, P_fconstructorArity e) => P_fconstructorArity (SumF d e) where
  fconstructorArity = foldSum fconstructorArity fconstructorArity
instance (P_fconstructorArity h, P_fconstructorArity i) => P_fconstructorArity (ProdF h i) where
  fconstructorArity = \(x :*: y) -> (fconstructorArity x) + (fconstructorArity y)
instance P_fconstructorArity EmptyF where
  fconstructorArity = const 0
instance P_fconstructorArity ParF where
  fconstructorArity = const 1
instance P_fconstructorArity RecF where
  fconstructorArity = const 1
instance P_fconstructorArity (CompF r s) where
  fconstructorArity = const 1
instance P_fconstructorArity (ConstF v) where
  fconstructorArity = const 1
class P_fconstructorArgs a where
  fconstructorArgs :: a b c
instance (P_fconstructorArgs d, P_fconstructorArgs e) => P_fconstructorArgs (ProdF d e) where
  fconstructorArgs = fconstructorArgs :*: fconstructorArgs
instance P_fconstructorArgs EmptyF where
  fconstructorArgs = EmptyF
instance P_fconstructorArgs ParF where
  fconstructorArgs = ParF undefined
instance P_fconstructorArgs RecF where
  fconstructorArgs = RecF undefined
instance P_fconstructorArgs (CompF n o) where
  fconstructorArgs = CompF undefined
instance P_fconstructorArgs (ConstF r) where
  fconstructorArgs = ConstF undefined
class P_fconstructors a where
  fconstructors :: [a b c]
instance (P_fconstructors d, P_fconstructors e) => P_fconstructors (SumF d e) where
  fconstructors = (map InL fconstructors) ++ (map InR fconstructors)
instance P_fconstructorArgs h => P_fconstructors h where
  fconstructors = fconstructorArgs : []
class P_fconstructorsAndArities a where
  fconstructorsAndArities :: [(a b c, Int)]
instance (P_fconstructorsAndArities d, P_fconstructorsAndArities e) =>
	   P_fconstructorsAndArities (SumF d e) where
  fconstructorsAndArities = (map (mapFst InL) fconstructorsAndArities) ++
			    (map (mapFst InR) fconstructorsAndArities)
instance (P_fconstructorArity (ProdF h i), P_fconstructors (ProdF h i)) =>
	   P_fconstructorsAndArities (ProdF h i) where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
instance P_fconstructorsAndArities EmptyF where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
instance P_fconstructorsAndArities ParF where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
instance P_fconstructorsAndArities RecF where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
instance (P_fconstructorArity (CompF r s), P_fconstructors (CompF r s)) =>
	   P_fconstructorsAndArities (CompF r s) where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
instance (P_fconstructorArity (ConstF v), P_fconstructors (ConstF v)) =>
	   P_fconstructorsAndArities (ConstF v) where
  fconstructorsAndArities = (\x -> (x, fconstructorArity x) : []) $ (head fconstructors)
int2fconstructor :: P_fconstructors a => Int -> a b c
int2fconstructor n = fconstructors !! n
constructors :: (FunctorOf a b, P_fconstructors a) => [b c]
constructors = map inn fconstructors
int2constructor :: (FunctorOf a b, P_fconstructors a) => Int -> b c
int2constructor n = constructors !! n
constructorsAndArities :: (FunctorOf a b, P_fconstructorsAndArities a) => [(b c, Int)]
constructorsAndArities = map (mapFst inn) fconstructorsAndArities
constructorNamesAndArities :: (FunctorOf a b, P_fconstructorsAndArities a) => b c -> [([Char], Int)]
constructorNamesAndArities x = (map (mapFst constructorName)) $
			       (asTypeOf constructorsAndArities ((x, undefined) : []))
constructorNames :: (FunctorOf a b, P_fconstructors a) => b c -> [[Char]]
constructorNames x = (map constructorName) $ (asTypeOf constructors (x : []))
constructorArity :: (FunctorOf a b, P_fconstructorArity a) => b c -> Int
constructorArity = fconstructorArity . out
