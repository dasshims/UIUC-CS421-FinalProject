
{-
 -
 - Generic Transpose function, an experiment/toy.
 -
 - It is more direct (no serialization/deserialization) than the one in dir
 -   PolyP/PolyLib/Transpose.hs
 -
 -
 - todo: + make part of testsuite
 -       + generalize a bit more (types with more arguments)
 -}

import Test.QuickCheck
import Data.List
import Debug.Trace

data Tree a = Fork (Tree a) a (Tree a) | Empty deriving Show

-- Instance of transpose (I wrote it to get a feel on how to write the generic one)

replace :: a -> Tree b -> Tree a
replace _ Empty = Empty
replace x (Fork ys y zs) = Fork (replace x ys) x (replace x zs)

tZipWith3 f Empty Empty Empty = Empty
tZipWith3 f (Fork ys1 y1 zs1) (Fork ys2 y2 zs2) (Fork ys3 y3 zs3)
  = Fork (tZipWith3 f ys1 ys2 ys3) (f y1 y2 y3) (tZipWith3 f zs1 zs2 zs3)

transposeT Empty = Empty -- crappy result due to lack of information
transposeT xs@(Fork _ x _) = transpose' x xs

transpose' :: Tree a {- shape of inner structure -} -> Tree (Tree a) -> Tree (Tree a)
transpose' shape Empty = replace Empty shape
transpose' shape (Fork xs x ys) = tZipWith3 Fork (transpose' shape xs) x (transpose' shape ys)

tree1 = Fork Empty (Fork Empty 1 (Fork Empty 2 Empty)) Empty

fromList [] = Empty
fromList (x:xs) = Fork Empty x (fromList xs)

toList Empty = []
toList (Fork Empty x xs) = x : toList xs

showInput f x = trace (show x) (f x)

mytrans = map toList . toList . showInput transposeT . fromList . map fromList

prop_trans :: Int -> Int -> Bool
prop_trans d1 d2 = let xss = replicate d1 (replicate d1 0) :: [[Int]] in
                   mytrans xss == transpose xss

-- End of Tree specific transpose


-- Generic representation for writing transpose

data K a x = K a
data ProdF a b x = Prod (a x) (b x)
data SumF a b x = Inl (a x) | Inr (b x)
data Id a = Id a

data Rep :: * -> * where
  RUnit :: Rep ()

data RepF :: (* -> *) -> * where
  RK :: RepF (K a)
  RProd :: RepF a -> RepF b -> RepF (ProdF a b)
  RSum :: RepF a -> RepF b -> RepF (SumF a b)
  RId :: RepF Id
  RList :: RepF []
  RView :: RepF g -> (forall a . EP (f a) (g a)) -> RepF f
--RComp :: RepF 

data EP a b = EP { from :: a -> b , to :: b -> a }

type L' a = (SumF (K ()) (ProdF Id [])) a

rList :: RepF []
rList = RView (RSum RK (RProd RId rList)) (EP fromListF toListF)

fromListF :: [a] -> L' a
fromListF [] = Inl (K ())
fromListF (x:xs) = Inr (Prod (Id x) xs)

toListF :: L' a -> [a]
toListF (Inl (K ())) = []
toListF (Inr (Prod (Id x) xs)) = x:xs

rTree :: RepF Tree
rTree = RView (RSum (RProd (RProd rTree RId) rTree) RK) (EP fromTreeF toTreeF)

fromTreeF Empty = Inr (K ())
fromTreeF (Fork xs x ys) = Inl (Prod (Prod xs (Id x)) ys)

toTreeF (Inr (K ())) = Empty
toTreeF (Inl (Prod (Prod xs (Id x)) ys)) = Fork xs x ys

-- Wrapper over generic transpose
transposeF :: RepF d -> RepF e -> d (e a) -> e (d a)
transposeF repd repe xss
  = case collectF repd xss of
	[] -> error "No elements!"
        x:_ -> transposeF' (replaceF repe x) repd repe xss

-- Generic transpose
--   if this abstracts over return and apply it is no longer a generic
--   function over two arguments.
transposeF' :: (forall a.a -> e a) -> RepF d -> RepF e -> d (e a) -> e (d a)
transposeF' inj RK repe (K x) = inj (K x)
transposeF' inj RId repe (Id x) = mapF repe Id x
transposeF' inj (RSum a b) repe (Inl x)
    = mapF repe Inl $ transposeF' inj a repe x
transposeF' inj (RSum a b) repe (Inr x)
    = mapF repe Inr $ transposeF' inj b repe x
transposeF' inj (RProd a b) repe (Prod x y)
    = zipF repe Prod
           (transposeF' inj a repe x)
           (transposeF' inj b repe y)
transposeF' inj (RView rep ep) repe xss
    = mapF repe (to ep) $
      transposeF' inj rep repe (from ep xss)                                          

-- Generalised matrix multiplication
mult :: forall a d e f. Num a => RepF d -> RepF e -> RepF f -> d (e a) -> e (f a) -> d (f a)
mult repd repe repf xss yss = mapF repd (\v -> mapF repf (inner repe v) (transposeF repe repf yss) )
                                        xss
-- Inner product
inner :: Num a => RepF e -> e a -> e a -> a
inner repe xs ys = sum $ collectF repe $ zipF repe (*) xs ys

-- Generic collect aka flatten
collectF :: RepF f -> f a -> [a]           
collectF RK _ = []                         
collectF RId (Id x) = [x]
collectF (RSum a b) (Inl x) = collectF a x
collectF (RSum a b) (Inr x) = collectF b x
collectF (RProd a b) (Prod x y) = collectF a x ++ collectF b y
collectF (RView rep ep) xs = collectF rep (from ep xs)

-- Replace all occurrences of a's with the same b
replaceF :: RepF f -> f a -> b -> f b
replaceF rep xs x = mapF rep (const x) xs

-- Generic zip
zipF :: RepF f -> (a -> b -> c) -> f a -> f b -> f c 
zipF RK f (K x) (K _) = (K x) -- if they are not the same it is weird!
zipF RId f (Id x) (Id y) = Id (f x y)
zipF (RSum a b) f (Inl x) (Inl y) = Inl $ zipF a f x y
zipF (RSum a b) f (Inr x) (Inr y) = Inr $ zipF b f x y
zipF (RProd a b) f (Prod x1 y1) (Prod x2 y2) = Prod (zipF a f x1 x2) (zipF b f y1 y2)
zipF (RView rep ep) f x y = to ep $ zipF rep f (from ep x) (from ep y)

-- Generic map
mapF :: RepF f -> (a -> b) -> f a -> f b
mapF RK f (K x) = (K x)
mapF RId f (Id x) = Id (f x)
mapF (RSum a b) f (Inl x) = Inl $ mapF a f x
mapF (RSum a b) f (Inr y) = Inr $ mapF b f y
mapF (RProd a b) f (Prod x y) = Prod (mapF a f x) (mapF b f y)
mapF (RView rep ep) f x = to ep $ mapF rep f (from ep x)

