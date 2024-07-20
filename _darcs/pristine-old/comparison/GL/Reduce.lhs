> module Reduce where

> import GL2
> import BinTreeDatatype
> import BinTreeReps
> import Data.Generics hiding (Generic)
> import TreeDatatype
> import TreeReps2

Generic reduce combines the 'c' elements of a type constructor using
the 'combine' function given by the user.

The first argument of GReduce is the type that indexes the function.
The second argument is a "non-generic" type variable, the parametric type
of the combined element.

TODO COMPARISON:
Something nice in this approach is that adding non-generic variables
to a generic function is easy:
Just

> newtype Reducer c a b   =  Reducer { appReducer :: (c -> c -> c) -> c -> a -> c }
>
> instance Generic (Reducer c) where
>   unit                  =  Reducer (\ _ e _ -> e)
>   plus a b              =  Reducer (\ op e x ->  case x of
>                                               Inl l  ->  appReducer a op e l
>                                               Inr r  ->  appReducer b op e r)
>   prod a b              =  Reducer (\ op e x  ->  (appReducer a op e (outl x))
>                                                   `op` (appReducer b op e (outr x)))
>   view iso1 iso2 a      =  Reducer (\ op e x  ->  appReducer a op e (from iso1 x))
>   char                  =  Reducer (\ _ e _  ->  e)
>   int                   =  Reducer (\ _ e _  ->  e)
>   float                 =  Reducer (\ _ e _  ->  e)

A polymorphic count function that is parametrised on an operation
to perform on the 'a' elements of the structure.

> countG                  :: (Reducer c a b -> Reducer Int a1 b1) -> (a -> c) -> a1 -> Int
> countG rep f            =  appReducer (rep (Reducer (\ _ _ x -> f x))) (+) 0

The generic size function: each occurrence of an element yields 1

> sizeG :: (Num c) => (Reducer c a b -> Reducer Int a1 b1) -> a1 -> Int
> sizeG rep = countG rep (const 1)

The sum function yields the element itself, it constraints the type of the element
to Int.

> sumG :: (Reducer a a b -> Reducer Int a1 b1) -> a1 -> Int
> sumG rep = countG rep id

The functions below are instances of the generic functions that are used
in the test driver.

> sizeBTree :: BinTree a -> Int
> sizeBTree = sizeG bintree

> sizeListBTree :: [BinTree a] -> Int
> sizeListBTree = sizeG (rList . bintree)

> sizeListBTreeList :: [BinTree [a]] -> Int
> sizeListBTreeList = sizeG (rList . bintree . rList)

> sizeList :: [a] -> Int
> sizeList = sizeG rList

> sumListBTreeList :: [BinTree [Int]] -> Int
> sumListBTreeList = sumG (rList . bintree . rList)

> sizeListTree :: [Tree Int w] -> Int
> sizeListTree = sizeG (\rep -> rList (tree rep (defValue 0)))

> sumListTree :: [Tree Int w] -> Int
> sumListTree = sumG (\rep -> rList (tree rep (defValue 0)))

> defValue v = Reducer (\_ _ _ -> v)

The collect generic abstraction and the instance used in the test.

> collectG                :: (Reducer [a] a b -> Reducer [a2] a1 b1) -> a1 -> [a2]
> collectG rep              = appReducer (rep (Reducer (\ _ _ x -> [x]))) (++) []
>
> collectListBTreeList :: [BinTree [a]] -> [a]
> collectListBTreeList = collectG (rList . bintree . rList)

> collectListTree :: [Tree a w] -> [a]
> collectListTree = collectG (\rep -> rList (tree rep (defValue [])))
