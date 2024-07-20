> module CrushRight where

> import GL2
> import BinTreeDatatype
> import BinTreeReps2
> import Data.Generics hiding (Generic)
> import TreeDatatype
> import TreeReps2

Generic reduce combines the 'c' elements of a type constructor using
the 'combine' function given by the user.

The arguments of CrushRight are:
  * c : parametric type of the elements in the structre that are to be combined.
  * b : result of combination.
  * a : type that indexes the function
  * x : used for arity two functions, like map. It is not used here.

> newtype CrushRight b a x   =  CrushRight { crushRight :: a -> b -> b }
>
> instance Generic (CrushRight b) where
>   unit                  =  CrushRight (\ _ e -> e)
>   plus a b              =  CrushRight (\ x e ->  case x of
>                                               Inl l  ->  crushRight a l e
>                                               Inr r  ->  crushRight b r e)
>   prod a b              =  CrushRight (\ x e  ->  (crushRight a (outl x)
>                                                     (crushRight b (outr x) e)
>                                                     ))
>   view iso1 iso2 a      =  CrushRight (\ x e ->  crushRight a (from iso1 x) e)
>   char                  =  CrushRight (\ _ e  ->  e)
>   int                   =  CrushRight (\ _ e  ->  e)
>   float                 =  CrushRight (\ _ e  ->  e)


A crushRight for types |T| with kind *->*, these types
are represented by the type |RepF f a b| where |a| represents
the argument type and |b| represents the type constructor
applied to |a|. In short, |b === T a|.
For example, the representation of lists is |RepF f a [a] z|

We do not use second arities here, so the second arity variable
is made equal to the first.

Note that the type corresponding to the second arity is "ignored".

> type RepF f a b  = forall c . f c a a -> f c b b

> crushr :: RepF CrushRight a b -> (a -> d -> d) -> d -> b -> d
> crushr tyConRep op b x = crushRight rep x b
>   where
>     -- the a-values in the b-structure are combined with the accumulated b
>     rep = tyConRep (CrushRight op)

> defValue = CrushRight (\_ b -> b)

A polymorphic count function that is parametrised on an operation
to perform on the 'a' elements of the structure.

> countG                  :: RepF CrushRight a b -> (a -> Int) -> b -> Int
> countG tyConRep f        =  crushr tyConRep ((+) . f) 0

The generic size function: each occurrence of an element yields 1

> sizeG :: RepF CrushRight a b -> b -> Int
> sizeG tyConRep = countG tyConRep (const 1)

The sum function yields the element itself, it constraints the type of the element
to Int.

> sumG :: RepF CrushRight Int b -> b -> Int
> sumG tyConRep = countG tyConRep id

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

> sizeListTree :: [WTree Int w] -> Int
> sizeListTree = sizeG (\rep -> rList (tree rep defValue))

> sumListTree :: [WTree Int w] -> Int
> sumListTree = sumG (\rep -> rList (tree rep defValue))


The collect generic abstraction and the instance used in the test.

> flatten :: RepF CrushRight a b -> b -> [a]
> flatten tyConRep = crushr tyConRep (:) []


> flattenListTree :: [WTree a b] -> [a]
> flattenListTree = flatten (\rep -> rList (tree rep defValue))


