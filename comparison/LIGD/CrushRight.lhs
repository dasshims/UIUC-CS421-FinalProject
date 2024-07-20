{-# OPTIONS_GHC -fglasgow-exts #-}

> module CrushRight where

> import LIGD
> import BinTreeDatatype
> import BinTreeReps
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps

Generic crush combines the 'c' elements of a type constructor using
the 'combine' function given by the user.

The arguments of CrushRight are:
  * a : type that indexes the function
  * x : used for arity two functions, like map. It is not used here.
  * c : parametric type of the elements in the structre that are to be combined.
  * b : result of combination.

> newtype CrushRight b a x = CrushRight { crushRight :: a -> b -> b }


> crushRightG :: Rep2 (CrushRight b) a x -> a -> b -> b
> crushRightG (RVar2 f) t b = crushRight f t b
> crushRightG (RSum2 rA rB ep _) t b =
>              case from ep t of
>                Inl t' -> crushRightG rA t' b
>                Inr t' -> crushRightG rB t' b
> crushRightG (RPair2 rA rB ep _) t b
>     = case from ep t of
>       x :*: y -> crushRightG rA x
>                              (crushRightG rB y b)
> crushRightG (RType2 e rA ep _) t b
>     = crushRightG rA (from ep t) b
> crushRightG (RCon2 nm rA) t b
>     = crushRightG rA t b
> crushRightG _ _ b = b

A crushRight for types |T| with kind *->*, these types
are represented by the type |RepF f a b| where |a| represents
the argument type and |b| represents the type constructor
applied to |a|. In short, |b === T a|.
For example, the representation of lists is |RepF f a [a] z|

We do not use second arities here, so the second arity variable
is made equal to the first.

Note that the type corresponding to the second arity is "ignored".

> type RepF f a b  = forall c . Rep2 (f c) a a -> Rep2 (f c) b b

> crushr :: RepF CrushRight a b -> (a -> d -> d) -> d -> b -> d
> crushr tyConRep op b x = crushRightG rep x b
>   where
>     -- the a-values in the b-structure are combined with the accumulated b
>     rep = tyConRep (RVar2 (CrushRight op))

Function to use with parametric parameters that we ignore


> defValue :: Rep2 (CrushRight c ) a a
> defValue = RVar2 (CrushRight (\_ b -> b))

--------


A polymorphic count function that is parametrised on an operation
to perform on the 'a' elements of the structure.

> countG :: RepF CrushRight a b -> (a -> Int) -> b -> Int
> countG tyConRep f = crushr tyConRep ((+) . f) 0

--------

The generic size function: each occurrence of an element yields 1

> sizeG :: RepF CrushRight a b -> b -> Int
> sizeG tyConRep = countG tyConRep (const 1)

--------

The sum function yields the element itself, it constraints the type of the element
to Int.

> sumG :: RepF CrushRight Int b -> b -> Int
> sumG tyConRep = countG tyConRep id

--------

The functions below are instances of the generic functions that are used
in the test driver.

> sizeListTree :: [WTree a b] -> Int
> sizeListTree = sizeG (\rep -> rList2 (rTree2 rep defValue))

> sumListTree :: [WTree Int b] -> Int
> sumListTree = sumG (\rep -> rList2 (rTree2 rep defValue))

--------

The flatten generic abstraction and the instance used in the test.

> flatten :: RepF CrushRight a b -> b -> [a]
> flatten tyConRep = crushr tyConRep (:) []

> flattenListTree :: [WTree a b] -> [a]
> flattenListTree = flatten (\rep -> rList2 (rTree2 rep defValue))
