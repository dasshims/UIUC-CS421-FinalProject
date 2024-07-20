{-# OPTIONS -fglasgow-exts #-}

> module Reduce where

> import LIGD
> import BinTreeDatatype
> import BinTreeReps
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps

Generic reduce combines the 'c' elements of a type constructor using
the 'combine' function given by the user.

The first argument of GReduce is the type that indexes the function.
The second argument is a "non-generic" type variable, the parametric type
of the combined element.

We don't use the second type argument (only used in functions such as map).
We just silently drop it.

> newtype GReduce a x c = GReduce { gReduce :: (c -> c -> c) -> c -> a -> c }


> reduceG :: Rep2 GReduce a x c -> GReduce a x c

> reduceG (RVar2 f) = f
> reduceG (RSum2 rA rB ep _)
>   = GReduce (\ op b t ->
>              case from ep t of
>                Inl t' -> gReduce (reduceG rA) op b t'
>                Inr t' -> gReduce (reduceG rB) op b t'
>             )
> reduceG (RPair2 rA rB ep _)
>   = GReduce (\ op b t ->
>              case from ep t of
>                x :*: y -> gReduce (reduceG rA) op b x
>                           `op`
>                           gReduce (reduceG rB) op b y
>             )
> reduceG (RType2 e rA ep _)
>   = GReduce (\ op b t -> gReduce (reduceG rA) op b (from ep t))
> reduceG (RCon2 nm rA)
>   = GReduce (\ op b t -> gReduce (reduceG rA) op b t)
> reduceG _ = GReduce (\ op b _ -> b)

The following three functions are generic abstractions

A polymorphic count function that is parametrised on an operation
to perform on the 'a' elements of the structure.

> countG :: (Rep2 GReduce a x Int -> Rep2 GReduce b y Int) -> (a -> Int) -> b -> Int
> countG rep f = gReduce (reduceG $ rep $ RVar2 $ GReduce (\_ _ i-> f i)) (+) 0

The generic size function: each occurrence of an element yields 1

> sizeG :: (Rep2 GReduce a x Int -> Rep2 GReduce b y Int) -> b -> Int
> sizeG rep = countG rep (const 1)

The sum function yields the element itself, it constraints the type of the element
to Int.

> sumG :: (Rep2 GReduce Int x Int -> Rep2 GReduce b y Int) -> b -> Int
> sumG rep = countG rep id

The functions below are instances of the generic functions that are used
in the test driver.

> sizeListTree :: [Tree a b] -> Int
> sizeListTree = sizeG (\rep -> rList2 (rTree2 rep (defValue (0::Int))))

> sumListTree :: [Tree Int b] -> Int
> sumListTree = sumG (\rep -> rList2 (rTree2 rep (defValue 0)))

> defValue v = RVar2 (GReduce (\_ _ _ -> v))

The collect generic abstraction and the instance used in the test.

> collectG :: (forall c.Rep2 GReduce a x [c] -> Rep2 GReduce b y [c]) -> b -> [a]
> collectG rep = gReduce (reduceG $ rep $ RVar2 $ GReduce (\_ _ i -> [i])) (++) []

> collectListTree :: [Tree a b] -> [a]
> collectListTree = collectG (\rep -> rList2 (rTree2 rep (defValue [])))
