> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

{----------------------------------------------------------------------------

 Module      :  Reduce
 Author      :  Alex Gerdes (agerdes@mac.com)
                Alexey Rodriguez (mrchebas@gmail.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test implements the reduce function.

----------------------------------------------------------------------------}

> module CrushRight where

> import RepLib hiding (flatten)
> import TreeDatatype
> import Language.Haskell.TH

> $(derive [''WTree])

> data CrushRight d a = CrushRight { crushRight :: a -> d -> d }
>
> crushRightR1 :: R1 (CrushRight d) a -> a -> d -> d
> crushRightR1 (Arrow1 r1 r2)   f b  = error "urk"
> crushRightR1 (Data1 dt cons)  x b  =
>   case (findCon cons x) of
>       Val emb rec kids ->
>         foldr_l (\ca field accum -> crushRight ca field accum) b rec kids
> crushRightR1 _                x b  = b
>

Instantiations for datatypes with one argument

> class CR1 t where
>   crushRight1 :: Rep a
>               => (a   -> b -> b)
>               -> (t a -> b -> b)

> instance CR1 [] where
>   crushRight1 op
>     = crushRightR1 (rList1 (CrushRight op) (CrushRight (crushRight1 op)))

Instantiations for datatypes with two arguments

> class CR2 t where
>   crushRight2 :: (Rep a,Rep c)
>               => (a     -> b -> b)
>               -> (c     -> b -> b)
>               -> (t a c -> b -> b)


> class Rep1 (CrushRight d) a => CR d a where
>   cr :: a -> d -> d
>   cr = crushRightR1 rep1

> instance CR2 WTree where
>   crushRight2 op1 op2
>      = crushRightR1 (rWTree1 (CrushRight op1)
>                              (CrushRight (crushRight2 op1 op2))
>                              (CrushRight op2))

Now come the instances

> crush' :: (CR1 t, Rep a) => (a -> b -> b) -> (b -> t a -> b)
> crush' = flip . crushRight1

> flattenListTree :: (Rep a, Rep w) => [WTree a w] -> [a]
> flattenListTree = crush' (crushRight2 (:) (const id)) []

> sizeListTree :: (Rep a,Rep w) => [WTree a w] -> Int
> sizeListTree = crush' (crushRight2 (const (+1)) (const id)) 0 

> sumListTree :: (Rep w) => [WTree Int w] -> Int
> sumListTree = crush' (crushRight2 (+) (const id)) 0

Note that doing this instantiation is based on the type classes CRX
above. If one would want to do instantiation on a more complex type
expression, such as BinTree [WTree a w], where we are interested in
the a's, then we have to instantiate crushRight for each of the types,
that is lists, BinTree and WTree, and compose the resulting functions.
In this case, this does not pose a problem, but it could be more
inconvenient when the function is more complex.


