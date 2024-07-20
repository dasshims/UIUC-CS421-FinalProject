> {-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances #-}

{----------------------------------------------------------------------------

 Module      :  Reduce
 Author      :  Alex Gerdes (agerdes@mac.com)
 Copyright   :  (c) Open University Netherlands, 2007
 License     :  BSD

 This test implements the reduce function.

----------------------------------------------------------------------------}

> module Reduce where

> import RepLib
> import TreeDatatype
> import Language.Haskell.TH

> $(derive [''Tree])

> class Rep1 GColD a => GCol a where
>    gcol :: a -> [Int]
>    gcol = gcolR1 rep1
>
> data GColD a = GColD { gcolD :: a -> [Int] }
>
> gcolR1 :: R1 GColD a -> a -> [Int]
> gcolR1 (Arrow1 r1 r2)    f  = error "urk"
> gcolR1 (Data1 dt cons)   x  =
>   case (findCon cons x) of
>       Val emb rec kids ->
>         foldl_l (\ca a b -> a ++ (gcolD ca b)) [] rec kids
> gcolR1 _                 x  = []
>
> instance GCol a => Sat (GColD a) where
>    dict = GColD gcol
>
> instance GCol Float
> instance GCol Int where
>    gcol x = [x]
> instance GCol Integer where
>    gcol x = [(fromInteger x)]
> instance GCol Bool
> instance GCol ()
> instance GCol Char
> instance (GCol a, GCol b) => GCol (a,b)
> instance (GCol a) => GCol [a]
> instance (GCol a, GCol w) => GCol (Tree a w) where
>    gcol (Leaf a) = gcol a
>    gcol (Fork l r) = gcol l ++ gcol r
>    gcol (WithWeight t w) = gcol t

> collectListTree :: GCol t => t -> [Int]
> collectListTree = gcol

> sizeListTree :: GCol t => [t] -> Int
> sizeListTree = length . gcol

> sumListTree :: (GCol w) => [Tree Int w] -> Int
> sumListTree = sum . gcol
