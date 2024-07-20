> {-# OPTIONS_GHC -fglasgow-exts #-}
> module BinTreeRep where
> import PolyLib.Prelude
> import BinTreeDatatype

> instance FunctorOf (SumF ParF (ProdF RecF RecF)) BinTree where
>   inn (InL (ParF a))            = Leaf a
>   inn (InR (RecF a :*: RecF b)) = Bin a b
>   out (Leaf a)        = InL (ParF a)
>   out (Bin a b)       = InR (RecF a :*: RecF b)
>   datatypeName _ = "BinTree"
>   constructorName (Leaf _)  = "Leaf"
>   constructorName (Bin _ _) = "Bin"
