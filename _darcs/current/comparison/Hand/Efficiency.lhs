> module Efficiency where
> import BinTreeDatatype


> bigeq :: Int -> Bool
> bigeq n = equalBinTreeChar t t
>   where t = fulltree n '*'

> fulltree :: Int -> a -> BinTree a
> fulltree 0     a = Leaf a
> fulltree (n+1) a = Bin t t
>   where t = fulltree n a

> equalBinTreeChar  :: BinTree Char -> BinTree Char -> Bool
> equalBinTreeChar (Leaf x)  (Leaf y)    = x == y
> equalBinTreeChar (Bin l r) (Bin l' r') = equalBinTreeChar l l' && equalBinTreeChar r r'
