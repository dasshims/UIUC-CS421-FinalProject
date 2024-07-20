> module Efficiency where
> import SYB1
> import GEq
> import BinTreeDatatype

import BinTreeReps -- The representation has been added to the core lib.

> bigeq :: Int -> Bool
> bigeq n = equalBinTreeChar t t
>   where t = fulltree n '*'

> fulltree :: Int -> a -> BinTree a
> fulltree 0     a = Leaf a
> fulltree (n+1) a = Bin t t
>   where t = fulltree n a

fulltree r = gFullTree (rBinTree r)

> equalBinTreeChar  :: BinTree Char -> BinTree Char -> Bool
> equalBinTreeChar x y = equal (BinTreeR CharR :> x) (BinTreeR CharR :> y)
