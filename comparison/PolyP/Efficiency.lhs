> module Efficiency where
> import PolyLib.Prelude
> import PolyLib.Equal(pequal)
> import Fan(gfulltree)
> -- import GEq()
> import BinTreeRep()
> import BinTreeDatatype

> bigeq :: Int -> Bool
> bigeq n = equalBinTreeChar t t
>   where t = fulltree n '*'

> fulltree :: Int -> a -> BinTree a
> fulltree d a = gfulltree (const a) d

fulltree 0     a = Leaf a
fulltree (n+1) a = Bin t t
  where t = fulltree n a

> equalBinTree      :: Eq a => BinTree a -> BinTree a -> Bool
> equalBinTree      = pequal (==)

> equalBinTreeChar  :: BinTree Char -> BinTree Char -> Bool
> equalBinTreeChar  = equalBinTree
