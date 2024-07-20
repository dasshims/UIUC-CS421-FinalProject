> import Char (chr,ord)

> import GMap
> import BinTreeDatatype

> example1 = [1,2,7,3,4]
> example2 = [Leaf 1 `Bin` Leaf 7,Leaf 3 `Bin` Leaf 4]
> example3 = [Leaf [1] `Bin` Leaf [2,7],Leaf [3] `Bin` Leaf [4]]

> main = print ( mapList toChar example1
>              , mapListBTree toChar example2
>              , mapListBTreeList toChar example3
>              )
>   where
>     toChar i = chr (i + ord 'A')
