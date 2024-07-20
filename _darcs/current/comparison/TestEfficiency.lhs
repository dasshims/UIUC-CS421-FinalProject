> import Efficiency(bigeq)
> import TreeDatatype
> import TreeDatatype(bigwtree, deepSeqWTree)
> import FoldTree (selectIntWTree)
> import Common(time)

----------------------------------------------------------------------------------

depth = 21
main' =do before <- getCPUTime
          let answer = bigeq depth
          seq answer (return ())
          after <- getCPUTime
          let diffInMilliseconds = (after - before) `div` 1000000000
          putStrLn (show diffInMilliseconds ++ "; " ++ show depth)
          print answer


> depth2 = 11
> main = do let t = bigwtree depth2
>           t1 <- time (deepSeqWTree t ())
>           t2 <- time (length (selectIntWTree t))
>           putStrLn (show t1 ++ "; " ++ show t2 ++ "; " ++ show depth2)
