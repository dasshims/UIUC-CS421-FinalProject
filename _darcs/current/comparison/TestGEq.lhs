> import BinTreeDatatype
> import GEq (equalBTreeInt)
> import Common(time)

GEqBTree tests:
 * Separate compilation.
 * Multiple arguments (equality has two).
 * Support for regular datatypes such as BinTree is
   assumed (or maybe eventually will be tested in TestGEqBTree)


----------------------------------------------------------------------------------

> main = print ( equalBTreeInt mytree mytree
>              , equalBTreeInt mytree mytree2
>             )

> depth2 = 13
> eff   = do let t  = bigbtree depth2
>            let t' = tweakRightmost (bigbtree (depth2-1+1))
>            t1  <- time (deepSeqBTree t ())
>            t1' <- time (deepSeqBTree t' ())
>            t2  <- time (equalBTreeInt t t')
>            putStrLn (show t2 ++ "; " ++ show depth2)
