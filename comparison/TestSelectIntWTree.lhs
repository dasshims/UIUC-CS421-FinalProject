> import TreeDatatype
> import FoldTree (selectIntWTree)
> import Common(time)

selectInt is an instance for trees of a generic function
that collects all of the Int's in a value.

A variant of this function is selectSalary, which does the
collection of Salary values. It is tested separately in
TestSelectSalary because it tests ad-hoc definitions
in addition.

> main = do print ( selectIntWTree mytree
>                 )
> depth2 = 11
> eff   = do let t = bigwtree depth2
>            t1 <- time (deepSeqWTree t ())
>            t2 <- time (length (selectIntWTree t))
>            putStrLn (show t2 ++ "; " ++ show depth2)
