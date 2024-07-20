> import TreeDatatype
> import RmWeights(rmWeightsWTree)
> import Common(time)

Constructor cases
-----------------

This is for testing constructor cases, that is, can you write
an ad-hoc case to handle one constructor and then let the rest
be handled/traversed generically?

Why is this useful?
Suppose you want to rewrite "x+0" to "x" in a big AST, you do not
want to program the traversal of other constructors, right?

Well, this removes "WithWeight" constructors handling the others
generically.

Note that cheating ad-hoc cases for datatypes are accepted for
LIGD and Spine. Why? Because you may want to pay the price for
cheating but still get the genericity for most constructors not
handled explicitly.

> main = print $ (rmWeightsWTree mytree4,rmWeightsWTree mytree3)

> depth2 = 13
> eff   = do let t = bigwtree depth2
>            t1 <- time (deepSeqWTree t ())
>            t2 <- time (deepSeqWTree (rmWeightsWTree t) ())
>            putStrLn (show t2 ++ "; " ++ show depth2)
