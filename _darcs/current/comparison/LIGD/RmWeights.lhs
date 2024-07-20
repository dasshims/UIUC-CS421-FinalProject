> module RmWeights where

> import LIGD
> import TreeDatatype
> import TreeReps

Below we tried to define the rmWeights function using the everywhere
combinator but we did not succeed. The reason is that while withWeightCase
needs a representation with a constructor for RWTree, everywhere prefers
an RType constructor to encode a WTree, so that it can traverse it
generically. The solution would be to be able to convert a RWTree to
an RType, a bit like toSpine in the Spine case. Instead we use the
more verbose shown in the paper (no traversal combinator).

    import GMapQ

    withWeightCase :: Rep tT -> tT -> tT
    withWeightCase (RWTree _ _ ep) x
      = case from ep x of
        WithWeight t w -> to ep t
        _              -> x
    
    rmWeightsListWTree :: [WTree Int Int] -> [WTree Int Int]
    rmWeightsListWTree = everywhere withWeightCase undefined

So here is the real function:

> rmWeights :: Rep tT -> tT -> tT

First the code that does the traversal (what everywhere would do above)

> rmWeights (RCon  s rA) t
>            = rmWeights rA t
> rmWeights (RType e rA   ep) t 
>            = to ep (rmWeights rA (from ep t))
> rmWeights (RSum rA rB   ep) t 
>            =  case from ep t of
>               Inl a -> (to ep . Inl) (rmWeights rA a)
>               Inr b -> (to ep . Inr) (rmWeights rB b)
> rmWeights (RPair rA rB  ep) t 
>            =  case from ep t of
>               (a :*: b) -> (\x y -> to ep (x:*:y))
>                            (rmWeights rA a)
>                            (rmWeights rB b)

The ad-hoc case for RWTree which consists of two parts

> rmWeights r@(RWTree rA rW ep) t
>            =  case from ep t of

Ad-hoc case for WithWeight

>               WithWeight t w -> to ep (rmWeights (rTreeAdHoc rA rW) t)

Generic handling of other constructors

>               t              -> to ep (rmWeights (rTreeRec rA rW) t)

The other cases leave the tree unchanged

> rmWeights r t = t



> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree = rmWeights (rTreeAdHoc rInt rInt)
