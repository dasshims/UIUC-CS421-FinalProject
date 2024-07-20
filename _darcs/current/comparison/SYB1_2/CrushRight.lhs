> module CrushRight where

> import Data.Generics
> import Debug.Trace

> import BinTreeDatatype
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes

Crush in Claus and Oleg's style
===============================


> -- The general SYB version of crush in Oleg and Claus style.
> crusht :: (Data a, Data x) => (a -> b -> b) -> x -> b -> Dyn -> b
> crusht combine val initial trep
>   = case cast val of
>       -- val has type a, and trep represents a hole (type X)
>       Just val_a | hasmark trep -> combine val_a initial
>       -- otherwise find the X's
>       _                         -> traverse (trep,val)
>   where
>    -- Check whether this type representation corresponds to the elements (the X things)
>    hasmark :: Dyn -> Bool
>    hasmark (Dyn x) = typeOf x == typeOf X
>    traverse (Dyn t,v) | typeOf t == typeOf v -- default case (no X marker)
>                       = initial
>    -- traverse in parallel with the type representation
>    traverse (Dyn t,x) | (tcon,tkids) <- splitTyConApp (typeOf t),
>                         (con,kids)   <- splitTyConApp (typeOf x),
>                         not (length tkids == length kids &&
>                              tcon == con)
>         = error $ unwords ["template type", show (typeOf t),
>                            "inconsistent with value type", show (typeOf x)]
>    traverse (Dyn t,x) = recurse (dynamize t1) xdyn
>      where
>        xdyn@(con,kids) = dynamize x
>        t1 = fromConstr con `asTypeOf` t
>    recurse (_tcon,tkids) (_con,kids) = foldr comb' initial (zip tkids kids)
>      where
>        comb' (trep,Dyn kid) res = crusht combine kid res trep

> data Dyn = forall a. Data a => Dyn a
> data Kids a = Kids{growUp:: [Dyn]}

> dynamize :: Data a => a -> (Constr,[Dyn])
> dynamize x = (toConstr x, growUp $ gfoldl f (const (Kids [])) x)
>   where f (Kids l) a = Kids (l ++ [Dyn a])

> castfn :: (Typeable a, Typeable b, Typeable c, Typeable d) =>
>   (a -> b) -> Maybe (c -> d)
> castfn f = cast f

> -- "X marks the spots";-) X should be private
> data X = X deriving (Data,Typeable)

> crushRight :: forall a b c.
>               (Data a, Data (c a), Data (c X)) =>
>               (a -> b -> b) -> b -> c a -> b
> crushRight combine initial cont = crusht combine cont initial (Dyn (undefined::c X))

> crushRight2 :: forall a b c d.
>                (Data a, Data (c a d), Data (c X d)) =>
>                (a -> b -> b) -> b -> c a d -> b
> crushRight2 combine initial cont = crusht combine cont initial (Dyn (undefined::c X d))

> crushListTree :: (Data a, Data w) => (a -> b -> b) -> b -> [WTree a w] -> b
> crushListTree 
>   = crushRight . flip . crushRight2

> flattenListTree :: forall a w.(Data a,Data w) => [WTree a w] -> [a]
> flattenListTree 
>   = crushListTree (:) []

> sizeListTree :: forall a w.(Data a,Data w) => [WTree a w] -> Int
> sizeListTree = length . flattenListTree

> sumListTree :: forall w.(Data w) => [WTree Int w] -> Int
> sumListTree = crushListTree (+) 0


