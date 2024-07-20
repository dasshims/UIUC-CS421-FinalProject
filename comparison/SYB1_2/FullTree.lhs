%----------------------------------------------------------------------------
%
%  Title       :  FullTree.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  1 April 2008
%
%  Remarks     :  genUpTo is copied from the SYB website.
%
%----------------------------------------------------------------------------

> {-# OPTIONS -fglasgow-exts #-}

> module FullTree where

> import Data.Generics
> import BinTreeDatatype
> import BinTreeReps

> genBinTree :: Int -> [BinTree Char]
> genBinTree = genUpTo

> genList :: Int -> [[Char]]
> genList = genUpTo

> -- Generate all terms of a given depth
> genUpTo :: Data a => Int -> [a]
> genUpTo 0 = []
> genUpTo d = result
>    where
>      -- Getting hold of the result (type)
>      result = concat (map recurse cons')
> 
>      -- Retrieve constructors of the requested type
>      cons :: [Constr]
>      cons = dataTypeConstrs (dataTypeOf (head result))
> 
>      -- Find all terms headed by a specific Constr
>      recurse :: Data a => Constr -> [a]
>      recurse con = gmapM (\_ -> genUpTo (d-1)) 
>                          (fromConstr con)

>      -- We could also deal with primitive types easily.
>      -- Then we had to use cons' instead of cons.
>      --
>      cons' :: [Constr]
>      cons' = case dataTypeRep ty of
>               AlgRep cons -> cons
>               IntRep      -> [mkIntConstr ty 0]
>               FloatRep    -> [mkIntConstr ty 0]
>               StringRep   -> [mkStringConstr ty "0"]
>       where
>         ty = dataTypeOf (head result)     
> 



