> {-# LANGUAGE UndecidableInstances #-}

> module FullTree where

> import Data.Generics.SYB.WithClass.Basics
> import BinTreeDatatype
> import BinTreeReps

> genBinTree :: Int -> [BinTree Char]
> genBinTree = genUpTo

> genList :: Int -> [[Char]]
> genList = genUpTo


> data GenUpToD a = GenUpToD { genUpToD :: Int -> [a] }

> class GenUpTo a where
>   genUpTo :: Int -> [a]

> instance GenUpTo a => Sat (GenUpToD a)
>   where dict = GenUpToD genUpTo

> instance Data GenUpToD a => GenUpTo a where
>     genUpTo d = genUpToG d

> genUpToCtx :: Proxy GenUpToD
> genUpToCtx = undefined

Based on the version given in SYB1_2

> genUpToG :: forall a. (Data GenUpToD a) => Int -> [a]
> genUpToG 0 = []
> genUpToG d = result
>   where
>     -- Getting hold of the result (type)
>     result = concat (map recurse cons')
>
>     cons :: [Constr]
>     cons = dataTypeConstrs (dataTypeOf genUpToCtx (undefined :: a))
>
>     recurse :: Constr -> [a]
>     recurse con = gmapM genUpToCtx
>                         (\_ -> genUpToD dict (d-1)) 
>                         (fromConstr genUpToCtx con)
>     -- We could also deal with primitive types easily.
>     -- Then we had to use cons' instead of cons.
>     --
>     cons' :: [Constr]
>     cons' = case dataTypeRep ty of
>              AlgRep cons -> cons
>              IntRep      -> [mkIntConstr ty 0]
>              FloatRep    -> [mkIntConstr ty 0]
>              StringRep   -> [mkStringConstr ty "0"]
>      where
>        ty = dataTypeOf genUpToCtx (undefined :: a)     
>



 
