> module FullTree where

> import LIGD
> import BinTreeReps
> import BinTreeDatatype


> genBinTree :: Int -> [BinTree Char]
> genBinTree = gGenerate (rBinTree rChar)

> genList :: Int -> [[Char]]
> genList = gGenerate (rList rChar)

> gGenerate :: Rep t -> Int -> [t]
> gGenerate (RInt         ep) s  =  map (to ep) $ consumeDepth [0] s
> gGenerate (RChar        ep) s  =  map (to ep) $ consumeDepth ['0'] s
> gGenerate (RUnit        ep) s  =  map (to ep) [Unit]
> gGenerate (RFloat       ep) s  =  map (to ep) $ consumeDepth [0.0] s
> gGenerate (RType e rA   ep) s  =  map (to ep) $ gGenerate rA s
> gGenerate (RPair rA rB  ep) s  =  map (to ep) [ l :*: r
>                                               | l <- gGenerate rA s
>                                               , r <- gGenerate rB s
>                                               ]
> gGenerate (RSum rA rB   ep) s  =  map (to ep) $ map Inl (gGenerate rA s) ++
>                                                 map Inr (gGenerate rB s)
> gGenerate (RCon n rA      ) 0  =  []
> gGenerate (RCon n rA      ) s  =  gGenerate rA (s - 1)

Primitive types consume one depth unit
This hack would not be needed with a list-like view.

> consumeDepth ls 0 = []
> consumeDepth ls _ = ls




