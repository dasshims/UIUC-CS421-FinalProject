%----------------------------------------------------------------------------
%
%  Title       :  GEq.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  1 April 2008
%
%  Remarks     :  
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

> module FullTree where

> import RepLib hiding (generate, Generate, GenerateD, generateR1, generateD)
> import BinTreeDatatype

> $(derive [''BinTree])

> genBinTree :: Int -> [BinTree Char]
> genBinTree = generate
> 
> genList :: Int -> [String]
> genList = reverse . generate
>   -- probably arising from order of cons in rep.


> data GenerateD a = GenerateD { generateD :: Int -> [a] }

> -- | Generate elements of a type up to a certain depth
> class Rep1 GenerateD a => Generate a where
>   generate :: Int -> [a]
>   generate = generateR1 rep1
 
> instance Generate a => Sat (GenerateD a) where
>   dict = GenerateD generate
 
> genEnum :: (Enum a) => Int -> [a]
> genEnum d = enumFromTo (toEnum 0) (toEnum d)

> generateR1 :: R1 GenerateD a -> Int -> [a]
> generateR1 _ 0 = []
> generateR1 Int1  d = genEnum d
> generateR1 Char1 d = ['0']
> generateR1 Integer1 d = genEnum d
> generateR1 Float1 d = genEnum d
> generateR1 Double1 d = genEnum d
> generateR1 (Data1 dt cons) d =
>   [ to emb l | (Con emb rec) <- cons,
>                l <- fromTupM (\x -> generateD x (d-1)) rec]

> instance Generate Int
> instance Generate Char
> instance Generate Integer
> instance Generate Float
> instance Generate Double

> instance Generate ()
> instance (Generate a, Generate b) => Generate (a,b)
> instance Generate a => Generate [a]

> instance Generate a => Generate (BinTree a)

