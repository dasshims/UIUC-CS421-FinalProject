> module FoldTree where
> import TreeDatatype
> import CompanyDatatypes
> import qualified PerfectDatatype as P

> selectIntWTree :: WTree Int Int -> [Int]
> selectIntWTree = loop []
>   where
>     loop acc (Leaf x) = x : acc
>     loop acc (Fork l r) = loop (loop acc r) l
>     loop acc (WithWeight t w) = loop (w : acc) t

