> module Common where

> import Data.List (intersperse)
> import TreeDatatype
> import Control.Applicative
> import System.CPUTime(getCPUTime)

Common useful code for all library approaches.

> showSList :: (a -> ShowS) -> [a] -> ShowS
> showSList f xs = showChar '[' . (foldr (.) id $
>                                  intersperse (showChar ',')
>                                  (map f xs)) .
>                                 showChar ']'

> showList :: (a -> String) -> [a] -> String
> showList f xs = showSList (\x ss -> f x ++ ss) xs ""

> removeWeights :: WTree a w -> WTree a w
> removeWeights (Leaf a)         = Leaf a
> removeWeights (Fork t1 t2)     = Fork (removeWeights t1) (removeWeights t2)
> removeWeights (WithWeight t w) = removeWeights t

> newtype Id a = Id { unId :: a } --absent from GHC libs?

> instance Applicative Id where
>   pure = Id
>   (Id f) <*> (Id x) = Id (f x)

> instance Functor Id where
>   fmap f (Id x) = Id (f x)

> paren :: Bool -> String -> String
> paren cond str = if not cond then str else "(" ++ str ++ ")"

> time e = do before <- getCPUTime
>             seq e (return ())
>             after <- getCPUTime
>             let diffInMilliseconds = (after - before) `div` 1000000000
>             return diffInMilliseconds

time e = liftM2 subtract getCPUTime (m >> getCPUTime)   
