\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module FoldTree where

import Generics.MultiRec
import CompanyDatatypes
import CompanyReps
import TreeDatatype
import TreeReps
import Control.Monad
import Control.Monad.Writer

selectSalary :: Company -> [Salary]
selectSalary = snd . runWriter . go Company
  where
    go :: Ix CompanyU a => CompanyU a -> a -> Writer [Salary] a
    go Salary s = tell [s] >> return s
    go _      x = composM go x

data Const x y = Const x

selectIntWTree :: WTree Int Int -> [Int]
selectIntWTree t =
    let collectAlgebra :: Algebra (WTreeU Int Int) (Const [Int])
        collectAlgebra _ = tag (\(K i) -> Const [i])
                         & tag (\(I (Const l) :*: I (Const r)) -> Const (l ++ r))
                         & tag (\(I (Const t) :*: K w) -> Const (t ++ [w]))
        (Const is) = fold collectAlgebra t
    in is
\end{code}
