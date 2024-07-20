{-# OPTIONS_GHC -fglasgow-exts #-}

module GADT where

data Expr :: * -> * where
  Num   ::  Int -> Expr Int
  Plus  ::  Expr Int -> Expr Int -> Expr Int
  Eq    ::  Expr Int -> Expr Int -> Expr Bool
  If    ::  forall a. Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (Num n)           =  n
eval (Plus x y)        =  eval x + eval y
eval (Eq x y)          =  eval x == eval y
eval (If c this that)  =  if eval c then eval this else eval that

expr :: Expr Int
expr = Num 1 `Plus` If (Num 2 `Eq` Num 3) (Num 4) (Num 5)
