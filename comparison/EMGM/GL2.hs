--{-# OPTIONS_GHC -fglasgow-exts -fgenerics -fallow-undecidable-instances #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}


 module GL2 where

 import Data.Generics hiding (Generic)

 class Generic g where
    unit        :: g Unit Unit
    char        :: g Char Char
    int         :: g Int Int
    plus        :: g a1 a2 -> g b1 b2 -> g (a1 :+: b1) (a2 :+: b2)
    prod        :: g a1 a2 -> g b1 b2 -> g (a1 :*: b1) (a2 :*: b2)
    view        :: Iso b1 a1 -> Iso b2 a2 -> g a1 a2 -> g b1 b2
    float       :: g Float Float

 data Iso a b = Iso {from :: a -> b, to :: b -> a}

 type Name   = String
 type Arity  = Int

 outl (x :*: y) = x
 outr (x :*: y) = y

 infixr 8 <*>
 infixr 7 <|>

 (<|>)  :: Generic g => g a1 a2 -> g b1 b2 -> g (a1 :+: b1) (a2 :+: b2)
 (<|>)  = plus

 (<*>)  :: Generic g => g a1 a2 -> g b1 b2 -> g (a1 :*: b1) (a2 :*: b2)
 (<*>)  = prod

 class FunctorRep f where
      functorRep  :: Generic g => g a1 a2 -> g (f a1) (f a2)

 class Over t where
    over :: t
 instance Generic g => Over (g Unit Unit) where
    over = unit
 instance Generic g => Over (g Int Int) where
    over = int
 instance Generic g => Over (g Char Char) where
    over = char
 instance (Generic g, Over (g a1 a2), Over (g b1 b2)) => Over (g (a1 :+: b1) (a2:+: b2)) where
    over = plus over over
 instance (Generic g, Over (g a1 a2), Over (g b1 b2)) => Over (g (a1 :*: b1) (a2 :*: b2)) where
    over = prod over over

-- Typereps

-- list
 isoList :: Iso [a] (Unit :+: (a :*: [a]))
 isoList = Iso fromList toList

 fromList :: [a] -> Unit :+: (a :*: [a])
 fromList []               = Inl Unit
 fromList (x:xs)           = Inr (x :*: xs)

 toList :: Unit :+: (a :*: [a]) -> [a]
 toList (Inl Unit)        = []
 toList (Inr (x :*: xs))  = x : xs

 instance FunctorRep [] where
   functorRep   =  rList

 rList :: Generic g => g a1 a2 -> g [a1] [a2]
 rList a = view isoList isoList (unit <|> (a <*> rList a))
