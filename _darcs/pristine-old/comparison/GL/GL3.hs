{-# OPTIONS -fglasgow-exts #-}

{- In this module a Generic class support up-to 3 geric type arguments is developed.
This combines the ideas in Section 4 of the JFP version of GM with the extensibility 
provided by GL. 

-}

 module GL3 where

 import Data.Generics hiding (Generic)

 class Generic g where
    unit        :: g Unit Unit Unit
    char        :: g Char Char Char
    int         :: g Int Int Int
    constr      :: Name -> Arity -> g a b c -> g a b c
    constr _ _  = id
    plus        :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :+: b1) (a2 :+: b2) (a3 :+: b3)
    prod        :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :*: b1) (a2 :*: b2) (a3 :*: b3)
    view        :: Iso b1 a1 -> Iso b2 a2 -> Iso b3 a3 -> g a1 a2 a3 -> g b1 b2 b3 
    float       :: g Float Float Float

 data Iso a b = Iso {from :: a -> b, to :: b -> a}

 type Name   = String
 type Arity  = Int

 outl (x :*: y) = x
 outr (x :*: y) = y

 infixr 8 <*>
 infixr 7 <|>

 (<|>)  :: Generic g => g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :+: b1) (a2 :+: b2) (a3 :+: b3)
 (<|>)  = plus

 (<*>)  :: Generic g => g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :*: b1) (a2 :*: b2) (a3 :*: b3)
 (<*>)  = prod

 class FunctorRep g f where
      functorRep  :: g a1 a2 a3 -> g (f a1) (f a2) (f a3)

 class GRep g a where
    over :: g a a a
 instance Generic g => GRep g Unit where
    over = unit
 instance Generic g => GRep g Int where
    over = int
 instance Generic g => GRep g Char where
    over = char
 instance (Generic g, GRep g a, GRep g b) => GRep g (a :+: b) where
    over = plus over over
 instance (Generic g, GRep g a, GRep g b) => GRep g (a :*: b) where
    over = prod over over
 instance (Generic g, GRep g a) => GRep g [a] where
    over = functorRep over

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

 instance Generic g => FunctorRep g [] where
   functorRep   =  rList

 rList :: Generic g => g a1 a2 a3 -> g [a1] [a2] [a3]
 rList a = view isoList isoList isoList (unit <|> (a <*> rList a))

