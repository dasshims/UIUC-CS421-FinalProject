{-# OPTIONS #-}

module Main where

import LIGD

-- A parameterised datatype for binary trees with data at the leafs
data Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w  

-- Representation for the tree

rTree :: Rep a -> Rep b -> Rep (Tree a b)
rTree ra rb = RType (App "Tree" [term ra, term rb])
                    (rSum (RCon "Leaf" ra)
                          (rSum (RCon "Fork" (rPair (rTree ra rb) (rTree ra rb)))
								        (RCon "WithWeight" (rPair (rTree ra rb) rb))))
						  (EP fromTree toTree)

fromTree (Leaf a) = Inl a
fromTree (Fork a b) = Inr (Inl (a :*: b))
fromTree (WithWeight a b) = Inr (Inr (a :*: b))

toTree (Inl a) = Leaf a
toTree (Inr (Inl (a :*: b))) = Fork a b
toTree (Inr (Inr (a :*: b))) = WithWeight a b

-- A typical tree
mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

-- accumulate all ints in a datastructure
-- just a simple type-indexed function. Not protected by a type class.
listifyInt :: Rep a -> a -> [Int]
listifyInt (RInt ep) i = [from ep i]
listifyInt (RSum rA rB ep) t = case from ep t of 
											Inl a -> listifyInt rA a
											Inr b -> listifyInt rB b
listifyInt (RPair rA rB ep) t = case from ep t of 
											(a :*: b) -> listifyInt rA a ++ listifyInt rB b 
listifyInt (RType e rA ep) t = listifyInt rA (from ep t)
listifyInt (RCon s rA)   t = listifyInt rA t
listifyInt _             t = []


listifyLeaf :: Rep a -> a -> [Int]
listifyLeaf (RCon "Leaf" rA) t = listifyInt rA t
listifyLeaf (RSum rA rB ep) t = case from ep t of 
											Inl a -> listifyLeaf rA a
											Inr b -> listifyLeaf rB b
listifyLeaf (RPair rA rB ep) t = case from ep t of 
											(a :*: b) -> listifyLeaf rA a ++ listifyLeaf rB b 
listifyLeaf (RType e rA ep) t = listifyLeaf rA (from ep t)
listifyLeaf (RCon s rA)   t = listifyLeaf rA t
listifyLeaf _             t = []


-- Print everything like an Int in mytree
-- In fact, we show two attempts:
--   1. print really just everything like an Int
--   2. print everything wrapped with Leaf
-- So (1.) confuses leafs and weights whereas (2.) does not.
-- 

main = print $ ( listifyInt (rTree rInt rInt) mytree
               , listifyLeaf (rTree rInt rInt) mytree
               )

					 