{-# OPTIONS -fglasgow-exts  #-} -- GHCi complains without it
                                -- when I import it...

module TreeDatatype where

import Data.Generics


-- A parameterised datatype for binary trees with data at the leafs
data -- (Data a, Data w) =>
     -- let the functions constrain the datatype
     Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w
       deriving (Typeable, Data, Show)


-- A typical tree
mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

-- and another
mytree2 :: Tree Int Int
mytree2 = Fork (Leaf 42)
               (WithWeight (Fork (Leaf 88) (Leaf 37)) 3)

-- yet one more
mytree3 :: Tree Int Int
mytree3 = Fork (WithWeight (Leaf 42) 1)
               (WithWeight (Leaf 88) 2)
