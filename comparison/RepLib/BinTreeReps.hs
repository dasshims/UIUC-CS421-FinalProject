{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

-- The organisational structure of a company

module BinTreeReps where

import RepLib hiding (rUnit)
import BinTreeDatatype
import Language.Haskell.TH hiding (Name)
import R2


$(derive
    [''BinTree
    ])

rBinTree2 :: forall a b c.c a b -> c (BinTree a) (BinTree b)
                                -> R2 c (BinTree a) (BinTree b)
rBinTree2 a t = Data2 "BinTree" [Con2 rLeafEmb rLeafEmb (a :**: MNil2),
                                 Con2 rBinEmb rBinEmb (t :**: t :**: MNil2)]
rLeafEmb :: Emb (a :*: Nil) (BinTree a)
rLeafEmb = Emb { to   = \ (a :*: Nil) -> (Leaf a),
                 from = \ x -> case x of
                          Leaf a -> Just (a :*: Nil)
                          _      -> Nothing }
rBinEmb :: Emb (BinTree a :*: BinTree a :*: Nil) (BinTree a)
rBinEmb = Emb { to   = \ (l :*: r :*: Nil) -> (Bin l r),
                from = \ x -> case x of
                         Bin l r -> Just (l :*: r :*: Nil)
                         _       -> Nothing}


