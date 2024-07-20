{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

-- The organisational structure of a company

module TreeReps where

import RepLib hiding (rUnit)
import TreeDatatype
import Language.Haskell.TH hiding (Name)
import R2


$(derive
    [''WTree
    ])


rWTree2 :: ctx (WTree a1 w1) (WTree a2 w2)
                              -> ctx a1 a2
                              -> ctx w1 w2
                              -> R2 ctx (WTree a1 w1) (WTree a2 w2)
rWTree2 t a w = Data2 "BinTree" [Con2 rLeafEmb rLeafEmb (a :**: MNil2),
                                 Con2 rForkEmb rForkEmb (t :**: t :**: MNil2),
                                 Con2 rWithWeightEmb rWithWeightEmb (t :**: w :**: MNil2)
                                ]
rLeafEmb :: Emb (a :*: Nil) (WTree a w)
rLeafEmb = Emb { to   = \ (a :*: Nil) -> (Leaf a),
                 from = \ x -> case x of
                               Leaf a -> Just (a :*: Nil)
                               _      -> Nothing }
rForkEmb :: Emb (WTree a w :*: WTree a w :*: Nil) (WTree a w)
rForkEmb = Emb { to   = \ (l :*: r :*: Nil) -> (Fork l r),
                from = \ x -> case x of
                              Fork l r -> Just (l :*: r :*: Nil)
                              _       -> Nothing}


rWithWeightEmb :: Emb (WTree a w :*: w :*: Nil) (WTree a w)
rWithWeightEmb = Emb { to   = \ (t :*: w :*: Nil) -> (WithWeight t w),
                       from = \ x -> case x of
                                     WithWeight t w -> Just (t :*: w :*: Nil)
                                     _              -> Nothing}


