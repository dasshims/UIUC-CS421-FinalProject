> {-#  OPTIONS_GHC -fglasgow-exts  #-}

> module Reduce where

> import NOW hiding (Zero, Succ)
> import TreeDatatype
> import qualified PerfectDatatype as PD
> import PerfectDatatype hiding (Fork)
 
> newtype Reduce c a = InReduce { outReduce :: a -> c}

> preduce :: PType (Reduce c) a -> (c -> c -> c) -> c -> a -> c
> preduce (PVar c) op b x                       =  outReduce c x
> preduce (PCharR) _ b _                        =  b
> preduce (PIntR)  _ b _                        =  b
> preduce (PPairR l r) op b (x, y)              =  preduce l op b x `op` preduce r op b y
> preduce (PListR a) op b []                    =  b 
> preduce (PListR a) op b (x:xs)                =  preduce a op b x `op` preduce (PListR a) op b xs 
> preduce (PTreeNOWR a) op b x                  =  preduce (PListR a) op b (inorder x)
> preduce (PWTreeR a w) op b (Leaf x)           =  preduce a op b x
> preduce (PWTreeR a w) op b (Fork l r)         =  preduce (PWTreeR a w) op b l `op` preduce (PWTreeR a w) op b r
> preduce (PWTreeR a w) op b (WithWeight t wt)  =  preduce (PWTreeR a w) op b t `op` preduce w op b wt 
> preduce (PPerfectR a) op b (Zero x)           =  preduce a op b x
> preduce (PPerfectR a) op b (Succ p)           =  preduce (PPerfectR (PForkR a)) op b p 
> preduce (PForkR a) op b (PD.Fork x y)         =  preduce a op b x `op` preduce a op b y
                                
> pcollect         :: (PType (Reduce [c]) c -> PType (Reduce [c]) a) -> a -> [c]
> pcollect rep     = preduce (rep a) (++) [] where a = PVar (InReduce (\x -> [x]))

> collectListTree  :: [WTree a w] -> [a]
> collectListTree  = pcollect (\ a -> PListR (PWTreeR a (PVar (InReduce (\x -> [])))))

> sizeListTree     :: [WTree a w] -> Int
> sizeListTree     = length . collectListTree

> sumListTree      :: [WTree Int w] -> Int
> sumListTree      = sum . collectListTree