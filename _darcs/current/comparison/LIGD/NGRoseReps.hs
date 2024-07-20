{-# OPTIONS_GHC -fglasgow-exts #-}
module NGRoseReps where

import LIGD
import NGRoseDatatype

rNGRose :: (forall a.Rep a -> Rep (f a)) -> Rep a -> Rep (NGRose f a)
rNGRose rf ra
    = RType (App "NGRose" [terms2s rf,term ra])
            (RCon "NGRose" (rPair ra (rf (rNGRose (rComp rf rf)
                                     ra))))
            (EP fromNGRose toNGRose)

fromNGRose (NGRose x trees) = x :*: trees

toNGRose   (x :*: trees)   = NGRose x trees
{-
rFork :: Rep a -> Rep (Fork a)
rFork ra
    = RType (App "Fork" [term ra])
            (RCon "Fork" (rPair ra ra))
            (EP fromFork toFork)


fromFork (Fork x y) = x :*: y
toFork (x :*: y) = Fork x y
-}
rComp :: (forall a.Rep a -> Rep (f a))
      -> (forall a.Rep a -> Rep (g a))
      -> (forall a.Rep a -> Rep (Comp f g a))
rComp rf rg ra
    = RType (App "Comp" [terms2s rf,terms2s rg,term ra])
            (RCon "Comp" (rf (rg ra)))
            (EP fromComp toComp)

-- Extract the head of the f term, exploiting that
-- term application is syntactic (no reduction or type abstraction), see LIGD paper
terms2s :: (forall a.Rep a -> Rep (f a)) -> Term
terms2s r = case term (r rInt) of App t ts -> App t (init ts)

fromComp (Comp x) = x
toComp x = Comp x

