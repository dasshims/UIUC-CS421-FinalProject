{-# OPTIONS_GHC -fglasgow-exts #-}
module GRoseReps where

import LIGD
import GRoseDatatype

rGRose :: (forall a.Rep a -> Rep (f a)) -> Rep a -> Rep (GRose f a)
rGRose rf ra
    = RType (App "GRose" [tf,term ra])
            (RCon "GRose" (rPair ra (rf (rGRose rf ra))))
            (EP fromGRose toGRose)
  -- Extract the head of the f term, exploiting that
  -- term application is syntactic (no reduction or type abstraction), see LIGD paper
             where
               tf = case rf ra of
                      RType (App tf _) _ _ -> App tf []

fromGRose (GRose x trees) = x :*: trees

toGRose   (x :*: trees)   = GRose x trees
