%----------------------------------------------------------------------------
%
%  Title       :  Nested.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  This teste cheat separate compilation. The WTree data
%                 type is added to the repr. GADT.
%
%----------------------------------------------------------------------------

> {-#  OPTIONS_GHC -fglasgow-exts  #-}

> module RmWeights where

> import SYB1
> import TreeDatatype

Remove weights from a WTree. Note that the implementation
is fairly simple. Can be simplified much further using a
traversal (gmapT).

> rmWeights :: Typed a -> a

Here we handle the WithWeight constructor, we return the
subtree child and throw away the weights.
Note that this is achieved by matching |TreeWR| and |WithWeight|.

> rmWeights (r@(TreeWR ar wr) :> WithWeight t w)
>   = rmWeights (r :> t)

All other cases, includding other constructors of TreeWR
are handled generically by converting them to the Spine view.

> rmWeights typed = rmWeights' (toSpine typed)

This function does the generic work. It simply applies
|rmWeights| to all the arguments of a constructor.

> rmWeights' :: Spine a -> a
> rmWeights' (Con con) = constr con
> rmWeights' (spine :$ arg) = rmWeights' spine (rmWeights arg)

> rmWeightsWTree :: WTree Int Int -> WTree Int Int
> rmWeightsWTree t = rmWeights (TreeWR IntR IntR :> t)
