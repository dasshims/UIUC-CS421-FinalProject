%----------------------------------------------------------------------------
%
%  Title       :  FullTree.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  1 April 2008
%
%  Remarks     :  
%
%----------------------------------------------------------------------------

> {-#  OPTIONS_GHC -fglasgow-exts  #-}

> module FullTree where

> import SYB1
> import BinTreeDatatype

> datatype' :: Type a -> Datatype a
> datatype' (CharR)      =  [ Sig (char c) | c <- ['0'] ]
> datatype' (ListR a)    =  [ Sig nil, Sig cons :* a :* ListR a ]
> datatype' (BinTreeR a) =  [ Sig tBinleaf :* a, Sig tbin :* BinTreeR a :* BinTreeR a ]

> generate' :: Type a -> Int -> [a]
> generate' a  0              =  []
> generate' a  (d + 1)        =  concat [ generateSpine' s d | s <- datatype' a ]

> generateSpine' :: Signature a -> Int -> [a]
> generateSpine' (Sig c)   d  =  [constr c]
> generateSpine' (s :* a)  d  =  [ f x | f <- generateSpine' s d, x <- generate' a d ]

> genBinTree = generate' (BinTreeR CharR)
> genList = generate' (ListR CharR)