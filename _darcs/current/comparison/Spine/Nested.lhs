%----------------------------------------------------------------------------
%
%  Title       :  Nested.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  This teste cheat separate compilation. The Perfect data
%                 type is added to the repr. GADT.
%
%----------------------------------------------------------------------------

> {-#  OPTIONS_GHC -fglasgow-exts  #-}

> module Nested where

> import SYB1 hiding (Perfect, Zero, Succ)
> import GEq
> import PerfectDatatype

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect x y = equal (PPerfectR IntR :> x) (PPerfectR IntR :> y)


 collectPerfect = collectPerfect' . (PPerfectR IntR :>)

 collectPerfect' :: Typed a -> [Int]
 collectPerfect' (PPerfectR a :> Zero x) = collectPerfect' (a :> x)
 collectPerfect' (PPerfectR a :> Succ x) = collectPerfect' (PPerfectR (ForkR a) :> x) 
 collectPerfect' (ForkR a :> Fork l r)   = collectPerfect' (a :> l) ++ collectPerfect' (a :> r)
 collectPerfect' (IntR :> x) = [x]
