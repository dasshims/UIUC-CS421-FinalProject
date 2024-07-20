%----------------------------------------------------------------------------
%
%  Title       :  Nested.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  This test should check if a library can represent/handle
%                 nested datatypes. Uniplate can handle nested datatypes,
%                 however it can't implement generic equality. So this test
%                 fails undeserved.
%
%----------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

> module Nested where

> import PerfectDatatype
> import Data.Generics
> import GEq

> equalPerfect :: Perfect Int -> Perfect Int -> Bool
> equalPerfect =  error "Uniplate: generic equality cannot be implemented."
