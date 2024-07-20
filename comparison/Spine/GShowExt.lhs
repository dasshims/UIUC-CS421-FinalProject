%----------------------------------------------------------------------------
%
%  Title       :  GShowExt.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  Support for extension in Spine only works for functions 
%                 with an untied recursive knot (not allowed for the test: 
%                 big burden for users). Unclear how to handle the list 
%                 extension
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GShowExt (gshowsCompany) where

> import Prelude hiding (show, shows)
> import SYB1 -- cheating
> import CompanyDatatypes

> gshowsCompany :: Company -> String
> gshowsCompany = error "Support for extension in SYB only works for functions with an untied recursive knot (not allowed for the test: big burden for users)."

