%----------------------------------------------------------------------------
%
%  Title       :  GShow.lhs
%  Author(s)   :  Alexey Rodriguez, Patrik Jansson, Alex Gerdes
%  License     :  BSD
%  Created     :  5 March 2008
%
%  Remarks     :  These tests cheat separate compilation. Several datatypes
%                 are added to the type rep., so recompilation is required
%
%----------------------------------------------------------------------------

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GShow (gshowsCompany, gapplyShowList) where

> import Prelude hiding (show)
> import SYB1
> import CompanyDatatypes(Company)
> import GMapQ(gmapQ)

> gshowsCompany :: Company -> String
> gshowsCompany = show . (CompanyR :>)

> gapplyShowList :: [Int] -> [String]
> gapplyShowList = gmapQ show (ListR IntR)

