%----------------------------------------------------------------------------
%
%  Title       :  GShow.lhs
%  Author(s)   :  Patrik Jansson
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  -
%
%----------------------------------------------------------------------------

> module GShow where
> import CompanyDatatypes
> import BinTreeDatatype

> eMsg = error "Uniplate: GShow cannot be implemented"

> gshowsCompany :: Company -> String
> gshowsCompany = eMsg

> gapplyShowBinTree :: BinTree Int -> [String]
> gapplyShowBinTree = eMsg

> gapplyShowList    :: [Int] -> [String]
> gapplyShowList    =  eMsg
