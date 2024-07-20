{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}


-- Overriding the traversal for a particular instance of a strategy
-- Here, we import gshowsCompany and override the showing of arrays

module GShowExt (gshowsCompany) where

import Syb4A
import GShow hiding (gshowsCompany)
import qualified GShow (gshowsCompany)
import Data.List (intersperse)

import CompanyDatatypes
import CompanyDats

instance (GAPP (TL_red_ctr ShowR) spec a ShowR ShowR)
    => LDat (TL_red_ctr ShowR) spec [a] ShowR where
    gin tlab@(TL_red_ctr f) spec xs     = 
     ShowR .
     (\s -> showChar '[' . s . showChar ']') .
     (foldr (\(ShowR x) y -> x . y) id . intersperse (ShowR (showChar ',')))$
     map (gapp tlab spec) xs

gshowsCompany x = GShow.gshowsCompany x

test = gshowsCompany genCom
