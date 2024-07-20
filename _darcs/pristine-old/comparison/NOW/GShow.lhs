{-# OPTIONS -fglasgow-exts #-}

> module GShow (gshowsCompany) where

> import Prelude hiding (show)
> import NOW
> import CompanyDatatypes

> gshowsCompany :: Company -> String
> gshowsCompany = show . (CompanyR :>)
