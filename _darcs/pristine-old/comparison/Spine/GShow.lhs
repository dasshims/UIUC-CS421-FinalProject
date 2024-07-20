{-# OPTIONS -fglasgow-exts #-}

> module GShow (gshowsCompany) where

> import Prelude hiding (show)
> import SYB1
> import CompanyDatatypes

> gshowsCompany :: Company -> String
> gshowsCompany = show . (CompanyR :>)


Legacy
---

Weird, without -fglasgow-exts the first
equation of gshow does not type.

BUG?

 gshow :: Type a -> a -> String
 gshow Char c = ('\'':c:'\'':[])
 gshow rep x = gshow' (toSpine rep x) ""

