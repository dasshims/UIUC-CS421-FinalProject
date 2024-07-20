{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

-- we develop several versions of GShow

module GShow (gshowsCompany, gapplyShowList, gshowsCompanyDumb_arr,
	     gs2, ShowR(..)) where

import CompanyDatatypes
import CompanyDats

import Syb4A
import Data.List (intersperse)

-- Alas, the following gshow are _too_ advanced for the test suite
-- The test suites don't care for all that sophistication...
-- Dumbing down <sigh>

gsdumb spec a = gapp (TL_red_ctr shows_term) spec a
 where
 show_kids delim xs = (foldr (.) id . intersperse (showChar delim)) xs
 shows_term "[]" [] = showString "[]"
 shows_term ctor [] = shows ctor
 -- shows_term ":"  kids = show_arr kids
 shows_term ":" kids = showParen True (show_kids ' ' (("(:)"++):kids))
 shows_term ctor kids = showParen True (show_kids ' ' ((ctor++):kids))
 show_arr xs = (showChar '[' . show_kids ',' xs . showChar ']')

gshowsCompanyDumb x = gsdumb spec x ""
 where spec = (\ (s::Int)  _  -> shows s) :+:
	      (\ (s::Char) _  -> shows s) :+:
	      (\ (s::Float) _ -> shows s) :+:
	      HNil


-- Turn gsdumb into a highest-order generic function
newtype GSDumb spec = GSDumb spec

instance (GAPP (TL_red_ctr ShowS) spec a ShowS ShowS)
    => Apply (GSDumb spec) a String where
   apply (GSDumb spec) x = gsdumb spec x ""

-- The signature could be dropped...
gapplyShowList :: [Int] -> [String]
gapplyShowList lst = gapp (TL_red_shallow (GSDumb spec)) HNil lst
 where spec = (\ (s::Int)   _ -> shows s) :+:
	      (\ (s::Char)  _ -> shows s) :+:
	      (\ (s::Float) _ -> shows s) :+:
	      HNil

-- A different GShow to present arrays better (still pretty dumb compared to
-- gs2 below). But it is needed for the tests

gsdumb_arr spec a = gapp (TL_red_ctr shows_term) spec a
 where
 show_kids delim xs = (foldr (.) id . intersperse (showChar delim)) xs
 shows_term "[]" [] True = showString ""
 shows_term "[]" [] False = showString "[]"
 shows_term ctor [] _ = shows ctor
 shows_term ":" [h,t] False = showChar '[' . h False . t True . showChar ']'
 shows_term ":" [h,t] True  = showChar ',' . h False . t True
 shows_term ctor kids _ = 
     showParen True (show_kids ' ' ((ctor++):(map ($ False) kids)))

gshowsCompanyDumb_arr x = gsdumb_arr spec x False ""
 where spec = (\ (s::Int)   _ -> shows' s) :+:
	      (\ (s::Char)  _ -> shows' s) :+:
	      (\ (s::Float) _ -> shows' s) :+:
	      HNil
       shows' s _ = shows s


-- Generic show
-- The output is virtually identical to that of Haskell's show
-- (we can tweak the code to drop redundant parentheses)

gs1 a = gapp (TL_red_ctr shows_term)
	-- override for strings, to match the behavior of Haskell's show
	-- for strings. We also override for primitive datatypes, like Float
	((\ (s::String) _ -> shows s) :+:
	 (\ (s::Float)  _ -> shows s) :+:
	HNil)
	a
 where
 show_kids delim xs = (foldr (.) id . intersperse (showChar delim)) xs
 shows_term "[]" [] = showString "[]"
 shows_term ctor [] = shows ctor
 -- shows_term ":"  kids = show_arr kids
 shows_term ":" kids = showParen True (show_kids ' ' (("(:)"++):kids))
 shows_term ctor kids = showParen True (show_kids ' ' ((ctor++):kids))
 show_arr xs = (showChar '[' . show_kids ',' xs . showChar ']')

-- Now, we attempt to match Haskell's show.
-- We introduce two flags: the first is True if the term is internal,
-- and so its output needs outer parentheses.
-- The second flag is True if we are showing an array.
type ShowS' = (Bool,Bool) -> ShowS

l' :: ShowS -> ShowS'
l' sh f str = sh str

shows' :: Show a => a -> ShowS'
shows' = l' . shows 

gs2 a = gapp (TL_red_ctr shows_term)
	-- override for strings, to match the behavior of Haskell's show
	-- for strings. We also override for primitive datatypes, like Float
	((\ (s::String) _ -> shows' s) :+:
	 (\ (s::Float)  _ -> shows' s) :+:
	HNil)
	a
 where
 show_kids delim xs = (foldr (.) id . intersperse (showChar delim)) xs
 shows_term "[]" [] (_,True) = showString ""
 shows_term "[]" [] (_,False) = showString "[]"
 shows_term ctor [] _ = shows ctor
 shows_term ":" [h,t] (_,False) = showChar '[' . 
				  h (False,False) . t (False,True) . 
				  showChar ']'
 shows_term ":" [h,t] (_,True)  = showChar ',' . 
				  h (False,False) . t (False,True)
 shows_term ctor kids (inner,arr) = 
     showParen inner (show_kids ' ' ((ctor++):(map ($ (True,False)) kids)))


test1 = print $ gs1 genCom ""

test2 = gs2 genCom (False,False) ""

-- Test that we match show
tests = test2 == show genCom

-- gshowsCompany x = gs2 x (False,False) ""


-- The version of gsdumb that can be extended (or, overridden) for
-- particular data types

newtype ShowR = ShowR{unShowR :: String->String}
gsdumb_ext a = gapp (TL_red_ctr (\x y -> ShowR $ shows_term x y)) spec a
 where
 show_kids delim xs = 
     (foldr (\ (ShowR x) y -> x . y) id . 
      intersperse (ShowR (showChar delim))) xs
 shows_term "[]" [] = showString "[]"
 shows_term ctor [] = shows ctor
 shows_term ":" kids = showParen True (show_kids ' ' ((ShowR ("(:)"++)):kids))
 shows_term ctor kids = showParen True (show_kids ' ' ((ShowR (ctor++)):kids))
 spec = (\ (s::Int)  _  -> ShowR $ shows s) :+:
	(\ (s::Char) _  -> ShowR $ shows s) :+:
	(\ (s::Float) _ -> ShowR $ shows s) :+:
	HNil


-- switching for the dumb GShow for the tests...
gshowsCompany x = unShowR (gsdumb_ext x) ""


