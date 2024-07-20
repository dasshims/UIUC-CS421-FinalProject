{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

-- we develop several versions of GShow

module SmashA.GShow (gshowsCompany) where

import CompanyDatatypes
import SmashA.CompanyDats

import SmashA.Syb4A
import Data.List (intersperse)

-- Generic show
-- The output is vertually identical to that of Haskell's show
-- (we can tweak the code to drop redundant parentheses)

gs1 a = gapp (TL_red_ctr shows_term)
	-- override for strings, to match the behavior of Haskell's show
	-- for strings. We also override for primitive datatypes, like Float
	((\ (s::String) -> shows s) :+:
	 (\ (s::Float) -> shows s) :+:
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
	((\ (s::String) -> shows' s) :+:
	 (\ (s::Float) ->  shows' s) :+:
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

gshowsCompany x = gs2 x (False,False) ""

