{-# OPTIONS -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

-- we develop several versions of GShow

module Smash.GShow (gshowsCompany) where

import CompanyDatatypes
import Smash.CompanyDats

import Smash.Syb4
import Data.List (intersperse)

-- Generic show

-- The following version produces the output almost identical to that
-- of the regular show (modulo the outermost parentheses)

-- The first version is the ugliest one -- and also the most revealing
-- That's why it is retained, despite its extreme ugliness.
-- The revealing part is that it is literally the dual of the derived
-- Show instances for the Company datatype.
-- The SCons list below truly reflect the derived instances of Show,
-- only expressed as the data structure rather than the collection of 
-- instances.
-- This comes from the fact Smash is `dual' to typeclasses.
-- The ugliness comes from several sources. First, the ugly signature.
-- Alas, the signature is required due to the polymorphic recursion.
-- The long constraint is wholly inferred: I literally cut the constraint
-- from the GHCi error message and pasted it here. It would be nice if Haskell
-- supported partial signatures, as often requested: we could merely
-- specify the type, which is a -> ShowS, and GHC will infer the constrains
-- (which it _certainly_ can, in this case). 
-- The large amount of boilerplate below could all be eliminated if
-- our Dat class had a method that reported the name of the constructor:
-- similar to the one in the Data class. It is easy to extend Dat to that
-- effect.
gs1 :: (Dat (SCons String
				 (SCons Salary
					(SCons Company
					       (SCons Person
						      (SCons Employee
							     (SCons Unit
								    (SCons Dept
									   (SCons [Unit]
										  (SCons [Dept]
											 SNil)))))))))
			  a) => 
	 a -> ShowS
gs1 a = gmapq (SCons (\ (s::String) -> shows s) 
		 (SCons (\ (S sal) -> shows_term "S" [shows sal])
		 (SCons (\ (C ds) -> shows_term "C" [shows ds])
		  (SCons (\ (P name addr) -> 
			  shows_term "P" [gs1 name, gs1 addr])
		   (SCons (\ (E p s) ->
			   shows_term "E" [gs1 p,gs1 s])
		    (SCons (\u ->
			    case u of
			    PU e -> shows_term "PU" [gs1 e]
			    DU e -> shows_term "DU" [gs1 e])
		     (SCons (\ (D n m us) ->
			      shows_term "D" [gs1 n, gs1 m,
					      gs1 us])
		      (SCons (\ (units::[Unit]) -> show_arr (map gs1 units))
		       (SCons (\ (ds::[Dept]) -> show_arr (map gs1 ds))
		 SNil)))))))))
                (show_kids ' ') a
  where
  show_kids delim xs = (foldr (.) id . intersperse (showChar delim)) xs
  show_arr xs = (showChar '[' . show_kids ',' xs . showChar ']')
  shows_term ctor kids = showParen True (show_kids ' ' ((ctor++):kids))


test1 = print $ gs1 genCom ""


gshowsCompany x = gs1 x ""

