{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- The organisational structure of a company

module Smash.CompanyDats where

import Smash.Syb4

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import CompanyDatatypes


-- we do need however the instances for our version of Data (which
-- we call Dat). Unlike Data, Dat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually

-- Syb4 already has instances for Int, Bool, Char, any array and a pair

-- we didn't have Float
instance SApply spec Float => Dat spec Float

instance (SApply spec Salary, SApply spec Float) 
    => Dat spec Salary where
  genmapq spec reducer (S s)  = reducer [gmapq spec reducer s]

-- First, omit the instance cosntraints, let GHC complain about missing
-- constraints. Then cut and paste the constraints from the error message in
-- the GHCi window
instance (SApply spec Person, SApply spec Char, 
	  SApply spec Address) 
    => Dat spec Person where
  genmapq spec reducer (P name addr)  = reducer [gmapq spec reducer name,
						 gmapq spec reducer addr]
-- Just cut and paste...
instance (SApply spec Employee, 
	  SApply spec Person,
	  SApply spec Name,
	  SApply spec Char,
	  SApply spec Salary, 
	  SApply spec Float)
    => Dat spec Employee where
  genmapq spec reducer (E p s)  = reducer [gmapq spec reducer p,
					   gmapq spec reducer s]

-- Just cut and paste...
-- This is where GHC API could help: it could insert the `missing' constraints
-- automatically. There was even a paper about it in HW2006...
instance (SApply spec Unit,
	  SApply spec [Unit],
	  SApply spec Employee,
	  SApply spec Person,
	  SApply spec Name,
	  SApply spec Char,
	  SApply spec Salary,
	  SApply spec Float,
	  SApply spec Dept)
    => Dat spec Unit where
  genmapq spec reducer (PU e)  = reducer [gmapq spec reducer e]
  genmapq spec reducer (DU d)  = reducer [gmapq spec reducer d]

instance (SApply spec Dept,
	  SApply spec [Unit],
	  SApply spec Unit,
	  SApply spec Employee,
	  SApply spec Name,
	  SApply spec Person,
	  SApply spec Char,
	  SApply spec Salary,
	  SApply spec Float)
    => Dat spec Dept where
  genmapq spec reducer (D n m us)  = reducer [gmapq spec reducer n,
					      gmapq spec reducer m,
					      gmapq spec reducer us]

instance (SApply spec Company,
	  SApply spec [Dept],
	  SApply spec Dept,
	  SApply spec [Unit],
	  SApply spec Unit,
	  SApply spec Employee,
	  SApply spec Person,
	  SApply spec Name,
	  SApply spec Char,
	  SApply spec Salary,
	  SApply spec Float)
    => Dat spec Company where
  genmapq spec reducer (C ds)  = reducer [gmapq spec reducer ds]

