{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- The organisational structure of a company
-- Deriving TDat instances. The TDat is analogous, and more general,
-- than Dat (see CompanyDats.hs). Still, Dat represents a nice special
-- case (which its simpler to derive). Therefore, we keep both Dat and
-- TDat classes. 

module Smash.CompanyTDats where

import Smash.Syb4

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import CompanyDatatypes


-- we do need however the instances for our version of Data (which
-- we call TDat). Unlike Data, TDat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually

-- Syb4 already has instances for Int, Bool, Char, any array and a pair,
-- Either and Maybe 

-- we didn't have Float
instance STApply spec Float Float w => TDat spec Float w where
    gtmapq spec x = stapply spec x x


instance (STApply spec Salary Salary w,
	  STApply spec Float Float Float)
    => TDat spec Salary w where
    gtmapq spec x@(S a) = stapply spec x (S $ gtmapq spec a)

-- First, omit the instance cosntraints, let GHC complain about missing
-- constraints. Then cut and paste the constraints from the error message in
-- the GHCi window
instance (STApply spec Person Person w,
	  STApply spec Char Char w',
	  STApply spec [Char] [w'] [Char])
    => TDat spec Person w where
    gtmapq spec x@(P n a) = stapply spec x (P (gtmapq spec n)
					      (gtmapq spec a))

-- Just cut and paste...
instance (STApply spec Employee Employee w,
	  STApply spec Person Person Person,
	  STApply spec Char Char w',
	  STApply spec [Char] [w'] [Char],
	  STApply spec Float Float Float,
	  STApply spec Salary Salary Salary)
    => TDat spec Employee w where
    gtmapq spec x@(E p s) = stapply spec x (E (gtmapq spec p)
					      (gtmapq spec s))

-- Just cut and paste...
-- This is where GHC API could help: it could insert the `missing' constraints
-- automatically. There was even a paper about it in HW2006...
instance (STApply spec Employee Employee Employee,
	  STApply spec Person Person Person,
	  STApply spec Char Char w',
	  STApply spec [Char] [w'] [Char],
	  STApply spec Float Float Float,
	  STApply spec Salary Salary Salary,
	  STApply spec Unit Unit w,
	  STApply spec Dept Dept Dept,
	  STApply spec [Unit] [w] [Unit])
    => TDat spec Unit w where
    gtmapq spec x@(PU e) = stapply spec x (PU (gtmapq spec e))
    gtmapq spec x@(DU d) = stapply spec x (DU (gtmapq spec d))

instance (STApply spec Dept Dept Dept,
	  STApply spec [Char] [w'] [Char],
	  STApply spec Char Char w',
	  STApply spec [Unit] [w'1] [Unit],
	  STApply spec Unit Unit w'1,
	  STApply spec Salary Salary Salary,
	  STApply spec Float Float Float,
	  STApply spec Person Person Person,
	  STApply spec Employee Employee Employee)
    => TDat spec Dept Dept where
    gtmapq spec x@(D n m us) = stapply spec x (D (gtmapq spec n)
					         (gtmapq spec m)
					         (gtmapq spec us))

instance (STApply spec Company Company w,
	  STApply spec [Dept] [Dept] [Dept],
	  STApply spec Dept Dept Dept,
	  STApply spec [Char] [w'] [Char],
	  STApply spec Char Char w',
	  STApply spec [Unit] [w'1] [Unit],
	  STApply spec Unit Unit w'1,
	  STApply spec Salary Salary Salary,
	  STApply spec Float Float Float,
	  STApply spec Person Person Person,
	  STApply spec Employee Employee Employee)
    => TDat spec Company w where
    gtmapq spec x@(C ds) = stapply spec x (C (gtmapq spec ds))

