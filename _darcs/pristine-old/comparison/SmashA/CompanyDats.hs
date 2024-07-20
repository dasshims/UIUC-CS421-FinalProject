{-# OPTIONS -fglasgow-exts  #-}
{-# OPTIONS -fallow-undecidable-instances  #-}

-- The organisational structure of a company

module SmashA.CompanyDats where

import SmashA.Syb4A

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import CompanyDatatypes


-- we do need however the instances for our version of Data (which
-- we call LDat). Unlike Data, LDat is not part of Haskell as so we have to 
-- derive it manually. It is pretty straightforward. We can use Derive
-- or TH (as is done by RepLib).
-- But here, we just do it manually. The derivation is the straightforward
-- function of the definition of the BinTree data type.
-- We derive the instances of LDat for all three traversal strategies

-- Syb4 already has instances for Int, Bool, Char, any array and a pair

-- we didn't have Float
instance LDat TL_recon spec Float Float where
    gin _ spec x = x
instance LDat (TL_red w) spec Float w where
    gin (TL_red f) spec x = f []
instance LDat (TL_red_ctr w) spec Float w where
    gin (TL_red_ctr f) spec x = f "Float" []


instance (GAPP TL_recon spec Float Float Float)
    => LDat TL_recon spec Salary Salary where
  gin tlab spec (S s)  = S (gapp tlab spec s)

instance (GAPP (TL_red w) spec Float w w)
    => LDat (TL_red w) spec Salary w where
  gin tlab@(TL_red f) spec (S s)  = f [gapp tlab spec s]

instance (GAPP (TL_red_ctr w) spec Float w w)
    => LDat (TL_red_ctr w) spec Salary w where
  gin tlab@(TL_red_ctr f) spec (S s)  = f "S" [gapp tlab spec s]


instance (GAPP TL_recon spec String String String)
    => LDat TL_recon spec Person Person where
  gin tlab spec (P name addr)  = P (gapp tlab spec name)
				   (gapp tlab spec addr)

instance (GAPP (TL_red w) spec String w w)
    => LDat (TL_red w) spec Person w where
  gin tlab@(TL_red f) spec (P name addr)  = f [gapp tlab spec name,
					       gapp tlab spec addr]


instance (GAPP (TL_red_ctr w) spec String w w)
    => LDat (TL_red_ctr w) spec Person w where
  gin tlab@(TL_red_ctr f) spec (P name addr)  = f "P" [gapp tlab spec name,
						       gapp tlab spec addr]

instance (GAPP TL_recon spec Person Person Person,
	  GAPP TL_recon spec Salary Salary Salary)
    => LDat TL_recon spec Employee Employee where
  gin tlab spec (E p s)  = E (gapp tlab spec p)
			     (gapp tlab spec s)

instance (GAPP (TL_red w) spec Person w w,
	  GAPP (TL_red w) spec Salary w w)
    => LDat (TL_red w) spec Employee w where
  gin tlab@(TL_red f) spec (E p s)  = f [gapp tlab spec p,
					 gapp tlab spec s]

instance (GAPP (TL_red_ctr w) spec Person w w,
	  GAPP (TL_red_ctr w) spec Salary w w)
    => LDat (TL_red_ctr w) spec Employee w where
  gin tlab@(TL_red_ctr f) spec (E p s)  = f "E" [gapp tlab spec p,
						 gapp tlab spec s]


instance (GAPP TL_recon spec Employee Employee Employee,
	  GAPP TL_recon spec Dept Dept Dept)
    => LDat TL_recon spec Unit Unit where
  gin tlab spec (PU e)  = PU (gapp tlab spec e)
  gin tlab spec (DU d)  = DU (gapp tlab spec d)

instance (GAPP (TL_red w) spec Employee w w,
	  GAPP (TL_red w) spec Dept w w)
    => LDat (TL_red w) spec Unit w where
  gin tlab@(TL_red f) spec (PU e)  = f [gapp tlab spec e]
  gin tlab@(TL_red f) spec (DU d)  = f [gapp tlab spec d]

instance (GAPP (TL_red_ctr w) spec Employee w w,
	  GAPP (TL_red_ctr w) spec Dept w w)
    => LDat (TL_red_ctr w) spec Unit w where
  gin tlab@(TL_red_ctr f) spec (PU e)  = f "PU" [gapp tlab spec e]
  gin tlab@(TL_red_ctr f) spec (DU d)  = f "DU" [gapp tlab spec d]


{-
instance (GAPP TL_recon spec String String String,
	  GAPP TL_recon spec Employee Employee Employee,
	  GAPP TL_recon spec [Unit] [Unit] [Unit])
    => LDat TL_recon spec Dept Dept where
  gin tlab spec (D n m us)  = D (gapp tlab spec n)
			        (gapp tlab spec m)
			        (gapp tlab spec us)
-}

-- Weird, the above instance causes GHC to loop forever. Som kind of bug?
instance (STApply spec Char Char Char,
	  STApply spec String String String,
	  STApply spec Person Person Person,
	  STApply spec Salary Salary Salary,
	  STApply spec Float Float Float,
	  STApply spec Unit Unit Unit,
	  STApply spec [Unit] [Unit] [Unit],
	  STApply spec Dept Dept Dept,
	  STApply spec Employee Employee Employee
	 )
    => LDat TL_recon spec Dept Dept where
  gin tlab spec (D n m us)  = D (gapp tlab spec n)
			        (gapp tlab spec m)
			        (gapp tlab spec us)


instance (GAPP (TL_red w) spec String w w,
	  GAPP (TL_red w) spec Employee w w,
	  GAPP (TL_red w) spec [Unit] w w)
    => LDat (TL_red w) spec Dept w where
  gin tlab@(TL_red f) spec (D n m us)  = f [gapp tlab spec n,
					    gapp tlab spec m,
					    gapp tlab spec us]

instance (GAPP (TL_red_ctr w) spec String w w,
	  GAPP (TL_red_ctr w) spec Employee w w,
	  GAPP (TL_red_ctr w) spec [Unit] w w)
    => LDat (TL_red_ctr w) spec Dept w where
  gin tlab@(TL_red_ctr f) spec (D n m us)  = f "D" [gapp tlab spec n,
						    gapp tlab spec m,
						    gapp tlab spec us]

instance (GAPP TL_recon spec [Dept] [Dept] [Dept])
    => LDat TL_recon spec Company Company where
  gin tlab spec (C ds)  = C (gapp tlab spec ds)

instance (GAPP (TL_red w) spec [Dept] w w)
    => LDat (TL_red w) spec Company w where
  gin tlab@(TL_red f) spec (C ds)  = f [gapp tlab spec ds]

instance (GAPP (TL_red_ctr w) spec [Dept] w w)
    => LDat (TL_red_ctr w) spec Company w where
  gin tlab@(TL_red_ctr f) spec (C ds)  = f "C" [gapp tlab spec ds]


-- The lockstep traversal strategy

instance LDat (TL_red_lockstep w) spec (Couple Float) w where
    gin (TL_red_lockstep _ f) spec _ = f []


instance (GAPP (TL_red_lockstep w) spec (Couple Float) w w)
    => LDat (TL_red_lockstep w) spec (Couple Salary) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (S sa) (S sb)) = 
      f [gapp tlab spec (Couple sa sb)]

instance (GAPP (TL_red_lockstep w) spec (Couple String) w w)
    => LDat (TL_red_lockstep w) spec (Couple Person) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (P name1 addr1) (P name2 addr2))
      = f [gapp tlab spec (Couple name1 name2),
	   gapp tlab spec (Couple addr1 addr2)]

instance (GAPP (TL_red_lockstep w) spec (Couple Person) w w,
	  GAPP (TL_red_lockstep w) spec (Couple Salary) w w)
    => LDat (TL_red_lockstep w) spec (Couple Employee) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (E p1 s1) (E p2 s2)) 
      = f [gapp tlab spec (Couple p1 p2),
	   gapp tlab spec (Couple s1 s2)]

instance (GAPP (TL_red_lockstep w) spec (Couple Employee) w w,
	  GAPP (TL_red_lockstep w) spec (Couple Dept) w w)
    => LDat (TL_red_lockstep w) spec (Couple Unit) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (PU e1) (PU e2)) 
      = f [gapp tlab spec (Couple e1 e2)]
  gin tlab@(TL_red_lockstep _ f) spec (Couple (DU d1) (DU d2))
      = f [gapp tlab spec (Couple d1 d2)]
  gin tlab@(TL_red_lockstep d _) spec _ = d


instance (GAPP (TL_red_lockstep w) spec (Couple String) w w,
	  GAPP (TL_red_lockstep w) spec (Couple Employee) w w,
	  GAPP (TL_red_lockstep w) spec (Couple [Unit]) w w)
    => LDat (TL_red_lockstep w) spec (Couple Dept) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (D n1 m1 us1) (D n2 m2 us2))
      = f [gapp tlab spec (Couple n1 n2),
	   gapp tlab spec (Couple m1 m2),
	   gapp tlab spec (Couple us1 us2)]

instance (GAPP (TL_red_lockstep w) spec (Couple [Dept]) w w)
    => LDat (TL_red_lockstep w) spec (Couple Company) w where
  gin tlab@(TL_red_lockstep _ f) spec (Couple (C ds1) (C ds2))
      = f [gapp tlab spec (Couple ds1 ds2)]

