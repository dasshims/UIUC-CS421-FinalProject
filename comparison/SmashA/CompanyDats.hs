{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-undecidable-instances  #-}

-- The organisational structure of a company

module CompanyDats where

import Syb4A

-- The latter not only defines paradise but makes instances
-- for Data and Typeable. We do not use those instances here.
import CompanyDatatypes

import Control.Monad  (liftM,liftM2,liftM3)

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

-- Weird, the above instance causes GHC to loop forever. Some kind of bug?
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

-- Some text explaining the problem above (and justifying the score for
-- universe size)
--
--
--
-- AR> The reason for putting sufficient only was the comment right before the LDat instance
-- AR> for Dept. It says:
-- AR> 
-- AR> -- Weird, the above instance causes GHC to loop forever. Some kind of bug?
-- AR> 
-- AR> So now, the programmer cannot use a "normal" instance such as:
-- AR> 
-- AR> instance (GAPP TL_recon spec String String String,
-- AR>           GAPP TL_recon spec Employee Employee Employee,
-- AR>           GAPP TL_recon spec [Unit] [Unit] [Unit])
-- AR>     => LDat TL_recon spec Dept Dept where
-- AR>   gin tlab spec (D n m us)  = D (gapp tlab spec n)
-- AR>                                 (gapp tlab spec m)
-- AR>                                 (gapp tlab spec us)
-- AR> 
-- AR> 
-- AR> but the following must be used:
-- AR> 
-- AR> instance (STApply spec Char Char Char,
-- AR>           STApply spec String String String,
-- AR>           STApply spec Person Person Person,
-- AR>           STApply spec Salary Salary Salary,
-- AR>           STApply spec Float Float Float,
-- AR>           STApply spec Unit Unit Unit,
-- AR>           STApply spec [Unit] [Unit] [Unit],
-- AR>           STApply spec Dept Dept Dept,
-- AR>           STApply spec Employee Employee Employee
-- AR>          )
-- AR>     => LDat TL_recon spec Dept Dept where
-- AR>   gin tlab spec (D n m us)  = D (gapp tlab spec n)
-- AR>                                 (gapp tlab spec m)
-- AR>                                 (gapp tlab spec us)
-- AR> 
-- AR> 
-- AR> The cause could be a compiler bug, or maybe the library requires special
-- AR> instances in certain situations. Since it is not clear where the problem
-- AR> lies, Smash gets sufficient.
--
-- OK> I since had a chance to update my GHC to 6.8.2 (it's a good thing
-- OK> FreeBSD had a compiled package for it: the attempt to compile from
-- OK> source failed. When GHC was compiling itself, it reported a problem
-- OK> about an i-node being locked. I hate this. I never had problems like
-- OK> that with OCaml...). Anyway, the mysterious looping in
-- OK> renamer/typechecker persisted. I should have checked against the HEAD,
-- OK> but given the problems I had and the time it takes to compile GHC, I
-- OK> decided to pass on it.
-- OK> 
-- OK> I guess the issue here is which of the two instances above to call
-- OK> `normal'. The latter, albeit more voluminous, is also easier to write:
-- OK> one starts with one STApply constraint, and cut-and-paste more from
-- OK> GHC error messages. I remember there was a paper on the Haskell
-- OK> workshop that used GHC API to automate the process. In any event, both
-- OK> forms of instances can be generated automatically. For example, we can
-- OK> add derive Typeable to Company and the used TypeRep and the Template
-- OK> Haskell to generate the instances. TypeRep has just enough
-- OK> information. 
-- OK> 
-- OK> 	In any case, the fact Smash triggers some compiler bug is
-- OK> enough to earn it demerit in the complexity category...


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


-- Monadic reconstruction
instance Monad m => LDat (TL_reconM m) spec Float (m Float) where
    gin _ spec x = return x

instance (Monad m, GAPP (TL_reconM m) spec Float (m Float) (m Float))
    => LDat (TL_reconM m) spec Salary (m Salary) where
  gin tlab spec (S s)  = liftM S (gapp tlab spec s)

instance (Monad m, GAPP (TL_reconM m) spec String (m String) (m String))
    => LDat (TL_reconM m) spec Person (m Person) where
  gin tlab spec (P name addr)  = liftM2 P 
				   (gapp tlab spec name)
				   (gapp tlab spec addr)

instance (Monad m,
	  GAPP (TL_reconM m) spec Person (m Person) (m Person),
	  GAPP (TL_reconM m) spec Salary (m Salary) (m Salary))
    => LDat (TL_reconM m) spec Employee (m Employee) where
  gin tlab spec (E p s)  = liftM2 E 
			     (gapp tlab spec p)
			     (gapp tlab spec s)

instance (Monad m,
	  GAPP (TL_reconM m) spec Employee (m Employee) (m Employee),
	  GAPP (TL_reconM m) spec Dept (m Dept) (m Dept))
    => LDat (TL_reconM m) spec Unit (m Unit) where
  gin tlab spec (PU e)  = liftM PU (gapp tlab spec e)
  gin tlab spec (DU d)  = liftM DU (gapp tlab spec d)



{-
instance (Monad m,
	  GAPP (TL_reconM m) spec String (m String) (m String),
	  GAPP (TL_reconM m) spec Employee (m Employee) (m Employee),
	  GAPP (TL_reconM m) spec [Unit] (m [Unit]) (m [Unit]))
    => LDat (TL_reconM m) spec Dept (m Dept) where
  gin tlab spec (D n m us)  = liftM3 D 
			        (gapp tlab spec n)
			        (gapp tlab spec m)
			        (gapp tlab spec us)

-}

-- Weird, the above instance causes GHC to loop forever. Some kind of bug?
instance (Monad m,
	  STApply spec Char (m Char) (m Char),
	  STApply spec String (m String) (m String),
	  STApply spec Person (m Person) (m Person),
	  STApply spec Salary (m Salary) (m Salary),
	  STApply spec Float (m Float) (m Float),
	  STApply spec Unit (m Unit) (m Unit),
	  STApply spec [Unit] (m [Unit]) (m [Unit]),
	  STApply spec Dept (m Dept) (m Dept),
	  STApply spec Employee (m Employee) (m Employee)
	 )
    => LDat (TL_reconM m) spec Dept (m Dept) where
  gin tlab spec (D n m us)  = liftM3 D 
			        (gapp tlab spec n)
			        (gapp tlab spec m)
			        (gapp tlab spec us)

instance (Monad m, GAPP (TL_reconM m) spec [Dept] (m [Dept]) (m [Dept]))
    => LDat (TL_reconM m) spec Company (m Company) where
  gin tlab spec (C ds)  = liftM C (gapp tlab spec ds)
