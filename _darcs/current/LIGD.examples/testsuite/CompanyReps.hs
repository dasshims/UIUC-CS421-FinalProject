{-# OPTIONS -fglasgow-exts #-}
-- The glasgow exts are for the infix type constructor :*:

module CompanyReps where

import LIGD hiding (rUnit)
import CompanyDatatypes

rCompany :: Rep Company
rCompany = RType (App "Company" [])
			        (RCon "C" (rList rDept))
					  (EP unC C)

unC :: Company -> [Dept]
unC (C d) = d

rDept :: Rep Dept
rDept = RType (App "Dept" [])
		        (RCon "D" (rPair rName (rPair rManager (rList rUnit))))
				  (EP fromDept toDept)

toDept :: (Name :*: Manager :*: [Unit]) -> Dept
toDept (n :*: m :*: lu) = D n m lu

fromDept :: Dept -> (Name :*: Manager :*: [Unit])
fromDept (D n m lu) = (n :*: m :*: lu)

rUnit = RType (App "Unit" [])
		        (rSum (RCon "PU" rEmployee) 
					     (RCon "DU" rDept))
				  (EP fromUnit toUnit)
toUnit :: (Employee :+: Dept) -> Unit
toUnit (Inl x) = PU x
toUnit (Inr x) = DU x

fromUnit :: Unit -> (Employee :+: Dept)
fromUnit (PU x) = Inl x
fromUnit (DU x) = Inr x

rEmployee = RType (App "Employee" [])
				      (RCon "E" (rPerson `rPair` rSalary))
						(EP fromEmployee toEmployee)

toEmployee (p :*: s) = E p s
fromEmployee (E p s) = p :*: s

rPerson = RType (App "Person" [])
			       (RCon "P" (rName `rPair` rAddress))
					 (EP fromPerson toPerson)
toPerson (n :*: a) = P n a
fromPerson (P n a) = n :*: a

rSalary = RType (App "Salary" [])
			       (RCon "S" rFloat)
					 (EP unS S)
unS (S f) = f

rManager = rEmployee
rName    = rList rChar
rAddress = rList rChar


instance Representable Company  where rep = rCompany
instance Representable Dept     where rep = rDept
instance Representable Unit     where rep = rUnit
instance Representable Employee where rep = rEmployee
instance Representable Salary   where rep = rSalary
instance Representable Person   where rep = rPerson
