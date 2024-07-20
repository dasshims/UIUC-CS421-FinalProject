
-- the Unit data type from GMsec2 has been replaced by Unit'

module CompanyReps where

import GMsec2 hiding (Unit)
import CompanyDatatypes 
import qualified GMsec2

-- The type representation of the company data type.

-- Company --
instance TypeRep Company where 
    typeRep = datatype (Iso fromCompany toCompany)

fromCompany :: Company -> Constr (Plus (Constr GMsec2.Unit) (Constr (Pair Dept [Dept])))
fromCompany (C x) = Constr "C" 1 (fromList x)

toCompany :: Constr (Plus (Constr GMsec2.Unit) (Constr (Pair Dept [Dept]))) -> Company
toCompany (Constr _ _ x) = C (toList x)

-- Dept --
instance TypeRep Dept 
    where typeRep = datatype (Iso fromDept toDept)
--fromDept :: Dept -> Constr (Pair Name (Pair Manager (Plus (Constr GMsec2.Unit) (Constr (Pair Unit [Unit])))))
fromDept (D n m us) = (Constr "D" 3 (Pair n (Pair m (fromList us))))

-- toDept :: Constr (Pair Name (Pair Manager (Plus GMsec2.Unit (Pair Unit [Unit])))) -> Dept
toDept (Constr _ _ (Pair n (Pair m us))) = D n m (toList us)

-- Unit --
instance TypeRep Unit
    where typeRep = datatype (Iso fromUnit toUnit)
fromUnit :: Unit -> Plus (Constr Employee) (Constr Dept)
fromUnit (PU e) = Inl (Constr "PU" 1 e)
fromUnit (DU d) = Inr (Constr "DU" 1 d)

toUnit :: Plus (Constr Employee) (Constr Dept) -> Unit
toUnit (Inl (Constr _ _ e)) = PU e
toUnit (Inr (Constr _ _ d)) = DU d

-- Employee --
instance TypeRep Employee
    where typeRep = datatype (Iso fromEmployee toEmployee)
fromEmployee :: Employee -> Constr (Pair Person Salary)
fromEmployee (E p s) = Constr "E" 2 (Pair p s)

toEmployee :: Constr (Pair Person Salary) -> Employee
toEmployee (Constr _ _ (Pair p s)) = E p s

-- Person --
instance TypeRep Person
    where typeRep = datatype (Iso fromPerson toPerson)
fromPerson :: Person -> Constr (Pair Name Address)
fromPerson (P n a) = Constr "P" 2 (Pair n a)

toPerson :: Constr (Pair Name Address) -> Person
toPerson (Constr _ _ (Pair n a)) = P n a

-- Salary --
instance TypeRep Salary
    where typeRep = datatype (Iso fromSalary toSalary)
fromSalary (S f) = Constr "S" 1 f
toSalary (Constr _ _ f) = (S f)
