{-# OPTIONS_GHC -fglasgow-exts #-}
-- The glasgow exts are for the infix type constructor :*:

{-

To make ad-hoc cases available for every data type within the
compound data type `Company', ONE subclass is declared. All
data types are added as methods to that subclass and a default
implementation is added. This default implementation uses the
isomorphism (view/datatype) method. If one needs to add an
ad-hoc case to the generic function, the default implementation
can be overidden. If the compound data type is change, for
instance when the Employee data type is extended with a room number,
the ONE subclass has to be extended with a method and a default
implementation, but the generic functions do not have to be
adapted.


If a subclass is given for every data type, a generic function
would have to make an instance declaration per data type subclass.
In this case, changing a data type would mean that every function
has to add an instance declaration.

-}


module CompanyReps3 where

import GL3 hiding (Name)
import CompanyDatatypes
import Data.Generics hiding (Generic, Unit)
import qualified Data.Generics as DG

-- The type representation of the company data type.

class Generic g => GenericCompany g where
  company  :: g Company Company Company
  company  =  view isoCompany isoCompany isoCompany (constr "C" 1 (rList dept))
  dept     :: g Dept Dept Dept
  dept     =  view isoDept isoDept isoDept
                   (constr "D" 3 (rList char <*> employee <*> rList unit'))
  unit'    :: g Unit Unit Unit
  unit'    =  view isoUnit isoUnit isoUnit
                   (constr "PU" 1 employee <|> constr "DU" 1 dept)
  employee :: g Employee Employee Employee
  employee =  view isoEmployee isoEmployee isoEmployee
                   (constr "E" 2 (person <*> salary))
  person   :: g Person Person Person
  person   =  view isoPerson isoPerson isoPerson
                   (constr "P" 2 (rList char <*> rList char))
  salary   :: g Salary Salary Salary
  salary   =  view isoSalary isoSalary isoSalary (constr "S" 1 float)

instance GenericCompany g => GRep g Company where
  over = company

-- Company --
isoCompany = Iso fromCompany toCompany
fromCompany :: Company -> [Dept]
fromCompany (C x) = x
toCompany :: [Dept] -> Company
toCompany x = C x

-- Dept --
isoDept = Iso fromDept toDept
fromDept :: Dept -> Name :*: (Manager :*: [Unit])
fromDept (D n m us) = n :*: (m :*: us)
toDept :: Name :*: (Manager :*: [Unit]) -> Dept
toDept (n :*: (m :*: us)) = D n m us

-- Unit --
isoUnit = Iso fromUnit toUnit
fromUnit :: Unit -> Employee :+: Dept
fromUnit (PU e) = Inl e
fromUnit (DU d) = Inr d
toUnit :: Employee :+: Dept -> Unit
toUnit (Inl e) = PU e
toUnit (Inr d) = DU d

-- Employee --
isoEmployee = Iso fromEmployee toEmployee
fromEmployee :: Employee -> Person :*: Salary
fromEmployee (E p s) = p :*: s
toEmployee :: Person :*: Salary -> Employee
toEmployee (p :*: s) = E p s

-- Person --
isoPerson = Iso fromPerson toPerson
fromPerson :: Person -> Name :*: Address
fromPerson (P n a) = n :*: a
toPerson :: Name :*: Address -> Person
toPerson (n :*: a) = P n a

-- Salary --
isoSalary = Iso fromSalary toSalary
fromSalary (S f) = f
toSalary f = (S f)
