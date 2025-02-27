{-# OPTIONS_GHC -fglasgow-exts -fgenerics #-}

-- The organisational structure of a company

module CompanyDatatypes where
import Data.Generics hiding (Unit)

data Company  = C [Dept]               deriving Show
data Dept     = D Name Manager [Unit]  deriving Show
data Unit     = PU Employee | DU Dept  deriving Show
data Employee = E Person Salary        deriving Show
data Person   = P Name Address         deriving Show
data Salary   = S Float                deriving Show
type Manager  = Employee
type Name     = String
type Address  = String

-- An illustrative company
genCom :: Company
genCom = C [D "Research" laemmel [PU joost, PU marlow],
            D "Strategy" blair   []]

-- A typo for the sake of testing equality;
-- (cf. lammel vs. laemmel)
genCom' :: Company
genCom' = C [D "Research" lammel [PU joost, PU marlow],
             D "Strategy" blair   []]

genCom'' :: Company
genCom'' = C [D "Research" laemmel [PU joost, PU marlow]]

lammel, laemmel, joost, blair :: Employee
lammel  = E (P "Lammel" "Amsterdam") (S 8000)
laemmel = E (P "Laemmel" "Amsterdam") (S 8000)
joost   = E (P "Joost"   "Amsterdam") (S 1000)
marlow  = E (P "Marlow"  "Cambridge") (S 2000)
blair   = E (P "Blair"   "London")    (S 100000)

-- Some more test data
person1 = P "Lazy" "Home"
dept1   = D "Useless" (E person1 undefined) []



