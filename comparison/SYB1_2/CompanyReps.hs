{-# OPTIONS_GHC -XStandaloneDeriving -fglasgow-exts #-}

module CompanyReps where


import Data.Generics hiding (Unit)
import CompanyDatatypes

-- CompanyDatatypes no longer contains instances for Data and Typeable
-- otherwise it clashes with other typeclasses named the same way (as in
-- syb3).
-- We use GHC's standalone deriving to generate them.

deriving instance Typeable Company 
deriving instance Typeable Dept    
deriving instance Typeable Unit    
deriving instance Typeable Employee
deriving instance Typeable Person  
deriving instance Typeable Salary  

deriving instance Data Company 
deriving instance Data Dept    
deriving instance Data Unit    
deriving instance Data Employee
deriving instance Data Person  
deriving instance Data Salary  
