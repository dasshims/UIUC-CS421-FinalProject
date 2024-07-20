{-# LANGUAGE GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , TypeFamilies
           , TypeOperators
           , FlexibleInstances
           , EmptyDataDecls
           #-}
module CompanyReps where

import Generics.MultiRec hiding (C)
import qualified Generics.MultiRec as MR
import CompanyDatatypes

data CompanyU :: * -> * where
    Company  :: CompanyU Company
    DeptList :: CompanyU [Dept]
    Dept     :: CompanyU Dept
    UnitList :: CompanyU [Unit]
    Unit     :: CompanyU Unit
    Employee :: CompanyU Employee
    Person   :: CompanyU Person
    Salary   :: CompanyU Salary
    String   :: CompanyU String

data C
instance Constructor C where conName _ = "C"
data D
instance Constructor D where conName _ = "D"
data PU
instance Constructor PU where conName _ = "PU"
data DU
instance Constructor DU where conName _ = "DU"
data E
instance Constructor E where conName _ = "E"
data P
instance Constructor P where conName _ = "P"
data S
instance Constructor S where conName _ = "S"
data SCons
instance Constructor SCons where conName _ = "(:)"
data SNil
instance Constructor SNil where conName _ = "[]"

type NilT = MR.C SNil U
type ConsT t = MR.C SCons (I t :*: I [t])
type ConsTK t = MR.C SCons (K t :*: I [t])

type instance PF CompanyU = MR.C C (I [Dept]) :>: Company
                   :+: NilT :>: [Dept]
                   :+: ConsT Dept :>: [Dept]
                   :+: MR.C D (I String :*: I Employee :*: I [Unit]) :>: Dept
                   :+: NilT :>: [Unit]
                   :+: ConsT Unit :>: [Unit]
                   :+: MR.C PU (I Employee) :>: Unit
                   :+: MR.C DU (I Dept) :>: Unit
                   :+: MR.C E (I Person :*: I Salary) :>: Employee
                   :+: MR.C P (I String :*: I String) :>: Person
                   :+: MR.C S (K Float) :>: Salary
                   :+: ConsTK Char :>: String
                   :+: NilT :>: String

instance Ix CompanyU Company where
    from_ (C ds) = L (Tag (MR.C (I (I0 ds))))
    to_ (L (Tag (MR.C (I (I0 ds))))) = C ds
    index = Company

instance Ix CompanyU [Dept] where
    from_ [] = R (L (Tag (MR.C U)))
    from_ (d : ds) = R (R (L (Tag (MR.C (I (I0 d) :*: I (I0 ds))))))
    to_ (R (L (Tag (MR.C U)))) = []
    to_ (R (R (L (Tag (MR.C ((I (I0 d) :*: I (I0 ds)))))))) = d : ds
    index = DeptList

instance Ix CompanyU Dept where
    from_ (D n m us) = R (R (R (L (Tag (MR.C (I (I0 n) :*: I (I0 m) :*: I (I0 us)))))))
    to_ (R (R (R (L (Tag (MR.C (I (I0 n) :*: I (I0 m) :*: I (I0 us)))))))) = D n m us
    index = Dept

instance Ix CompanyU [Unit] where
    from_ [] = R (R (R (R (L (Tag (MR.C U))))))
    from_ (u : us) = R (R (R (R (R (L (Tag (MR.C (I (I0 u) :*: I (I0 us)))))))))
    to_ (R (R (R (R (L (Tag (MR.C U))))))) = []
    to_ (R (R (R (R (R (L (Tag (MR.C (I (I0 u) :*: I (I0 us)))))))))) = u : us
    index = UnitList

instance Ix CompanyU Unit where
    from_ (PU e) = R (R (R (R (R (R (L (Tag (MR.C (I (I0 e))))))))))
    from_ (DU d) = R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 d)))))))))))
    to_ (R (R (R (R (R (R (L (Tag (MR.C (I (I0 e))))))))))) = PU e
    to_ (R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 d)))))))))))) = DU d
    index = Unit

instance Ix CompanyU Employee where
    from_ (E p s) = R (R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 p) :*: I (I0 s))))))))))))
    to_ (R (R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 p) :*: I (I0 s))))))))))))) = E p s
    index = Employee

instance Ix CompanyU Person where
    from_ (P n a) = R (R (R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 n) :*: I (I0 a)))))))))))))
    to_ (R (R (R (R (R (R (R (R (R (L (Tag (MR.C (I (I0 n) :*: I (I0 a)))))))))))))) = P n a
    index = Person

instance Ix CompanyU Salary where
    from_ (S s) = R (R (R (R (R (R (R (R (R (R (L (Tag (MR.C (K s)))))))))))))
    to_ (R (R (R (R (R (R (R (R (R (R (L (Tag (MR.C (K s)))))))))))))) = S s
    index = Salary

instance Ix CompanyU String where
    from_ (c:cs) = R (R (R (R (R (R (R (R (R (R (R (L (Tag (MR.C (K c :*: I (I0 cs)))))))))))))))
    from_ ""     = R (R (R (R (R (R (R (R (R (R (R (R (Tag (MR.C U)))))))))))))
    to_ (R (R (R (R (R (R (R (R (R (R (R (L (Tag (MR.C (K c :*: I (I0 cs)))))))))))))))) = c:cs
    to_ (R (R (R (R (R (R (R (R (R (R (R (R (Tag (MR.C U)))))))))))))) = ""
    index = String

-- instance Eq_ CompanyU where
--     eq_ Company Company = Just Refl
--     eq_ DeptList DeptList = Just Refl
--     eq_ Dept Dept = Just Refl
--     eq_ UnitList UnitList = Just Refl
--     eq_ Unit Unit = Just Refl
--     eq_ Employee Employee = Just Refl
--     eq_ Person Person = Just Refl
--     eq_ Salary Salary = Just Refl
--     eq_ _ _ = Nothing
