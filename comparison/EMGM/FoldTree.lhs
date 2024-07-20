> {-# OPTIONS_GHC -fglasgow-exts #-}

> module FoldTree where

> import GL hiding (Fork, Tree)
> import Data.Generics((:+:)(Inl, Inr)) -- should be exported from GL
> -- import Data.Generics hiding (Generic, gmapQ)
> import TreeDatatype
> import TreeReps(GenericWTree)
> import CompanyDatatypes
> import CompanyReps(GenericCompany(salary))
> import qualified PerfectDatatype as P
> import PerfectReps
> import GMapQ(gmapQ)

Accumulate all ints in a datastructure
just a simple type-indexed function. Not protected by a type class.

> newtype SelectInt a = SelectIntG { appSelectInt :: a -> [Int] }

> instance Generic SelectInt where
>   unit        =  SelectIntG (\ x -> [])
>   plus a b    =  SelectIntG (\ x -> case x of
>                                      Inl l -> appSelectInt a l
>                                      Inr r -> appSelectInt b r)
>   prod a b    =  SelectIntG (\ x -> appSelectInt a (outl x) ++
>                                     appSelectInt b (outr x))
>   view iso a  =  SelectIntG (\ x -> appSelectInt a (from iso x))
>   int         =  SelectIntG (\ x -> [x]) -- ** only special case
>   char        =  SelectIntG (\ x -> [])
>   float       =  SelectIntG (\ x -> [])
> instance GenericList    SelectInt
> instance GenericCompany SelectInt
> instance GenericWTree   SelectInt

> selectIntWTree :: GRep SelectInt a => a -> [Int]
> selectIntWTree = appSelectInt over

> selectIntPerfect :: GRep SelectInt a => a -> [Int]
> selectIntPerfect = appSelectInt over

> newtype SelectSalaryG a = SelectSalaryG { selectSalaryG :: a -> [Salary] }

> instance Generic SelectSalaryG where
>   unit        =  SelectSalaryG (\ x -> [])
>   plus a b    =  SelectSalaryG (\ x -> case x of
>                                     Inl l -> selectSalaryG a l
>                                     Inr r -> selectSalaryG b r)
>   prod a b    =  SelectSalaryG (\ x -> selectSalaryG a (outl x) ++
>                                        selectSalaryG b (outr x))
>   view iso a  =  SelectSalaryG (\ x -> selectSalaryG a (from iso x))
>   int         =  SelectSalaryG (\ x -> [])
>   char        =  SelectSalaryG (\ x -> [])
>   float       =  SelectSalaryG (\ x -> [])
> instance GenericList SelectSalaryG
> instance GenericCompany SelectSalaryG where
>   salary      =  SelectSalaryG (\ x -> [x]) -- ** only special case

> selectSalary :: GRep SelectSalaryG a => a -> [Salary]
> selectSalary =  selectSalaryG over

> mysal :: WTree Salary Int
> mysal = Fork (WithWeight (Leaf (S 1.0)) 1)
>              (WithWeight (Fork (Leaf (S 2.0)) (Leaf (S 2.3))) 2)

> gapplySelectCompanies :: [Company] -> [[Salary]]
> gapplySelectCompanies = gmapQ selectSalaryG


-- ----------------------------------------------------------------

> newtype SelectFloat a = SelectFloatG { appSelectFloat :: a -> [Float] }

> instance Generic SelectFloat where
>   unit        =  SelectFloatG (\ x -> [])
>   plus a b    =  SelectFloatG (\ x -> case x of
>                                        Inl l -> appSelectFloat a l
>                                        Inr r -> appSelectFloat b r)
>   prod a b    =  SelectFloatG (\ x -> appSelectFloat a (outl x) ++
>                                        appSelectFloat b (outr x))
>   view iso a  =  SelectFloatG (\ x -> appSelectFloat a (from iso x))
>   int         =  SelectFloatG (\ x -> [])
>   char        =  SelectFloatG (\ x -> [])
>   float       =  SelectFloatG (\ x -> [x])
> instance GenericList    SelectFloat
> instance GenericCompany SelectFloat

> selectFloat :: GRep SelectFloat a => a -> [Float]
> selectFloat = appSelectFloat over


