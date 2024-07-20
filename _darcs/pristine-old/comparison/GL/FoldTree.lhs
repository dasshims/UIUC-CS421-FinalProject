> {-# OPTIONS -fglasgow-exts #-}

> module FoldTree where

> import GL hiding (Fork, Tree)
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps
> import Data.Generics hiding (Generic)


Accumulate all ints in a datastructure
just a simple type-indexed function. Not protected by a type class.

> newtype ListifyInt a         = ListifyIntG { appListifyInt :: a -> [Int] }

> instance Generic ListifyInt where
>   unit                        =  ListifyIntG (\ x -> [])
>   plus a b                    =  ListifyIntG (\ x -> case x of
>                                                      Inl l -> appListifyInt a l
>                                                      Inr r -> appListifyInt b r)
>   prod a b                    =  ListifyIntG (\ x -> appListifyInt a (outl x) ++
>                                                      appListifyInt b (outr x))
>   view iso a                  =  ListifyIntG (\ x -> appListifyInt a (from iso x))
>   int                         =  ListifyIntG (\ x -> [x])
>   char                        =  ListifyIntG (\ x -> [])
>   float                       =  ListifyIntG (\ x -> [])
> instance GenericCompany ListifyInt where

> listifyInt :: GRep ListifyInt a => a -> [Int]
> listifyInt = appListifyInt over


> newtype ListifyFloat a         = ListifyFloatG { appListifyFloat :: a -> [Float] }

> instance Generic ListifyFloat where
>   unit                        =  ListifyFloatG (\ x -> [])
>   plus a b                    =  ListifyFloatG (\ x -> case x of
>                                                        Inl l -> appListifyFloat a l
>                                                        Inr r -> appListifyFloat b r)
>   prod a b                    =  ListifyFloatG (\ x -> appListifyFloat a (outl x) ++
>                                                        appListifyFloat b (outr x))
>   view iso a                  =  ListifyFloatG (\ x -> appListifyFloat a (from iso x))
>   int                         =  ListifyFloatG (\ x -> [])
>   char                        =  ListifyFloatG (\ x -> [])
>   float                       =  ListifyFloatG (\ x -> [x])
> instance GenericCompany ListifyFloat where

> listifyFloat :: GRep ListifyFloat a => a -> [Float]
> listifyFloat = appListifyFloat over


> newtype ListSalary a          = ListSalary { listSalary :: a -> [Salary] }

> instance Generic ListSalary where
>   unit                        =  ListSalary (\ x -> [])
>   plus a b                    =  ListSalary (\ x -> case x of
>                                                     Inl l -> listSalary a l
>                                                     Inr r -> listSalary b r)
>   prod a b                    =  ListSalary (\ x -> listSalary a (outl x) ++
>                                                     listSalary b (outr x))
>   view iso a                  =  ListSalary (\ x -> listSalary a (from iso x))
>   int                         =  ListSalary (\ x -> [])
>   char                        =  ListSalary (\ x -> [])
>   float                       =  ListSalary (\ x -> [])
> instance GenericCompany ListSalary where
>   salary                      =  ListSalary (\ x -> [x])

> listifySalary :: GRep ListSalary a => a -> [Salary]
> listifySalary =  listSalary over

> mysal :: Tree Salary Int
> mysal = Fork (WithWeight (Leaf (S 1.0)) 1)
>               (WithWeight (Fork (Leaf (S 2.0)) (Leaf (S 2.3))) 2)


