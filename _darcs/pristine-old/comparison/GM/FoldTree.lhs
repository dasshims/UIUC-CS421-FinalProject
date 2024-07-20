> module FoldTree where

> import GMsec2
> import TreeDatatype
> import TreeReps
> import CompanyDatatypes
> import CompanyReps


Accumulate all ints in a datastructure
just a simple type-indexed function. Not protected by a type class.

> newtype ListifyIntG a         = ListifyIntG { applyListifyIntG :: a -> [Int] }

> listifyInt                    :: (TypeRep a) => a -> [Int]
> listifyInt                    =  applyListifyIntG typeRep

> instance Generic ListifyIntG where
>   unit                        =  ListifyIntG (\ x -> [])
>   plus                        =  ListifyIntG (\ x -> case x of
>                                                 Inl l -> listifyInt l
>                                                 Inr r -> listifyInt r)
>   pair                        =  ListifyIntG (\ x -> listifyInt (outl x) ++
>                                                      listifyInt (outr x))
>   datatype iso                =  ListifyIntG (\ x -> listifyInt (fromData iso x))
>   int                         =  ListifyIntG (\ x -> [x])
>   catchall                    =  ListifyIntG (\ x -> [])

> newtype ListifySalaryG a      = ListifySalaryG { applyListifySalaryG :: a -> [a] }


> listifySalary                 =  error "Not possible using the GM library"
