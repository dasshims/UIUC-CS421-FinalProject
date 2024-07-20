> {-# OPTIONS_GHC -fglasgow-exts #-}

> module GMapQ where

> import GL
> import Data.Generics hiding (Generic, gmapQ)
> import CompanyReps


This function is like SYB's gmapQ. It applies a generic function
to every field of a constructor.
The isTopLevel field is used to ensure that this is a shallow
mapping. This function would be simplified if the products would
be more spine-like. See Generic Views paper, the SYB section.

This is for the test of higher-orderness.

This function is buggy because it doesn't work for ad-hoc definitions
outside the Generic type-class.
The view case should not transform to the structure representation if
not at the top level anymore. In that case, it should instead call the
generic function argument.

Another problem lies with the representation of BinTree: it transforms
to the structure representation in a deep way. This makes it harder to
write GMapQ. This should be fixable by changing the representation though.

> data GMapQ g a = GMapQ {
>     genFunc      :: g a, 
>     applyGMapQ  :: forall b . Bool -> (forall a . g a -> a -> b) -> a -> [b]}

> instance Generic g => Generic (GMapQ g) where
>   unit             =  GMapQ (unit) (\ _b f u -> [f unit u])
>   int              =  GMapQ (int) (\ _b f u -> [f int u])
>   float            =  GMapQ (float) (\ _b f u -> [f float u])
>   char             =  GMapQ (char) (\ _b f u -> [f char u]) 
>   prod a b         =  GMapQ (prod (genFunc a) (genFunc b)) 
>                              (\ isTop f x -> applyGMapQ a isTop f (outl x)
>                                              ++
>                                              applyGMapQ b isTop f (outr x)) 
>   plus a b         =  GMapQ (plus (genFunc a) (genFunc b))
>                              (\ isTop f x -> case x of
>                                                 Inl l -> applyGMapQ a isTop f l
>                                                 Inr r -> applyGMapQ b isTop f r)
>   view iso a       =  GMapQ (view iso (genFunc a))
>                              (\ isTop f x -> if isTop
>                                              then applyGMapQ a False f (from iso x)
>                                              else [f (genFunc a) (from iso x)])
>   constr name arity a = GMapQ (constr name arity (genFunc a))
>                                (applyGMapQ a)

> gmapQ :: GRep (GMapQ g) a => (forall a. g a -> a -> b) -> a -> [b]
> gmapQ = applyGMapQ over True


Note: The argument of GMapQ might have an ad-hoc case. This means that it is not enough
calling it in standard cases above (and in fact calling it from view might be a bug?).
So, we must write a painful case like below for all possible ad-hoc invocation points.
Below, we give a solution that works only for Salary.

Wanted: A better solution that works! 

> instance (GenericList g, GenericCompany g) => GenericCompany (GMapQ g) where
>   salary  = GMapQ farg
>                   (\ isTop f x -> if isTop
>                                     -- isTop distinguishes a Salary argument from a
>                                     -- Salary occurrence as a constructor field
>                                     -- e.g.
>                                     --   gMapQ selectSalary (S 1.0) => [[]]
>                                     --   gMapQ selectSalary (S 1.0,S 2.0) => [[1.0],[2.0]]
>                                   then applyGMapQ salary isTop f x
>                                   else [f farg x])
>     where
>       farg = salary -- invokes ad-hoc case from h.o. function argument

Note: We are assuming that the querying argument does not have an ad-hoc case
for lists!

> instance GenericList g => GenericList (GMapQ g)

