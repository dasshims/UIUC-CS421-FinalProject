> {-# OPTIONS -fglasgow-exts #-}

> module GMapQ where 

> import LIGD hiding (Id(..))
> import CompanyDatatypes
> import CompanyReps
> import Control.Applicative
> import Common -- Applicative instance for Id

SYB-like combinators. Here a generic function is applied
to all the arguments of a constructor. The function would
be simpler (no silly |isTopLevel| argument) if the products
would be more spine-like. See Generic Views paper, the SYB section.

> gmapA :: Applicative f => Bool -> (forall tT.Rep tT -> tT -> f tT) -> Rep tT -> tT -> f tT
> gmapA isTopLevel f (RCon  s rA)      t
>            = gmapA isTopLevel f rA t
> gmapA True f (RType e rA   ep) t 
>            = liftA (to ep) (gmapA False f rA (from ep t))
> gmapA isTopLevel f (RSum rA rB   ep) t 
>            =  case from ep t of
>               Inl a -> liftA (to ep . Inl) (gmapA isTopLevel f rA a)
>               Inr b -> liftA (to ep . Inr) (gmapA isTopLevel f rB b)
> gmapA isTopLevel f (RPair rA rB  ep) t 
>            =  case from ep t of
>               (a :*: b) -> liftA2 (\x y -> to ep (x:*:y))
>                            (gmapA isTopLevel f rA a)
>                            (gmapA isTopLevel f rB b)
> gmapA isTopLevel f rep val 
>            = f rep val

A query is obtained by using the instance for |Const c|, where
c is a monoid. In this particular case, c is the list monoid.

> gmapQ :: (forall tT.Rep tT -> tT -> a) -> Rep tT -> tT -> [a]
> gmapQ f rep x = getConst (gmapA True (\rep x -> Const [f rep x]) rep x)

Here we use the Applicative instance for Id, which yields
a transformation.

> gmapT :: (forall tT.Rep tT -> tT -> tT) -> Rep tT -> tT -> tT
> gmapT f rep x = unId (gmapA True (\rep x -> Id (f rep x)) rep x)

> everywhere :: (forall tT.Rep tT -> tT -> tT) -> Rep tT -> tT -> tT
> everywhere f rep = f rep . gmapT (everywhere f) rep


