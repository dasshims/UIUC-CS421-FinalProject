> module GM ( module GM )
> where
> import CompanyDatatypes hiding (Unit)

> data Unit                     =  Unit
>
> data Plus a b                 =  Inl a | Inr b
>
> data Pair a b                 =  Pair { outl :: a, outr:: b }

> data Isomorphism a b          =  Iso { fromData :: b -> a, toData :: a -> b }

> data Tree a                   =  Leaf a | Fork (Tree a) (Tree a) deriving Show

> data Constr a                 =  Constr { name :: String, arity :: Int, arg :: a }

>
> fromTree (Leaf x)             =  Inl x
> fromTree (Fork l r)           =  Inr (Pair l r)
>
> toTree (Inl x)                =  Leaf x
> toTree (Inr (Pair l r))       =  Fork l r

> fromList []                   =  Inl Unit
> fromList (x : xs)             =  Inr (Pair x xs)
>
> toList (Inl Unit)             =  []
> toList (Inr (Pair x xs))      =  x : xs

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
Defining a generic function
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> newtype Count a1 a2           =  Count { applyCount :: a1 -> Int }
>
> instance Generic Count where
>   unit                        =  Count (\ _x -> 0)
>   plus a b                    =  Count (\ x ->  case x of
>                                                 Inl l  ->  applyCount a l
>                                                 Inr r  ->  applyCount b r)
>   pair a b                    =  Count (\ x ->  applyCount a (outl x)
>                                                 + applyCount b (outr x))
>   datatype iso1 iso2 a
>                               =  Count (\ x -> applyCount a (fromData iso1 x))
>   char                        =  Count (\ _x -> 0)
>   int                         =  Count (\ _x -> 0)
>   float                       =  Count (\ _x -> 0)

> size                          :: (FunctorRep f) => f a -> Int
> size                          =  applyCount (functorRep (Count (\ _x -> 1)))

> sum                           :: (FunctorRep f) => f Int -> Int
> sum                           =  applyCount (functorRep (Count (\ x -> x)))

Try:
<< let xss = [ [i * j | j <- [i .. 9]] | i <- [0 .. 9] ]
<< size xss
<< let base = Count (\ _x -> 1)
<< applyCount (list (list base)) xss
<< applyCount (list base) xss
<< applyCount base xss

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
Introducing a new type
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> tree                          :: (Generic g) => g a1 a2 -> g (Tree a1) (Tree a2)
> tree a                        =  datatype  (Iso fromTree toTree) (Iso fromTree toTree)
>                                            (a <+> tree a <*> tree a)

> infixr 3  <*>
> infixr 2  <+>
>
> a <+> b                       =  plus a b
> a <*> b                       =  pair a b

> instance (TypeRep a) => TypeRep (Tree a) where
>   typeRep                     =  tree typeRep
>
> instance FunctorRep Tree where
>   functorRep                  =  tree

> list                          :: (Generic g) => g a1 a2 -> g [a1] [a2]
> list a                        =  datatype (Iso fromList toList) (Iso fromList toList)
>                                           (unit <+> a <*> list a)
>
> instance (TypeRep a) => TypeRep [a] where
>   typeRep                     =  list typeRep
>
> instance FunctorRep [] where
>   functorRep                  =  list

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
Implementation
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> class Generic g where
>   unit                        :: g Unit Unit
>   plus                        :: g a1 a2 -> g b1 b2 -> g (Plus a1 b1) (Plus a2 b2)
>   pair                        :: g a1 a2 -> g b1 b2 -> g (Pair a1 b1) (Pair a2 b2)
>   datatype                    :: Isomorphism a1 b1 -> Isomorphism a2 b2 -> g a1 a2 -> g b1 b2
>   char                        :: g Char Char
>   int                         :: g Int Int
>   float                       :: g Float Float
>   constr                      :: g a1 a2 -> g (Constr a1) (Constr a2)
>   constr                      =  datatype (Iso arg (Constr "" (-1))) (Iso arg (Constr "" (-1)))
>
> class TypeRep a where
>   typeRep                     :: (Generic g) => g a a
> instance TypeRep Unit where
>   typeRep                     =  unit
> instance (TypeRep a, TypeRep b) => TypeRep (Plus a b) where
>   typeRep                     =  typeRep <+> typeRep
> instance (TypeRep a, TypeRep b) => TypeRep (Pair a b) where
>   typeRep                     =  typeRep <*> typeRep
> instance TypeRep Char where
>   typeRep                     =  char
> instance TypeRep Int where
>   typeRep                     =  int
> instance TypeRep Float where
>   typeRep                     =  float

> class FunctorRep f where
>   functorRep                  :: (Generic g) => g a1 a2 -> g (f a1) (f a2)

