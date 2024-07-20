> module GMsec2 ( module GMsec2 )
> where
> import Prettier hiding (Pretty, pretty, prettyList, char, int, block, render, float)
> import qualified Prettier
> import CompanyDatatypes hiding (Unit)

> line                          :: Doc
> line                          =  nl

> prettyChar                    :: Char -> Doc
> prettyChar                    =  Prettier.pretty

> prettyString                  :: String -> Doc
> prettyString                  =  Prettier.pretty

> prettyInt                     :: Int -> Doc
> prettyInt                     =  Prettier.int

> render                        :: Int -> Doc -> IO ()
> render n d                    =  putStrLn (Prettier.render (Page n) d)

> o                             :: (b -> c) -> (a -> b) -> (a -> c)
> o                             =  (.)

copied from Section 2:

> data Unit                     =  Unit
>
> data Plus a b                 =  Inl a | Inr b
>
> data Pair a b                 =  Pair { outl :: a, outr:: b }

> data Isomorphism a b          =  Iso { fromData :: b -> a, toData :: a -> b }

> data Tree a                   =  Leaf a | Fork (Tree a) (Tree a)

> fromList                      :: [a] -> Plus (Constr Unit) (Constr (Pair a [a]))
> fromList []                   =  Inl (Constr "[]" 0 Unit)
> fromList (x : xs)             =  Inr (Constr "(:)" 2 (Pair x xs))
>
> toList                        :: Plus (Constr Unit) (Constr (Pair a [a])) -> [a]
> toList (Inl (Constr _ _ Unit))             =  []
> toList (Inr (Constr _ _ (Pair x xs)))      =  x : xs

> class TypeRep a where
>   typeRep                     :: (Generic g) => g a

> instance TypeRep Unit where
>   typeRep                     =  unit
> instance (TypeRep a, TypeRep b) => TypeRep (Plus a b) where
>   typeRep                     =  plus
> instance (TypeRep a, TypeRep b) => TypeRep (Pair a b) where
>   typeRep                     =  pair
> instance TypeRep Char where
>   typeRep                     =  char
> instance TypeRep Int where
>   typeRep                     =  int
> instance TypeRep Float where
>   typeRep                     =  float

copied from Sections 2.4.1 - 2.4.3:

> data Constr a                 =  Constr { name :: String, arity :: Int, arg :: a }

> class Generic g where
>   unit                        :: g Unit
>   plus                        :: (TypeRep a, TypeRep b) => g (Plus a b)
>   pair                        :: (TypeRep a, TypeRep b) => g (Pair a b)
>   datatype                    :: (TypeRep a) => Isomorphism a b -> g b
>   char                        :: g Char
>   int                         :: g Int
>   float                       :: g Float
>
>   list                        :: (TypeRep a) => g [a]
>   list                        =  datatype (Iso fromList toList)
>
>   constr                      :: (TypeRep a) => g (Constr a)
>   constr                      =  datatype (Iso arg (Constr "" (-1)))

>   catchall                    :: (TypeRep a) => g a
>   unit                        =  catchall
>   plus                        =  catchall
>   pair                        =  catchall
>   char                        =  catchall
>   int                         =  catchall
>   float                       =  catchall

> instance (TypeRep a) => TypeRep [a] where
>   typeRep                     =  list
> instance (TypeRep a) => TypeRep (Constr a) where
>   typeRep                     =  constr
> instance (TypeRep a) => TypeRep (Tree a) where
>   typeRep                     =  datatype (Iso fromTree toTree)

> fromTree                      :: Tree a -> Plus (Constr a) (Constr (Pair (Tree a) (Tree a)))
> fromTree (Leaf x)             =  Inl (Constr "Leaf" 1 x)
> fromTree (Fork l r)           =  Inr (Constr "Fork" 2 (Pair l r))

> toTree                        :: Plus (Constr a) (Constr (Pair (Tree a) (Tree a))) -> Tree a
> toTree (Inl (Constr _ _ x))   =  Leaf x
> toTree (Inr (Constr _ _ (Pair l r)))  
>                               =  Fork l r

> newtype Pretty a              =  Pretty { applyPretty :: a -> Doc }
>
> pretty                        :: (TypeRep a) => a -> Doc
> pretty                        =  applyPretty typeRep

> block                         :: Doc -> Doc
> block d                       =  group (nest 1 d)

> lefty                         :: Int -> Tree Int
> lefty 0                       =  Leaf 0
> lefty (n + 1)                 =  Fork (lefty n) (Leaf (n + 1))

> prettyl                       :: (a -> Doc) -> ([a] -> Doc)
> prettyl _p []                 =  text "[]"
> prettyl p (a : as)            =  group (nest 1 (text "[" <> p a <> rest as))
>   where rest []               =  text "]"
>         rest (x : xs)         =  text "," <> line <> p x <> rest xs

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
Mutual recursion
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> instance Generic Pretty where
>   unit                        =  Pretty (\ _x -> empty)
>   plus                        =  Pretty (\ x -> case x of
>                                                 Inl l -> pretty l
>                                                 Inr r -> pretty r)
>   pair                        =  Pretty (\ x -> pretty (outl x) <> line <> pretty (outr x))
>   datatype iso                =  Pretty (\ x -> pretty (fromData iso x))
>   char                        =  Pretty (\ x -> prettyChar x)
>   int                         =  Pretty (\ x -> prettyInt  x)
>   float                       =  error "Does not work for float yet"
>   constr                      =  Pretty (\ x -> if arity x == 0 then
>                                                     text (name x)
>                                                 else
>                                                     block (text "(" <> text (name x) <> line 
>                                                            <> pretty (arg x) <> text ")"))
>   list                        =  Pretty (\ x -> prettyList x)
>   catchall                    =  error "Does not work for catchall yet"

> newtype PrettyList a          =  PrettyList { applyPrettyList :: [a] -> Doc }
>
> prettyList                    :: (TypeRep a) => [a] -> Doc
> prettyList                    =  applyPrettyList typeRep
>
> instance Generic PrettyList where
>   char                        =  PrettyList (\ x -> prettyString x)
>   datatype iso                =  PrettyList (\ x -> prettyl prettyd x)
>     where prettyd             =  pretty `o` fromData iso
>   list                        =  catchall
>   catchall                    =  PrettyList (\ x -> prettyl pretty x)

Try:
<< render 80 (pretty ['L', 'i', 's', 'a'])
<< render 80 (pretty [Leaf "a", Fork (Leaf "tr") (Leaf "ee")])
<< render 80 (pretty [Leaf [Leaf 'a']])