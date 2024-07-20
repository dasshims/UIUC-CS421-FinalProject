{-# LINE 71 "SYB1.lhs" #-}
  {-#  OPTIONS_GHC -fglasgow-exts  #-}
{-# LINE 74 "SYB1.lhs" #-}
  module SYB1 where
{-# LINE 76 "SYB1.lhs" #-}
  import qualified Prelude
  import Prelude hiding (show, shows, showsPrec,
                         read, reads, readsPrec,
                         map, succ)
  import List (sortBy)
  import CompanyDatatypes
  import qualified BinTreeDatatype as B
  import qualified TreeDatatype as T
  import qualified PerfectDatatype as P
  import GRoseDatatype
  import NGRoseDatatype
{-# LINE 82 "SYB1.lhs" #-}
  mypair  =  (,)
  dots = undefined
{-# LINE 85 "SYB1.lhs" #-}
  instance Functor Tree where
    fmap f Empty         =  Empty
    fmap f (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)
{-# LINE 89 "SYB1.lhs" #-}
  myshowChar :: Char -> String
  myshowChar = Prelude.show
  showInt :: Int -> String
  showInt = Prelude.show
{-# LINE 94 "SYB1.lhs" #-}
  showsPrecChar :: Int -> Char -> ShowS
  showsPrecChar = Prelude.showsPrec
  showsPrecInt :: Int -> Int -> ShowS
  showsPrecInt = Prelude.showsPrec
  showsPrecString :: Int -> String -> ShowS
  showsPrecString = Prelude.showsPrec
{-# LINE 101 "SYB1.lhs" #-}
  readsPrecChar    ::  Int -> ReadS Char
  readsPrecChar    =   Prelude.readsPrec
  readsPrecInt     ::  Int -> ReadS Int
  readsPrecInt     =   Prelude.readsPrec
  readsPrecString  ::  Int -> ReadS String
  readsPrecString  =   Prelude.readsPrec
{-# LINE 108 "SYB1.lhs" #-}
  mapList = Prelude.map
  mapTree :: (a -> b) -> (Tree a -> Tree b)
  mapTree = fmap
  sumList = Prelude.sum
{-# LINE 300 "SYB1.lhs" #-}
  data Type ::  * ->  * where
    CharR     ::  Type Char
    IntR      ::  Type Int
    ListR     ::  Type a -> Type [ a]
    PairR     ::  Type a -> Type b -> Type ( a, b)
{-# LINE 2 "Type" #-}
    BoolR    ::  Type Bool
{-# LINE 2 "Type" #-}
    ExprR     ::  Type a -> Type (Expr a)
{-# LINE 2 "Type" #-}
    SetR      ::  Type a -> Type [ a]
{-# LINE 2 "Type" #-}
    PerfectR  ::  Type a -> Type (Perfect a)
    PPerfectR ::  Type a -> Type (P.Perfect a)
{-# LINE 2 "Type" #-}
    TreeR     ::  Type a -> Type (Tree a)
    BinTreeR  ::  Type a -> Type (B.BinTree a) -- added by patrikj
{- Company datatypes -}
    CompanyR  ::  Type Company
    DeptR     ::  Type Dept
    UnitR     ::  Type Unit
    EmployeeR ::  Type Employee
    PersonR   ::  Type Person
    SalaryR   ::  Type Salary
    FloatR    ::  Type Float
{- GRose -}
    GRoseR    ::  (forall a.Type a -> Type (f a)) ->
                  Type a ->
                  Type (GRose f a)
{- NGRose -}
    NGRoseR   ::  (forall a.Type a -> Type (f a)) ->
                  Type a ->
                  Type (NGRose f a)
    CompR     ::  (forall a.Type a -> Type (f a)) -> (forall a.Type a -> Type (g a)) ->
                  Type a -> Type (Comp f g a)
{- The other tree -}
    TreeWR    ::  Type a -> Type w -> Type (T.WTree a w)
{- Fork -}
    ForkR     ::  Type a -> Type (P.Fork a)

{-# LINE 336 "SYB1.lhs" #-}
  infixl 1 :>
  data Typed a  =  (:>) { typeOf :: Type a, val :: a }
{-# LINE 362 "SYB1.lhs" #-}
  showsPrec ::  Int -> Typed a -> ShowS
  showsPrec d (CharR :>            c   )  =   showsPrecChar    d c
  showsPrec d (IntR :>             n   )  =   showsPrecInt     d n
-- Note: Show must not show lists or strings differently anymore, see TestGShow.lhs
--  showsPrec d (ListR   CharR :>  s   )  =   showsPrecString  d s
{-# LINE 370 "SYB1.lhs" #-}
--  showsPrec d (ListR  a :> xs)          =   showsList [ shows (a :>  x) | x <- xs ]
  showsPrec d (PairR  a b :> (x, y))
    =   showChar '('  .  shows (a :>  x) . showChar ','
                      .  shows (b :>  y) . showChar ')'
{-# LINE 2 "showsPrec" #-}
  showsPrec d (TreeR  a :> Empty)       =   showString "Empty"
  showsPrec d (TreeR  a :> Node l x r)
    =   showParen (d > 10) (showString "Node"  <> showsPrec 11 (TreeR  a :> l)
                                               <> showsPrec 11 (a :>  x)
                                               <> showsPrec 11 (TreeR  a :> r))
{- Not originally in -}
  showsPrec d (FloatR :>             n   )  =   showsPrecFloat     d n

{-# LINE 2 "showsPrec" #-}
  showsPrec d x = showParen (arity x > 0 && d > 10) (showsSpine (toSpine x))

{- Not originally in -}
  showsPrecFloat :: Int -> Float -> ShowS
  showsPrecFloat = Prelude.showsPrec


{-# LINE 4 "showsPrec" #-}
  showsSpine :: Spine a -> ShowS
  showsSpine (Con c)   =  showString (name c)
  showsSpine (f :$ x)  =  showsSpine f <> showsPrec 11 x
{-# LINE 405 "SYB1.lhs" #-}
  shows   ::  Typed a -> ShowS
--  shows   =   showsPrec 0
  shows   =   showsPrec 11 -- The priority was changed from 0 to 11 to,
                           -- print a parenthesis around pretty printed values.
                           -- This is to have the same behaviour as the other tested libraries,
                           -- we indeed go to the lowest common denominator.

  show    ::  Typed a -> String
  show x  =   shows x ""
{-# LINE 420 "SYB1.lhs" #-}
  data Tree a  =  Empty | Node (Tree a) a (Tree a)
{-# LINE 423 "SYB1.lhs" #-}
                   deriving (Show)
{-# LINE 488 "SYB1.lhs" #-}
  infixl  0  :$

  data Spine ::  * ->  * where
    Con   ::  Constr a -> Spine a
    (:$)  ::  Spine (a -> b) -> Typed a -> Spine b
{-# LINE 517 "SYB1.lhs" #-}
  data Constr a = Constr { constr :: a, name ::  String }
{-# LINE 523 "SYB1.lhs" #-}
  fromSpine :: Spine a -> a
  fromSpine (Con c)   =  constr c
  fromSpine (f :$ x)  =  (fromSpine f) (val x)
{-# LINE 547 "SYB1.lhs" #-}
  toSpine :: Typed a -> Spine a
  toSpine (TreeR  a :> Empty)         =  Con empty
  toSpine (TreeR  a :> Node l x r)  =  Con node :$ (TreeR  a :> l) :$ (a :>  x) :$ (TreeR  a :> r)
{-# LINE 553 "SYB1.lhs" #-}
  toSpine (IntR :>  i)                  =  Con (int i)
  toSpine (CharR :>  c)                 =  Con (char c)
  toSpine (PairR  a b :> (x, y))    =  Con pair :$ (a :>  x) :$ (b :>  y)
  toSpine (ListR  a :> [])           =  Con nil
  toSpine (ListR  a :> x :  xs)   =  Con cons :$ (a :>  x) :$ (ListR  a :> xs)
  --  toSpine (Ty DynamicR (Dyn x))        =  Con dyn :$ (Ty (TypedR (typeOf x)) x)
{-# LINE 561 "SYB1.lhs" #-}
  -- toSpine (Ty BoolR False)             =  Con false
  -- toSpine (Ty BoolR True)              =  Con true
{-# LINE 2 "toSpine" #-}
  toSpine (PerfectR  a :> Zero x)  =  Con zero :$ (a :>  x)
  toSpine (PerfectR  a :> Succ x)  =  Con succ :$ (PerfectR  (PairR a a) :> x)
  toSpine (PPerfectR  a :> P.Zero x)  =  Con pzero :$ (a :>  x)
  toSpine (PPerfectR  a :> P.Succ x)  =  Con psucc :$ (PPerfectR (ForkR a) :> x)
{-# LINE 2 "toSpine" #-}
  toSpine (ExprR  IntR :> Num i)       =  Con num :$ (IntR :>  i)
  toSpine (ExprR  IntR :> Plus e1 e2)  =  Con plus :$ (ExprR  IntR :> e1) :$ (ExprR  IntR :> e2)
  toSpine (ExprR  BoolR :> Eq e1 e2)   =  Con eq :$ (ExprR  IntR :> e1) :$ (ExprR  IntR :> e2)
  toSpine (ExprR  a :> If e1 e2 e3)
    =  Con ifV :$ (ExprR  BoolR :> e1) :$ (ExprR  a :> e2) :$ (ExprR  a :> e3)

{- Missing -}
  toSpine (FloatR :>  i)               =  Con (float i)

{- Company datatypes -}
  toSpine (CompanyR :> C depts) = Con company :$ (ListR DeptR :> depts)
  toSpine (DeptR :> D name manager depts) =
      Con dept :$ (ListR CharR :> name) :$
                  (EmployeeR :> manager) :$
                  (ListR UnitR :> depts)
  toSpine (UnitR :> PU employee) = Con unit_pu :$ (EmployeeR :> employee)
  toSpine (UnitR :> DU dept) = Con unit_du :$ (DeptR :> dept)
  toSpine (EmployeeR :> E person salary) = Con employee :$
                                           (PersonR :> person) :$
                                           (SalaryR :> salary)
  toSpine (PersonR :> P name address) = Con person :$
                                        (ListR CharR :> name) :$
                                        (ListR CharR :> address)
  toSpine (SalaryR :> S amount) = Con salary :$
                                  (FloatR :> amount)

{- GRose -}
  toSpine (GRoseR f a :> GRose x xs) = Con grose :$
                                       (a :> x) :$
                                       (f (GRoseR f a) :> xs)
{- NGRose -}
  toSpine (NGRoseR f a :> NGRose x xs) = Con ngrose :$
                                         (a :> x) :$
                                         (f (NGRoseR (CompR f f) a) :> xs)
  toSpine (CompR f g a :> Comp x) = Con comp :$ (f (g a) :> x)

{- The other tree -}
  toSpine (TreeWR a w :> T.Leaf x) = Con tleaf :$ (a :> x)
  toSpine (TreeWR a w :> T.Fork x y) = Con tfork :$ (TreeWR a w :> x) :$ (TreeWR a w :> y)
  toSpine (TreeWR a w :> T.WithWeight x wv) = Con twithweight :$ (TreeWR a w :> x) :$ (w :> wv)

  toSpine (BinTreeR a :> B.Leaf x)  = Con tBinleaf :$ (a :> x)
  toSpine (BinTreeR a :> B.Bin l r) = Con tbin :$ (BinTreeR a :> l) :$ (BinTreeR a :> r)
{- Fork -}
  toSpine (ForkR a :> P.Fork l r)   =  Con pfork :$ (a :> l) :$ (a :> r)


{- Company constructors -}

  company =  Constr { constr = C, name = "C" }
  dept    =  Constr { constr = D, name = "D" }
  unit_pu =  Constr { constr = PU, name = "PU" }
  unit_du =  Constr { constr = DU, name = "DU" }
  employee=  Constr { constr = E, name = "E" }
  person  =  Constr { constr = P, name = "P" }
  salary  =  Constr { constr = S, name = "S" }

{- GRose contructor -}
  grose = Constr { constr = GRose, name = "GRose" }
{- NGRose contructor -}
  ngrose = Constr { constr = NGRose, name = "NGRose" }
  comp = Constr { constr = Comp, name = "Comp" }

  tleaf = Constr { constr = T.Leaf, name = "Leaf" }
  tfork = Constr { constr = T.Fork, name = "Fork" }
  twithweight = Constr { constr = T.WithWeight, name = "WithWeight" }

  tBinleaf = Constr { constr = B.Leaf, name = "Leaf" }
  tbin     = Constr { constr = B.Bin, name = "Bin" }

{- Perfect constructor -}
  pzero   =  Constr { constr = P.Zero,    name = "Zero" }
  psucc   =  Constr { constr = P.Succ,    name = "Succ" }
  pfork   =  Constr { constr = P.Fork,    name = "Fork" }

{-# LINE 570 "SYB1.lhs" #-}
  empty   ::  Constr (Tree a)
  empty   =   Constr { constr = Empty, name = "Empty" }

  node    ::  Constr (Tree a -> a -> Tree a -> Tree a)
  node    =   Constr { constr = Node, name = "Node" }
{-# LINE 578 "SYB1.lhs" #-}
  false   =  Constr { constr = False, name = "False" }
  true    =  Constr { constr = True,  name = "True" }
  pair    =  Constr { constr = mypair,  name = "(,)" }
  nil     =  Constr { constr = [],     name = "[]" }
  cons    =  Constr { constr = (:),  name = "(:)" }
  zero    =  Constr { constr = Zero,    name = "Zero" }
  succ    =  Constr { constr = Succ,    name = "Succ" }
  num     =  Constr { constr = Num,     name = "Num" }
  plus    =  Constr { constr = Plus,    name = "Plus" }
{-# LINE 610 "SYB1.lhs" #-}
  arity  ::  Typed a -> Int
  arity  =   aritySpine . toSpine

  aritySpine :: Spine a -> Int
  aritySpine (Con c)   =  0
  aritySpine (f :$ x)  =  aritySpine f + 1
{-# LINE 658 "SYB1.lhs" #-}
  data Perfect a  =  Zero a | Succ (Perfect (a, a))
{-# LINE 661 "SYB1.lhs" #-}
                deriving (Show)
{-# LINE 671 "SYB1.lhs" #-}
  data Expr ::  * ->  * where
    Num   :: Int -> Expr Int
    Plus  :: Expr Int -> Expr Int  -> Expr Int
    Eq    :: Expr Int -> Expr Int -> Expr Bool
    If    :: Expr Bool -> Expr a -> Expr a -> Expr a
{-# LINE 689 "SYB1.lhs" #-}
  data Dynamic ::  * where
    Dyn  ::  Typed a -> Dynamic
{-# LINE 809 "SYB1.lhs" #-}
  type Datatype a = [Signature a]

  infixl  0  :*
  data Signature ::  * ->  * where
    Sig   ::  Constr a -> Signature a
    (:*)  ::  Signature (a -> b) -> Type a -> Signature b
{-# LINE 836 "SYB1.lhs" #-}
  datatype :: Type a -> Datatype a
  datatype (CharR)      =  [ Sig (char c) | c <- [minBound .. maxBound] ]
  datatype (IntR)       =  [ Sig (int i)  | i <- [minBound .. maxBound] ]
  datatype (ListR a)    =  [ Sig nil, Sig cons :* a :* ListR a ]
  datatype (PairR a b)  =  [ Sig pair :* a :* b ]
{-# LINE 844 "SYB1.lhs" #-}
  datatype (TreeR a)    =  [ Sig empty, Sig node :* TreeR a :* a :* TreeR a ]
  datatype (BoolR)        =  [ Sig false, Sig true ]
{-# LINE 2 "datatype" #-}
  datatype (PerfectR a)  =  [ Sig zero :* a, Sig succ :* PerfectR (PairR a a) ]
{-# LINE 2 "datatype" #-}
  datatype (ExprR BoolR)
    =  [  Sig eq :* ExprR IntR :* ExprR IntR,
          Sig ifV :* ExprR BoolR :* ExprR BoolR :* ExprR BoolR ]
  datatype (ExprR IntR)
    =  [  Sig num :* IntR,
          Sig plus :* ExprR IntR :* ExprR IntR,
          Sig ifV :* ExprR BoolR :* ExprR IntR :* ExprR IntR ]
  datatype (ExprR a)
    =  [  Sig ifV :* ExprR BoolR :* ExprR a :* ExprR a ]
{-# LINE 850 "SYB1.lhs" #-}
  eq   =  Constr { constr = Eq,    name = "Eq" }
  ifV  =  Constr { constr = If,    name = "If" }
{-# LINE 855 "SYB1.lhs" #-}

  char    ::  Char -> Constr Char
  char c  =   Constr { constr  =  c,  name = myshowChar  c  }

  int     ::  Int -> Constr Int
  int i   =   Constr { constr  =  i,  name = showInt     i  }

{- missing -}
  float   ::  Float -> Constr Float
  float i =   Constr { constr  =  i,  name = Prelude.show     i  }

{-# LINE 873 "SYB1.lhs" #-}
  generate :: Type a -> Int -> [a]
{-# LINE 876 "SYB1.lhs" #-}
  generate IntR d     =  [ 0 | d > 0 ]
{-# LINE 878 "SYB1.lhs" #-}
  generate a  0              =  []
  generate a  (d + 1)        =  concat [ generateSpine s d | s <- datatype a ]

  generateSpine :: Signature a -> Int -> [a]
  generateSpine (Sig c)   d  =  [constr c]
  generateSpine (s :* a)  d  =  [ f x | f <- generateSpine s d, x <- generate a d ]
{-# LINE 896 "SYB1.lhs" #-}
  test_gen1= generate (ListR BoolR) 3
  {-  \eval{test_gen1}  -}
  test_gen2= generate (ListR (ListR BoolR)) 3
  {-  \eval{test_gen2}  -}
{-# LINE 911 "SYB1.lhs" #-}
  readsPrec :: Type a -> Int -> ReadS a
  readsPrec (CharR)        d  =  readsPrecChar d
  readsPrec (IntR)         d  =  readsPrecInt d
  readsPrec (ListR CharR)  d  =  readsPrecString d
  readsPrec (ListR a)      d  =  readsList (reads a)
  readsPrec (PairR a b)    d
    =  readParen False (\ s0 -> [ ((x, y), s5) |  ("(",  s1)  <-  lex      s0,
                                                  (x,    s2)  <-  reads a  s1,
                                                  (",",  s3)  <-  lex      s2,
                                                  (y,    s4)  <-  reads b  s3,
                                                  (")",  s5)  <-  lex      s4 ])
  readsPrec a              d
    =  alt [ readParen (arity' s > 0 && d > 10) (readsSpine s) | s <- datatype a ]
{-# LINE 940 "SYB1.lhs" #-}
  readsSpine :: Signature a -> ReadS a
  readsSpine (Sig c)   s0  =  [ (constr c, s1) | (t, s1) <- lex s0, name c == t ]
  readsSpine (s :* a)  s0  =  [ (f x, s2) |  (f,  s1)  <-  readsSpine s        s0,
                                             (x,  s2)  <-  readsPrec a 11  s1 ]
{-# LINE 947 "SYB1.lhs" #-}
  arity' :: Signature a -> Int
  arity' (Sig c)   =  0
  arity' (s :* a)  =  arity' s + 1
{-# LINE 954 "SYB1.lhs" #-}
  reads  ::  Type a -> ReadS a
  reads a   =  readsPrec a 0

  read   ::  Type a -> String -> a
  read a s  =  case [ x | (x, t) <- reads a s, ("", "") <- lex t ] of
               [x]  ->  x
               []   ->  error "read: no parse"
               _    ->  error "read: ambiguous parse"
{-# LINE 1020 "SYB1.lhs" #-}
  gen a d = putStrLn (show (ListR  a :> generate a d))
  test_gen4= gen (ExprR IntR) 4
    {-  \eval{test_gen4}  -}
  test_gen5= gen (ExprR BoolR) 4
    {-  \eval{test_gen5}  -}
  test_gen6= gen (ExprR CharR) 4
    {-  \eval{test_gen6}  -}
{-# LINE 1104 "SYB1.lhs" #-}
  data Type' :: ( * ->  *) ->  * where
    ListR'     ::  Type' []
    TreeR'     ::  Type' Tree
{-# LINE 2 "Type1" #-}
    PerfectR'   ::  Type' Perfect
    PerfectR'1  ::  Type' f -> Type' (Perfect' f)
{-# LINE 2 "Type1" #-}
    SpineR' :: Type' f -> Type' (Spine' f)
{-# LINE 2 "Type" #-}
    IdR        ::  Type' Id
    CharR'0    ::  Type' Char'
    IntR'0     ::  Type' Int'
    ListR'1    ::  Type' f -> Type' (List' f)
    PairR'2    ::  Type' f -> Type' g -> Type' (Pair' f g)
    TreeR'1    ::  Type' f -> Type' (Tree' f)
{- BinTree *->* -}
    BinTreeR'  ::  Type' B.BinTree
    BinTreeR'1 ::  Type' f -> Type' (BinTree' f)
{- WTree 
    WTreeR'  :: Type a -> Type' (WTree a)
    WTreeR'1 :: Type' f -> Type' (WTree' f)
-}

  data BinTree' a' x = Leaf' (a' x) | Bin' (BinTree' a' x) (BinTree' a' x)

{-
  data WTreeWA w a = WA (T.WTree a w)

  data WTree' a' x  =  LeafW' (a' x) 
                    |  Fork' (WTree' a' x) (WTree' a' x)
                    |  WithWeight' (WTree' a' x)
-}

{-# LINE 1117 "SYB1.lhs" #-}
  infixl 1 ::>
  data Typed' f a  =  (::>) { typeOf' :: Type' f, val' :: f a }
{-# LINE 1135 "SYB1.lhs" #-}
  size :: Typed' f a -> Int
  size (ListR' ::>  [])             =  0
  size (ListR' ::>  x :  xs)     =  1 + size (ListR' ::>  xs)
  size (TreeR' ::>  Empty)           =  0
  size (TreeR' ::>  Node l x r)    =  size (TreeR' ::>  l) + 1 + size (TreeR' ::>  r)
{-# LINE 2 "size" #-}
  size (IdR ::>  x)                       =  1
  size (CharR'0 ::>  c)                   =  0
  size (IntR'0 ::>  i)                    =  0
  size (ListR'1  a' ::> Nil')           =  0
  size (ListR'1  a' ::> Cons' x xs)   =  size (a' ::>  x) + size (ListR'1  a' ::> xs)
  size (TreeR'1  a' ::> Empty')         =  0
  size (TreeR'1  a' ::> Node' l x r)
    =  size (TreeR'1  a' ::> l) + size (a' ::>  x) + size (TreeR'1  a' ::> r)
{-# LINE 2 "size" #-}
  size (SpineR'  a' ::> x)  =  sizeSpine x
  size (a' ::>  x)            =  case spineView a' of
                                  View'C b' from to -> size (b' ::>  from x)
{-# LINE 1180 "SYB1.lhs" #-}
  newtype Id          x  =  InId     { outId     :: x    }

  newtype Char'       x  =  InChar'  { outChar'  :: Char  }

  newtype Int'        x  =  InInt'   { outInt'   :: Int   }

  data List' a'      x  =  Nil' | Cons' (a' x) (List' a' x)

  data Pair' a' b'  x  =  PairC' (a' x) (b' x)

  data Tree' a'      x  =  Empty' | Node' (Tree' a' x) (a' x) (Tree' a' x)
{-# LINE 1233 "SYB1.lhs" #-}
  infixr :+>
  newtype (f :+> g) x  =  Fun { app :: f x -> g x }
{-# LINE 1240 "SYB1.lhs" #-}
  nil'   ::  forall x . forall a' . (List' a') x
  nil'   =   Nil'

  cons'  ::  forall x . forall a' . (a' :+> List' a' :+> List' a') x
  cons'  =   Fun (\ x -> Fun (\ xs -> Cons' x xs))
{-# LINE 1251 "SYB1.lhs" #-}
  data Spine' :: ( * ->  *) ->  * ->  * where
    Con'   ::  (forall x . f x) -> Spine' f a
    (:$$)  ::  Spine' (f :+> g) a -> Typed' f a -> Spine' g a
{-# LINE 1265 "SYB1.lhs" #-}
  fromSpine' :: Spine' f a -> f a
  fromSpine' (Con' c)   =  c
  fromSpine' (f :$$ x)  =  fromSpine' f `app` val' x
{-# LINE 1282 "SYB1.lhs" #-}
  toSpine' :: Typed' f a -> Spine' f a
  toSpine' (ListR'1  a' ::> Nil')             =  Con' nil'
  toSpine' (ListR'1  a' ::> Cons' x xs)     =  Con' cons' :$$ (a' ::>  x) :$$ (ListR'1  a' ::> xs)
{-# LINE 2 "toSpine1" #-}
  toSpine' (PerfectR'1  a' ::> Zero' x)     =  Con' zero' :$$ (a' ::>  x)
  toSpine' (PerfectR'1  a' ::> Succ' x)     =  Con' succ' :$$ (PerfectR'1  (PairR'2 a' a') ::> x)
{-# LINE 1290 "SYB1.lhs" #-}
  toSpine' (CharR'0 ::>  c)                     =  Con' (InChar' (outChar' c))
  toSpine' (IntR'0 ::>   i)                     =  Con' (InInt' (outInt' i))
  toSpine' (PairR'2  a' b' ::> PairC' x y)  =  Con' pair' :$$ (a' ::>  x) :$$ (b' ::>  y)
  toSpine' (TreeR'1  a' ::> Empty')           =  Con' empty'
  toSpine' (TreeR'1  a' ::> Node' l x r)    =  Con' node' :$$ (TreeR'1  a' ::> l) :$$ (a' ::>  x) :$$ (TreeR'1  a' ::> r)
{- BinTree -}
  toSpine' (BinTreeR'1  a' ::> Leaf' x)   =  Con' leaf' :$$ (a' ::> x)
  toSpine' (BinTreeR'1  a' ::> Bin' l r)  =  Con' bin' :$$ (BinTreeR'1  a' ::> l) :$$ (BinTreeR'1  a' ::> r)
{- WTree
  toSpine' (WTreeR'1  a' ::> LeafW' x)   =  Con' leafw' :$$ (a' ::> x)
  toSpine' (WTreeR'1  a' ::> Fork' l r)  =  Con' fork' :$$ (WTreeR'1  a' ::> l) :$$ (WTreeR'1  a' ::> r)
  toSpine' (WTreeR'1  a' ::> WithWeight' t)  =  Con' withweight' :$$ (WTreeR'1  a' ::> t) -}

{-# LINE 1297 "SYB1.lhs" #-}
  pair'   ::  forall x . forall a' b' . (a' :+> b' :+> Pair' a' b') x
  pair'   =   Fun (\ x -> Fun (\ y -> PairC' x y))

  zero'   ::  forall x . forall a' . (a' :+> Perfect' a') x
  zero'   =   Fun (\ x -> Zero' x)

  succ'   ::  forall x . forall a' . (Perfect' (Pair' a' a') :+> Perfect' a') x
  succ'   =   Fun (\ x -> Succ' x)

  empty'  ::  forall x . forall a' . (Tree' a') x
  empty'  =   Empty'

  node'   ::  forall x . forall a' . (Tree' a' :+> a' :+> Tree' a' :+> Tree' a') x
  node'   =   Fun (\ l -> Fun (\ x -> Fun (\ r -> Node' l x r)))
{- BinTree -}
--  leaf'  ::  forall x . forall a' . (BinTree' a') x
  leaf'  =   Fun (\ x -> Leaf' x)
--  bin'   ::  forall x . forall a' . (Tree' a' :+> a' :+> Tree' a' :+> Tree' a') x
  bin'   =   Fun (\ l -> Fun (\ r -> Bin' l r))
{- WTree
  leafw'  =   Fun (\ x -> LeafW' x)
  fork'   =   Fun (\ l -> Fun (\ r -> Fork' l r))
  withweight' = Fun (\ t -> WithWeight' t) -}

{-# LINE 1319 "SYB1.lhs" #-}
  infixr  5  :=>
  infixl  5  :<=

  type f :=> g  =  forall a . f a -> g a
  type f :<= g  =  forall a . g a -> f a

  data View' :: ( * ->  *) ->  * where
    View'C :: Type' g -> (f :=> g) -> (f :<= g) -> View' f
{-# LINE 1338 "SYB1.lhs" #-}
  spineView :: Type' f -> View' f
{-# LINE 2 "spineView" #-}
  spineView ListR' = View'C (ListR'1 IdR) (fromList InId) (toList outId)
{-# LINE 1341 "SYB1.lhs" #-}
  spineView (TreeR')     =  View'C (TreeR'1 IdR) (fromTree InId) (toTree outId)
{-# LINE 2 "spineView" #-}
  spineView (PerfectR')
    =  View'C (PerfectR'1 IdR) (fromPerfect InId) (toPerfect outId)
{- BinTree -}
  spineView (BinTreeR') =  View'C (BinTreeR'1 IdR) (fromBinTree InId) (toBinTree outId)
{-# LINE 1344 "SYB1.lhs" #-}
  spineView a'     = View'C (SpineR' a') (\ x -> toSpine' (a' ::>  x)) fromSpine'

{-# LINE 1359 "SYB1.lhs" #-}
  sizeSpine :: Spine' f a -> Int
  sizeSpine (Con' c)   =  0
  sizeSpine (f :$$ x)  =  sizeSpine f + size x
{-# LINE 1374 "SYB1.lhs" #-}
  map :: Type' f -> (a -> b) -> (f a -> f b)
  map IdR           m  =  InId . m . outId
  map (SpineR' a')  m  =  mapSpine m
  map a'            m  =  case spineView a' of
                          View'C b' from to -> to . map b' m . from

  mapSpine :: (a -> b) -> (Spine' f a -> Spine' f b)
  mapSpine m (Con' c)            =  Con' c
  mapSpine m (f :$$ (a' ::>  x))  =  mapSpine m f :$$ (a' ::>  map a' m x)
{-# LINE 1409 "SYB1.lhs" #-}
  fromList  :: (a -> a' x)  ->  ([ a] -> List' a' x)
  fromList from []          =  Nil'
  fromList from (x :  xs)  =  Cons' (from x) (fromList from xs)

  toList    :: (a' x -> a)  ->  (List' a' x -> [ a])
  toList to Nil'           =  []
  toList to (Cons' x xs)   =  ( to x) : (toList to xs)
{-# LINE 1419 "SYB1.lhs" #-}
  fromTree  :: (a -> a' x)  ->  (Tree a -> Tree' a' x)
  fromTree f Empty         =  Empty'
  fromTree f (Node l x r)  =  Node' (fromTree f l) (f x) (fromTree f r)

  toTree    :: (a' x -> a)  ->  (Tree' a' x -> Tree a)
  toTree f Empty'         =  Empty
  toTree f (Node' l x r)  =  Node (toTree f l) (f x) (toTree f r)
{- BinTree-}
  fromBinTree  :: (a -> a' x)  ->  (B.BinTree a -> BinTree' a' x)
  fromBinTree f (B.Leaf x)   =  Leaf' (f x)
  fromBinTree f (B.Bin l r)  =  Bin' (fromBinTree f l) (fromBinTree f r)
  toBinTree    :: (a' x -> a)  ->  (BinTree' a' x -> B.BinTree a)
  toBinTree f (Leaf' x)   =  B.Leaf (f x)
  toBinTree f (Bin' l r)  =  B.Bin (toBinTree f l) (toBinTree f r)

{-# LINE 1444 "SYB1.lhs" #-}
  ts = [ tree [0 .. i :: Int] | i <- [0 .. 9] ]
  test_size1= size (ListR' ::>  ts)
  {-  \eval{test_size1}  -}
  test_size4= size (ListR'1  (TreeR'1 IdR) ::> fromList (fromTree InId) ts)
  {-  \eval{test_size4}  -}
  test_size3= size (ListR'1  IdR ::> fromList InId ts)
  {-  \eval{test_size3}  -}
  test_size2= size (IdR ::>  InId ts)
  {-  \eval{test_size2}  -}
  test_size5= size (ListR'1  (TreeR'1 IntR'0) ::> fromList (fromTree InInt') ts)
  {-  \eval{test_size5}  -}
{-# LINE 1478 "SYB1.lhs" #-}
  data Perfect' a' x = Zero' (a' x) | Succ' (Perfect' (Pair' a' a') x)

{-# LINE 1497 "SYB1.lhs" #-}

  fromPerfect  :: (a -> a' x)  ->  (Perfect a -> Perfect' a' x)
  fromPerfect from (Zero x)  =  Zero' (from x)
  fromPerfect from (Succ x)  =  Succ' (fromPerfect (fromPair from from) x)

  toPerfect    :: (a' x -> a)  ->  (Perfect' a' x -> Perfect a)
  toPerfect to (Zero' x)   =  Zero (to x)
  toPerfect to (Succ' x)   =  Succ (toPerfect (toPair to to) x)
{-# LINE 1509 "SYB1.lhs" #-}
  fromPair  :: (a -> a' x)  ->  (b -> b' x)  ->  (( a, b) -> Pair' a' b' x)
  fromPair f g (x, y)      =  PairC' (f x) (g y)

  toPair    :: (a' x -> a)  ->  (b' x -> b)  ->  (Pair' a' b' x -> ( a, b))
  toPair f g (PairC' x y)  =  (f x, g y)
{-# LINE 1523 "SYB1.lhs" #-}
  test_perfect1= size (PerfectR' ::>  Succ (Zero (1, 2)))
  {-  \eval{test_perfect1}  -}
  test_perfect2= map (PerfectR') (+1) (Succ (Zero (1, 2)))
  {-  \eval{test_perfect2}  -}
{-# LINE 1549 "SYB1.lhs" #-}
  data Expr' :: ( * ->  *) ->  * ->  * where
    Num' :: Int' x -> Expr' Int' x
{-# LINE 1865 "SYB1.lhs" #-}
  tree :: [a] -> Tree a
  tree x
    | null x            =  Empty
    | otherwise         =  Node (tree x1) a (tree x2)
    where (x1, a : x2)  =  splitAt (length x `div` 2) x
{-# LINE 1875 "SYB1.lhs" #-}
  (<>) :: ShowS -> ShowS -> ShowS
  s1 <> s2 = s1 . showChar ' ' . s2
{-# LINE 1881 "SYB1.lhs" #-}
  showsList :: [ShowS] -> ShowS
  showsList []          =  showString "[]"
  showsList (x :  xs)  =  showChar '[' . x
                         .  foldr (.) id [ showChar ',' . s | s <- xs ]
                         .  showChar ']'
{-# LINE 1890 "SYB1.lhs" #-}
  alt :: [ReadS a] -> ReadS a
  alt rs = \ s -> concatMap (\ r -> r s) rs
{-# LINE 1896 "SYB1.lhs" #-}
  readsList :: ReadS a -> ReadS [a]
  readsList r = readParen False (\ s -> [ x | ("[", s1) <- lex s, x <- readl s1 ])
    where  readl   s  =   [ ([],      s1)  |  ("]",  s1)  <-  lex     s ]
                      ++  [ (x : xs,  s2)  |  (x,    s1)  <-  r       s,
                                              (xs,   s2)  <-  readl'  s1 ]
           readl'  s  =   [ ([],      s1)  |  ("]",  s1)  <-  lex     s ]
                      ++  [ (x : xs,  s3)  |  (",",  s1)  <-  lex     s,
                                              (x,    s2)  <-  r       s1,
                                              (xs,   s3)  <-  readl'  s2  ]
