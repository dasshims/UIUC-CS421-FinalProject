{-# LINE 107 "NOW.lhs" #-}
  {-#  OPTIONS_GHC -fglasgow-exts  #-}
{-# LINE 110 "NOW.lhs" #-}
  module NOW where
  import qualified Prelude
  import Prelude hiding (show, shows, showsPrec, showChar, showString,
                         read, reads, readsPrec,
                         map, sum, succ, compare)
  import Data.List (genericIndex, findIndex, foldl', sortBy)
  import Data.Maybe (fromJust)
  import Data.Map (Map(..))
  import qualified Data.Map as M
  import Control.Monad (liftM, liftM2)
  import TreeDatatype
  import qualified CompanyDatatypes as CD
  import CompanyDatatypes hiding (Unit, S)
  import GRoseDatatype
  import qualified PerfectDatatype as PD
  import qualified BinTreeDatatype as BT
  import qualified GADT

  data Type ::  * ->  * where
    CharR     ::  Type Char
    IntR      ::  Type Int
    PairR     ::  Type a -> Type b -> Type ( a, b)
    ListR     ::  Type a -> Type [ a]
    TreeNOWR     ::  Type a -> Type (TreeNOW a)
    BoolR    ::  Type Bool
    DynamicR  ::  Type Dynamic
    TypeR    ::  Type a -> Type (Type a)
    TypedR   ::  Type a -> Type (Typed a)
    FixR     ::  forall f . (forall a . Type a -> Type (f a)) -> Type (Fix f)
    PerfectR  ::  Type a -> Type (Perfect a)
    SpineR    ::  Type a -> Type (Spine a)
    (:->)     ::  Type a -> Type b -> Type (a -> b)
    Default   ::  Type a
    Use       ::  Type a -> Instance a -> Type a
    CountR     ::  (a -> Int) -> Type a
    ZeroR  ::  Type ZeroT
    UnitR  ::  Type Unit
    (:+$)  ::  Type a -> Type b -> Type (a :+: b)
    (:*$)  ::  Type a -> Type b -> Type (a :*: b)
    ExprR     :: Type a -> Type (Expr a)
    NatR      :: Type Nat
  -- AG: added type signatures for eval test here due to the non-openness
    WTreeR    ::  Type a -> Type w -> Type (WTree a w)
    CompanyR  ::  Type Company
    DeptR     ::  Type Dept
    UnitCDR   ::  Type CD.Unit
    EmployeeR ::  Type Employee
    PersonR   ::  Type Person
    SalaryR   ::  Type Salary
    FloatR    ::  Type Float
    GRoseR    ::  (forall a.Type a -> Type (f a)) ->
                  Type a ->
                  Type (GRose f a)
    BinTreeR  ::  Type a -> Type (BT.BinTree a)
    ExprR'     :: Type a -> Type (GADT.Expr a)

  toSpine  :: Typed a -> Spine a
  toSpine (TreeNOWR  a :> Empty)          =  Con empty
  toSpine (TreeNOWR  a :> Node l x r)   =  Con node :$ (TreeNOWR  a :> l) :$ (a :>  x) :$ (TreeNOWR  a :> r)
  toSpine (IntR :>  i)                  =  Con (int i)
  toSpine (CharR :>  c)                 =  Con (char c)
  toSpine (PairR  a b :> (x, y))    =  Con pair :$ (a :>  x) :$ (b :>  y)
  toSpine (ListR  a :> [])           =  Con nil
  toSpine (ListR  a :> x :  xs)   =  Con cons :$ (a :>  x) :$ (ListR  a :> xs)
  toSpine (DynamicR :>  Dyn x)        =  Con dyn :$ (TypedR  (typeOf x) :> x)
  toSpine (TypeR  a :> x)             =  fromType x
  toSpine (TypedR  a :> (t :>  x))   =  Con ty :$ (TypeR  t :> t) :$ (t :>  x)
  toSpine (BoolR :>  False)             =  Con false
  toSpine (BoolR :>  True)              =  Con true
  toSpine (FixR  f :> In x)         =  Con myin :$ (f  (FixR f) :> x)
  toSpine (ExprR  IntR :> Num i)       =  Con num :$ (IntR :>  i)
  toSpine (ExprR  IntR :> Plus e1 e2)  =  Con plus :$ (ExprR  IntR :> e1) :$ (ExprR  IntR :> e2)
  toSpine (ExprR  BoolR :> Eq e1 e2)   =  Con eq :$ (ExprR  IntR :> e1) :$ (ExprR  IntR :> e2)
  toSpine (ExprR  a :> If e1 e2 e3)    =  Con ifV :$ (ExprR  BoolR :> e1) :$ (ExprR  a :> e2) :$ (ExprR  a :> e3)
  -- AG: added type signatures for eval test here due to the non-openness
  toSpine (WTreeR a w :> Leaf x) = Con leaf :$ (a :> x)
  toSpine (WTreeR a w :> Fork l r) = Con fork :$ (WTreeR a w :> l) :$ (WTreeR a w :> r)
  toSpine (WTreeR a w :> WithWeight t f) = Con withweight :$ (WTreeR a w :> t) :$ (w :> f)
  toSpine (CompanyR :> C d) = Con company :$ (ListR DeptR :> d)
  toSpine (DeptR :> D n m us) = Con dept :$ (ListR CharR :> n) :$ (EmployeeR :> m) :$ (ListR UnitCDR :> us)
  toSpine (UnitCDR :> PU e) = Con pu :$ (EmployeeR :> e)
  toSpine (UnitCDR :> DU d) = Con du :$ (DeptR :> d)
  toSpine (EmployeeR :> E p s) = Con employee :$ (PersonR :> p) :$ (SalaryR :> s)
  toSpine (PersonR :> P n a) = Con person :$ (ListR CharR :> n) :$ (ListR CharR :> a)
  toSpine (SalaryR :> CD.S f) = Con salary :$ (FloatR :> f)
  toSpine (FloatR :> f) = Con (float f)
  toSpine (GRoseR f a :> GRose x xs) = Con grose :$ (a :> x) :$ (f (GRoseR f a) :> xs)
  toSpine (BinTreeR a :> BT.Leaf x) = Con bleaf :$ (a :> x)
  toSpine (BinTreeR a :> BT.Bin l r) = Con bin :$ (BinTreeR a :> l) :$ (BinTreeR a :> r)
  toSpine (ExprR'  IntR :> GADT.Num i)       =  Con num' :$ (IntR :>  i)
  toSpine (ExprR'  IntR :> GADT.Plus e1 e2)  =  Con plus' :$ (ExprR'  IntR :> e1) :$ (ExprR'  IntR :> e2)
  toSpine (ExprR'  BoolR :> GADT.Eq e1 e2)   =  Con eq' :$ (ExprR'  IntR :> e1) :$ (ExprR'  IntR :> e2)
  toSpine (ExprR'  a :> GADT.If e1 e2 e3)    =  Con ifV' :$ (ExprR'  BoolR :> e1) :$ (ExprR'  a :> e2) :$ (ExprR'  a :> e3)

  -- Constructor reps
  leaf :: Constr (a -> WTree a w)
  leaf = Descr { constr   = Leaf,
                 name     = "Leaf",
                 arity    = 1,
                 fixity   = Prefix 10,
                 order    = (0, 3) }
  fork :: Constr (WTree a w -> WTree a w -> WTree a w)
  fork = Descr { constr   = Fork,
                 name     = "Fork",
                 arity    = 2,
                 fixity   = Prefix 10,
                 order    = (1, 3) }
  withweight :: Constr (WTree a w -> w -> WTree a w)
  withweight = Descr { constr = WithWeight,
                       name   = "WithWeight",
                       arity  = 2,
                       fixity = Prefix 10,
                       order  = (2, 3) }
  company = Descr { constr = C,
                    name   = "C",
                    arity  = 1,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  dept = Descr { constr = D,
                    name   = "D",
                    arity  = 3,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  pu = Descr { constr = PU,
                    name   = "PU",
                    arity  = 1,
                    fixity = Prefix 10,
                    order  = (0, 2) }
  du = Descr { constr = DU,
                    name   = "DU",
                    arity  = 1,
                    fixity = Prefix 10,
                    order  = (1, 2) }
  employee = Descr { constr = E,
                    name   = "E",
                    arity  = 2,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  person = Descr { constr = P,
                    name   = "P",
                    arity  = 2,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  salary = Descr { constr = CD.S,
                    name   = "S",
                    arity  = 1,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  float :: Float -> Constr Float
  float f = Descr { constr = f,
                    name   = Prelude.show f,
                    arity  = 0,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  grose = Descr { constr = GRose,
                    name   = "GRose",
                    arity  = 2,
                    fixity = Prefix 10,
                    order  = (0, 1) }
  bleaf :: Constr (a -> BT.BinTree a)
  bleaf = Descr { constr  = BT.Leaf,
                 name     = "Leaf",
                 arity    = 1,
                 fixity   = Prefix 10,
                 order    = (0, 2) }
  bin :: Constr (BT.BinTree a -> BT.BinTree a -> BT.BinTree a)
  bin = Descr { constr    = BT.Bin,
                 name     = "Bin",
                 arity    = 2,
                 fixity   = Prefix 10,
                 order    = (1, 2) }
  num' :: Constr (Int -> GADT.Expr Int)
  num'     =  Descr { constr = GADT.Num,     
                     name = "Num" , 
                     arity = 1, 
                     fixity = Prefix 10, 
                     order = error "zero: order unknown" }
  plus'    =  Descr { constr = GADT.Plus,    
                     name = "Plus", 
                     arity = 2, 
                     fixity = Prefix 10, 
                     order = error "zero: order unknown"  }
  eq'   =  Descr { constr = GADT.Eq,    
                     name = "Eq", 
                     arity = 2, 
                     fixity = Prefix 10, 
                     order = error "zero: order unknown"  }
  ifV'  =  Descr { constr = GADT.If,    
                     name = "If", 
                     arity = 3, 
                     fixity = Prefix 10, 
                     order = error "zero: order unknown"  }

  data PType :: ( * ->  *) ->  * ->  * where
    PCharR     ::  PType polyT Char
    PIntR      ::  PType polyT Int
    PPairR     ::  PType polyT a -> PType polyT b -> PType polyT ( a, b)
    PListR     ::  PType polyT a -> PType polyT [ a]
    PTreeNOWR     ::  PType polyT a -> PType polyT (TreeNOW a)
    PVar       ::  polyT a -> PType polyT a
    PVar_0     ::  polyT a -> PType polyT a  --  |= PPoly_0 polyT aT -> PType_0 polyT aT|
    PVar_1     ::  PPoly_1 polyT a -> PType polyT x -> PType polyT (a x)
                                               --  |= PPoly_1 polyT aT -> PType_1 polyT aT|
    -- AG: added type reps
    PWTreeR     ::  PType polyT a -> PType polyT w -> PType polyT (WTree a w)
    PPerfectR   ::  PType polyT a -> PType polyT (PD.Perfect a)
    PForkR      ::  PType polyT a -> PType polyT (PD.Fork a)

  -- AG: start original NOW lib with Type and toSpine extracted

  type Pair  =  (,)
  type List  =  []

  mypair  =  (,)
{-# LINE 127 "NOW.lhs" #-}
  showsPrecChar :: Int -> Char -> ShowS
  showsPrecChar = Prelude.showsPrec
  showsPrecInt :: Int -> Int -> ShowS
  showsPrecInt = Prelude.showsPrec
  showsPrecString :: Int -> String -> ShowS
  showsPrecString = Prelude.showsPrec
{-# LINE 135 "NOW.lhs" #-}
  readsPrecChar    ::  Int -> ReadS Char
  readsPrecChar    =   Prelude.readsPrec
  readsPrecInt     ::  Int -> ReadS Int
  readsPrecInt     =   Prelude.readsPrec
  readsPrecString  ::  Int -> ReadS String
  readsPrecString  =   Prelude.readsPrec
{-# LINE 142 "NOW.lhs" #-}
  showInt :: Int -> String
  showInt = Prelude.show
  showChar :: Char -> String
  showChar = Prelude.show
  showString :: String -> String
  showString = Prelude.show
{-# LINE 149 "NOW.lhs" #-}
  compareChar :: Char -> Char -> Ordering
  compareChar = Prelude.compare
  compareInt :: Int -> Int -> Ordering
  compareInt = Prelude.compare
{-# LINE 154 "NOW.lhs" #-}
  mapList = Prelude.map
  mapTree :: (a -> b) -> (TreeNOW a -> TreeNOW b)
  mapTree = fmap
{-# LINE 158 "NOW.lhs" #-}
  sumList :: Num a => [a] -> a
  sumList = Prelude.sum
{-# LINE 481 "NOW.lhs" #-}
  data Expr ::  * ->  * where
    Num   :: Int -> Expr Int
    Plus  :: Expr Int -> Expr Int -> Expr Int
    Eq    :: Expr Int -> Expr Int -> Expr Bool
    If    :: forall a . Expr Bool -> Expr a -> Expr a -> Expr a
{-# LINE 2 "Expr" #-}
    Str   :: String -> Expr String
    Show  :: Expr Int -> Expr String
    Cat   :: Expr String -> Expr String -> Expr String
{-# LINE 2 "Expr" #-}
    Var   :: a -> Expr a                            --  inverse of |eval|
    Abs   :: (Expr a -> Expr b) -> Expr (a -> b)  --  abstraction
    App   :: Expr (a -> b) -> Expr a -> Expr b    --  application
{-# LINE 501 "NOW.lhs" #-}
  eval :: forall a. Expr a -> a
  eval (Num i)        =  i
  eval (Plus e1 e2)   =  eval e1 + eval e2
  eval (Eq e1 e2)     =  eval e1 == eval e2
  eval (If e1 e2 e3)  =  if eval e1 then eval e2 else eval e3
{-# LINE 2 "eval" #-}
  eval (Str s)        =  s
  eval (Show e)       =  showInt (eval e)
  eval (Cat e1 e2)    =  eval e1 ++ eval e2
{-# LINE 2 "eval" #-}
  eval (Var a)        =  a
  eval (Abs f)        =  \ a -> eval (f (Var a))
  eval (App e1 e2)    =  (eval e1) (eval e2)
{-# LINE 579 "NOW.lhs" #-}
  string  :: Expr a -> String
  string (Num i)        =  "(Num"   +++ showInt i ++ ")"
  string (Plus e1 e2)   =  "(Plus"  +++ string e1 +++ string e2 ++ ")"
  string (Eq e1 e2)     =  "(Eq"    +++ string e1 +++ string e2 ++ ")"
  string (If e1 e2 e3)  =  "(If"    +++ string e1 +++ string e2 +++ string e3 ++ ")"
  string (Str s)        =  "(Str"   +++ showString s ++ ")"
  string (Show e)       =  "(Show"  +++ string e ++ ")"
  string (Cat e1 e2)    =  "(Cat"   +++ string e1 +++ string e2 ++ ")"
{-# LINE 591 "NOW.lhs" #-}
  s1 +++ s2  =  s1 ++ " " ++ s2
{-# LINE 679 "NOW.lhs" #-}
  data TreeNOW a  =  Empty | Node (TreeNOW a) a (TreeNOW a)
                   deriving (Show)
{-# LINE 684 "NOW.lhs" #-}
  instance Functor TreeNOW where
    fmap f Empty         =  Empty
    fmap f (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)
{-# LINE 737 "NOW.lhs" #-}
  prettyInt :: Int -> Text
  prettyInt n = text (showInt n)

  prettyTreeInt :: TreeNOW Int -> Text
  prettyTreeInt Empty         =  text "Empty"
  prettyTreeInt (Node l x r)  =  align "(Node " (  prettyTreeInt  l  <> nl <>
                                                   prettyInt      x  <> nl <>
                                                   prettyTreeInt  r  <> text ")")

  align :: String -> Text -> Text
  align s d = indent (length s) (text s <> d)
{-# LINE 753 "NOW.lhs" #-}
  prettyChar :: Char -> Text
  prettyChar c = text (showChar c)

  prettyTreeChar :: TreeNOW Char -> Text
  prettyTreeChar Empty         =  text "Empty"
  prettyTreeChar (Node l x r)  =  align "(Node " (  prettyTreeChar  l  <> nl <>
                                                    prettyChar      x  <> nl <>
                                                    prettyTreeChar  r  <> text ")")
{-# LINE 813 "NOW.lhs" #-}

{-# LINE 864 "NOW.lhs" #-}
  infixl 1 :>
  data Typed a  =  (:>) { typeOf :: Type a, val :: a }
{-# LINE 887 "NOW.lhs" #-}
  pretty  ::  Typed a -> Text
  pretty (CharR :>  c)                 =   prettyChar c
  pretty (IntR :>  n)                  =   prettyInt n
  pretty (PairR  a b :> (x, y))    =   align "( " (pretty (a :>  x)) <> nl <>
                                          align ", " (pretty (b :>  y)) <> text ")"
{-# LINE 2 "pretty" #-}
  pretty     ((ListR CharR) :>  s)               =   text (showString s)
{-# LINE 896 "NOW.lhs" #-}
  pretty (ListR  a :> xs)            =   bracketed [ pretty (a :>  x) | x <- xs ]
  pretty (TreeNOWR  a :> Empty)         =   text "Empty"
  pretty (TreeNOWR  a :> Node l x r)
    =   align "(Node " (  pretty (TreeNOWR  a :> l)  <> nl <>
                          pretty (a :>  x)         <> nl <>
                          pretty (TreeNOWR  a :> r)  <> text ")")
{-# LINE 2 "pretty" #-}
  pretty     (TypedR  a :> (t :>  x))   =   align "( " (pretty (t :>  x)) <> nl <>   --  |t = a|
                                              align ": " (pretty (TypeR  t :> t)) <> text ")"
{-# LINE 2 "pretty" #-}
  pretty (PerfectR  a :> Zero x)   =   align "(Zero " (pretty (a :>  x) <> text ")")
  pretty (PerfectR  a :> Succ x)
      =   align "(Succ " (pretty (PerfectR  (PairR a a) :> x) <> text ")")
{-# LINE 2 "pretty" #-}
  pretty     x                           =   prettySpine (toSpine x)
{-# LINE 4 "pretty" #-}
  prettySpine :: Spine a -> Text
  prettySpine (Con c)   =  text (name c)
  prettySpine (f :$ x)  =  prettySpine1 f (pretty x)

  prettySpine1 :: Spine a -> Text -> Text
  prettySpine1 (Con c)   d  =  align ("(" ++ name c ++ " ") (d <> text ")")
  prettySpine1 (f :$ x)  d  =  prettySpine1 f (pretty x <> nl <> d)
{-# LINE 931 "NOW.lhs" #-}
  show    ::  Typed a -> String
  show x  =   render (pretty x)
{-# LINE 984 "NOW.lhs" #-}
  strings  ::  Typed a ->  [ String]
  strings (IntR :>  i)                =   []
  strings (CharR :>  c)               =   []
  strings ((ListR CharR) :>  s)             =   [s]
  strings (PairR  a b :> (x, y))  =   strings (a :>  x) ++ strings (b :>  y)
  strings (ListR  a :> xs)          =   concat [ strings (a :>  x) | x <- xs ]
  strings (TreeNOWR  a :> t)           =   strings (ListR  a :> inorder t)
{-# LINE 2 "strings" #-}
  strings     (FixR  f :> In x)      =   strings (f  (FixR f) :> x)
{-# LINE 2 "strings" #-}
  strings (SpineR  a :> x)  =  stringsSpine x
{-# LINE 2 "strings" #-}
  strings x  =  stringsSpine (toSpine x)
{-# LINE 2 "strings" #-}
{- -- the rest will never be matched!
  strings     (SpineR  a :> Con c)     =   []
  strings     (SpineR  a :> (f :$ x))  =   strings (SpineR  (typeOf x :-> a) :> f) ++ strings x
  strings     x                           =   strings (SpineR  (typeOf x) :> toSpine x)
{-# LINE 2 "strings" #-}
  strings (t :>  x)  =  case view t of
                       ViewC u fromData toData -> strings (u :>  fromData x)
-}
{-# LINE 6 "strings" #-}
  view = spine
{-# LINE 1034 "NOW.lhs" #-}
  data Perfect a  =  Zero a | Succ (Perfect ( a, a))
{-# LINE 1038 "NOW.lhs" #-}
                      deriving (Show)
{-# LINE 1124 "NOW.lhs" #-}
  infixl  0  :$

  data Spine ::  * ->  * where
    Con   ::  Constr a -> Spine a
    (:$)  ::  Spine (a -> b) -> Typed a -> Spine b
{-# LINE 1161 "NOW.lhs" #-}
  data Constr a = Descr  {  constr  ::  a,
                             name    ::  String,
                             arity   ::  Int,
                             fixity  ::  Fixity,
                             order   ::  (Integer, Integer) }

  data Fixity = Prefix Int | Infix Int | Infixl Int | Infixr Int | Postfix Int
{-# LINE 1174 "NOW.lhs" #-}
  fromSpine :: Spine a -> a
  fromSpine (Con c)   =  constr c
  fromSpine (f :$ x)  =  (fromSpine f) (val x)
{-# LINE 1199 "NOW.lhs" #-}

{-# LINE 1218 "NOW.lhs" #-}
  empty   ::  Constr (TreeNOW a)
  empty   =   Descr {  constr  =  Empty,
                       name    =  "Empty",
                       arity   =  0,
                       fixity  =  Prefix 10,
                       order   =  (0, 2)  }
  node    ::  Constr (TreeNOW a -> a -> TreeNOW a -> TreeNOW a)
  node    =   Descr {  constr  =  Node,
                       name    =  "Node",
                       arity   =  3,
                       fixity  =  Prefix 10,
                       order   =  (1, 2)  }
{-# LINE 1233 "NOW.lhs" #-}
  false   =  Descr { constr = False,   name = "False",  arity = 0,  fixity = Prefix 10,  order = (0, 2) }
  true    =  Descr { constr = True,    name = "True",   arity = 0,  fixity = Prefix 10,  order = (1, 2) }
  int :: Int -> Constr Int
  int i   =  Descr { constr  =  i,
                      name    =  Prelude.show i,
                      arity   =  0,
                      fixity  =  Prefix 10,
                      order   =  (toInteger i - l, u - l + 1) }
    where  l  =  toInteger (minBound :: Int)
           u  =  toInteger (maxBound :: Int)
  char :: Char -> Constr Char
  char c  =  Descr { constr  =  c,
                      name    =  Prelude.show c,
                      arity   =  0,
                      fixity  =  Prefix 10,
                      order   =  (toInteger (fromEnum c - l), toInteger (u - l + 1)) }
    where  l  =  fromEnum (minBound :: Char)
           u  =  fromEnum (maxBound :: Char)
  pair    =  Descr { constr = mypair,  name = "(,)",    arity = 2,  fixity = Prefix 10,  order = (0, 1) }
  nil     =  Descr { constr = [],     name = "[]",    arity = 0,  fixity = Prefix 10,  order = (0, 2) }
  cons    =  Descr { constr = (:),  name = "(:)",   arity = 2,  fixity = Prefix 10,  order = (1, 2) }
  myin    =  Descr { constr = In,      name = "In",     arity = 1,  fixity = Prefix 10,  order = (0, 1) }
{-# LINE 1257 "NOW.lhs" #-}
  zero    =  Descr { constr = Zero,    name = "Zero", arity = 0, fixity = Prefix 10, order = error "zero: order unknown" }
  succ    =  Descr { constr = Succ,    name = "Succ", arity = 1, fixity = Prefix 10, order = error "zero: order unknown"  }
  num     =  Descr { constr = Num,     name = "Num" , arity = 1, fixity = Prefix 10, order = error "zero: order unknown" }
  plus    =  Descr { constr = Plus,    name = "Plus", arity = 2, fixity = Prefix 10, order = error "zero: order unknown"  }
{-# LINE 1277 "NOW.lhs" #-}

  stringsSpine :: Spine a -> [String]
  stringsSpine (Con c)   =  []
  stringsSpine (f :$ x)  =  stringsSpine f ++ strings x
{-# LINE 1290 "NOW.lhs" #-}
  varstrings :: Typed a -> [String]
  varstrings ((ListR CharR) :>  s)  =  [s]
  varstrings x               =  varstringsSpine (toSpine x)
{-# LINE 1296 "NOW.lhs" #-}
  varstringsSpine :: Spine a -> [String]
  varstringsSpine (Con c)   =  []
  varstringsSpine (f :$ x)  =  varstringsSpine f ++ varstrings x
{-# LINE 1344 "NOW.lhs" #-}
  data Dynamic ::  * where
    Dyn  ::  Typed a -> Dynamic
{-# LINE 1352 "NOW.lhs" #-}
  misc  ::  [Dynamic]
  misc  =   [Dyn (IntR :>  4711), Dyn ((ListR CharR) :>  "hello world")]
{-# LINE 1488 "NOW.lhs" #-}
  data (:=:) ::  * ->  * ->  * where Refl :: (:=:) a a
{-# LINE 1500 "NOW.lhs" #-}
  newtype a :=:: b  =  Proof { apply_ :: forall f . f a -> f b }
{-# LINE 1511 "NOW.lhs" #-}
  apply :: (a :=: b) -> (a -> b)
  apply p x = case p of { Refl -> x }
{-# LINE 1523 "NOW.lhs" #-}
  ctx_1    ::  (a :=: b) -> (g a :=: g b)
  ctx_1 p  =   case p of { Refl -> Refl }

  ctx_2        ::  (a1 :=: b1) -> (a2 :=: b2) -> (g a1 a2 :=: g b1 b2)
  ctx_2 p1 p2  =   case p1 of { Refl -> case p2 of { Refl -> Refl } }
{-# LINE 1542 "NOW.lhs" #-}
  equal :: Type a -> Type b -> Maybe (a :=: b)
  equal  IntR           IntR           =  return Refl
  equal  CharR          CharR          =  return Refl
  equal  (PairR a1 a2)  (PairR b1 b2)  =  liftM2 ctx_2 (equal a1 b1) (equal a2 b2)
  equal  (ListR a)      (ListR b)      =  liftM ctx_1 (equal a b)
{-# LINE 1549 "NOW.lhs" #-}
  equal (TreeNOWR a)       (TreeNOWR b)      =  do { p <- equal a b; return (ctx_1 p) }
  equal BoolR           BoolR          =  return Refl
  equal DynamicR        DynamicR       =  return Refl
  equal (TypeR a)       (TypeR b)      =  do { p <- equal a b; return (ctx_1 p) }
  equal (TypedR a)      (TypedR b)     =  do { p <- equal a b; return (ctx_1 p) }
  --  and more
{-# LINE 1556 "NOW.lhs" #-}
  equal _               _              =  fail "types are not unifiable"
{-# LINE 1571 "NOW.lhs" #-}
  cast                   :: Dynamic -> Type a -> Maybe a
  cast (Dyn (a :>  x)) t  =  fmap (\p -> apply p x) (equal a t)
{-# LINE 1831 "NOW.lhs" #-}
  newtype Fix f = In { out :: f (Fix f) }
{-# LINE 1911 "NOW.lhs" #-}
  data Type' :: ( * ->  *) ->  * where
    ListR'    ::  Type' []
    TreeNOWR'    ::  Type' TreeNOW
{-# LINE 2 "Type1" #-}
    Const'    ::  Type a -> Type' (Const a)
    Unit'R    ::  Type' Unit'
    Sum'      ::  Type' f -> Type' g -> Type' (f :+.: g)
    Pair'     ::  Type' f -> Type' g -> Type' (f :*.: g)
{-# LINE 2 "Type" #-}
    ConstR     :: Type' (Const a)
    ComR       ::  Type' f -> Type' g -> Type' (Com f g)
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
    TreeNOWR'1    ::  Type' f -> Type' (TreeNOW' f)
{-# LINE 1926 "NOW.lhs" #-}
    VarChar'0  ::  Type' VarChar'
    VarInt'0   ::  Type' VarInt'
    VarList'1  ::  Type' f -> Type' (VarList' f)
    VarTree'1  ::  Type' f -> Type' (VarTree' f)
    Rose'0     ::  Type' f -> Type' (Rose f)
    Perfect'   ::  Type' Perfect
    Com'       ::  Type' f -> Type' g -> Type' (Com f g)
    Pair'2     ::  Type' f -> Type' g -> Type' (f :*.: g)
    Var1List'1 ::  Type' f -> Type' (Var1List' f)
    Var1Tree'1 ::  Type' f -> Type' (Var1Tree' f)
{-# LINE 1942 "NOW.lhs" #-}
  infixl 1 ::>
  data Typed' f a  =  (::>) { typeOf' :: Type' f, val' :: f a }
{-# LINE 1958 "NOW.lhs" #-}
  size :: Typed' f a -> Int
  size (ListR' ::>  [])             =  0
  size (ListR' ::>  x :  xs)     =  1 + size (ListR' ::>  xs)
  size (TreeNOWR' ::>  Empty)           =  0
  size (TreeNOWR' ::>  Node l x r)    =  size (TreeNOWR' ::>  l) + 1 + size (TreeNOWR' ::>  r)
{-# LINE 2 "size" #-}
  size (IdR ::>  x)                       =  1
  size (CharR'0 ::>  c)                   =  0
  size (IntR'0 ::>  i)                    =  0
  size (ListR'1  a' ::> Nil')           =  0
  size (ListR'1  a' ::> Cons' x xs)   =  size (a' ::>  x) + size (ListR'1  a' ::> xs)
  size (TreeNOWR'1  a' ::> Empty')         =  0
  size (TreeNOWR'1  a' ::> Node' l x r)
    =  size (TreeNOWR'1  a' ::> l) + size (a' ::>  x) + size (TreeNOWR'1  a' ::> r)
{-# LINE 2 "size" #-}
  size (ConstR ::>  x)               =  0
  size (ComR  f g ::> InCom xs)  =  sum' (tmap (\ x -> size (g ::>  x)) (f ::>  xs))
{-# LINE 2 "size" #-}
  size (Var1List'1  f ::> xs)                =  Prelude.sum [ size (f ::>  x) | x <- outList' xs ]
  size (Var1Tree'1  a' ::> InTree' Empty)  =  0
  size (Var1Tree'1  a' ::> InTree' (Node l x r))
    =  size (Var1Tree'1  a' ::> InTree' l) + size (a' ::>  x) + size (Var1Tree'1  a' ::> InTree' r)
{-# LINE 2 "size" #-}
  size (SpineR'  a' ::> x)  =  sizeSpine x
  size (a' ::>  x)            =  case spine' a' of
                                  View'C b' from to -> size (b' ::>  from x)
{-# LINE 1994 "NOW.lhs" #-}
  newtype Id a = InId { outId :: a }
{-# LINE 2011 "NOW.lhs" #-}
  newtype Char'       x  =  InChar' { outChar' :: Char }
  newtype Int'        x  =  InInt' { outInt' :: Int }
  data List' a'      x  =  Nil' | Cons' (a' x) (List' a' x)
  data Pair' a' b'  x  =  PairC' (a' x) (b' x)
  data TreeNOW' a'      x  =  Empty' | Node' (TreeNOW' a' x) (a' x) (TreeNOW' a' x)
{-# LINE 2172 "NOW.lhs" #-}
  data Type_0 ::  * ->  * where
    Char_0    ::  Type_0 Char
    Int_0     ::  Type_0 Int
    App_0_0   ::  Type_1 f -> Type_0 a -> Type_0 (f a)
{-# LINE 2 "Type0" #-}
    Use_0     ::  Type_0 a -> Instance_0 a -> Type_0 a --  im Prinzip |Map| mit Typ und zusätzlicher Indirektion
{-# LINE 2 "Type0" #-}
    Count_0R  ::  Count_0 a -> Type_0 a
{-# LINE 2181 "NOW.lhs" #-}
    Unit0     ::  Type_0 ()
    --  Map_0     ::  Map_0 aT aT -> Type_0 aT
    Default_0  ::  Type_0 a
{-# LINE 2186 "NOW.lhs" #-}
  data Type_1 :: ( * ->  *) ->  * where
    List_1    ::  Type_1 []
    TreeNOW_1    ::  Type_1 TreeNOW
    App_0_1   ::  Type_2 f -> Type_0 a -> Type_1 (f a)
{-# LINE 2193 "NOW.lhs" #-}
    Set_1    ::  Type_1 []
{-# LINE 2196 "NOW.lhs" #-}
    Abs00     ::  (forall a . Type_0 a -> Type_0 (f a)) -> Type_1 f
    Unit1     ::  Type_1 Unit'
    Id1       ::  Type_1 Id
    Sum11     ::  Type_1 f -> Type_1 g -> Type_1 (f :+.: g)
    Pair11    ::  Type_1 f -> Type_1 g -> Type_1 (f :*.: g)

    S_0_0     ::  Type_2 s -> Type_1 t -> Type_1 (S_0_0 s t)
{-# LINE 2205 "NOW.lhs" #-}
  data Type_2 :: ( * ->  * ->  *) ->  * where
    Pair_2    ::  Type_2 (,)
{-# LINE 2210 "NOW.lhs" #-}
    Either2   ::  Type_2 Either
    Abs01     ::  (forall a . Type_0 a -> Type_1 (f a)) -> Type_2 f

    K_1       ::  Type_1 s -> Type_2 (K_1 s)
{-# LINE 2230 "NOW.lhs" #-}
  intR       ::  Type_0 Int
  intR       =   Int_0

  charR      ::  Type_0 Char
  charR      =   Char_0

  pairR      ::  Type_0 a -> Type_0 b -> Type_0 ( a, b)
  pairR a b  =   Pair_2 `App_0_1` a `App_0_0` b

  listR      ::  Type_0 a -> Type_0 [ a]
  listR a    =   List_1 `App_0_0` a

  treeR      ::  Type_0 a -> Type_0 (TreeNOW a)
  treeR a    =   TreeNOW_1 `App_0_0` a
{-# LINE 2271 "NOW.lhs" #-}
  varsize    ::  Type_1 f -> f a -> Int
  varsize f  =   count_1 f (const 1)

  varsum     ::  Type_1 f -> f Int -> Int
  varsum f   =   count_1 f id
{-# LINE 2291 "NOW.lhs" #-}
  type Count_0  a  =  a -> Int
  type Count_1  f  =  forall a . Count_0 a -> Count_0 (f a)
  type Count_2  f  =  forall a . Count_0 a -> Count_1 (f a)
{-# LINE 2315 "NOW.lhs" #-}
  count_0  :: Type_0 a -> Count_0 a
  count_0 (f `App_0_0` a)   =  (count_1 f) (count_0 a)
{-# LINE 2 "count0" #-}
  count_0 (Count_0R c)     =  c
{-# LINE 2320 "NOW.lhs" #-}
  -- NEW
  count_0 (Use_0 a d)       =  case d of { Count_0I c -> c ; otherwise -> count_0 a }
{-# LINE 2323 "NOW.lhs" #-}
  count_0 t                 =  const 0

  count_1  :: Type_1 a -> Count_1 a
  count_1 List_1           c  =  sumList . mapList c
  count_1 TreeNOW_1           c  =  count_1 List_1 c . inorder
  count_1 (f `App_0_1` a)  c  =  (count_2 f) (count_0 a) c

  count_2  :: Type_2 a -> Count_2 a
  count_2 (Pair_2) c1 c2  =  \ (x1, x2) -> c1 x1 + c2 x2
{-# LINE 2376 "NOW.lhs" #-}
  type Map_0  a1  a2  =  a1 -> a2
  type Map_1  f1  f2  =  forall a1 a2 . Map_0 a1 a2 -> Map_0 (f1 a1) (f2 a2)
  type Map_2  f1  f2  =  forall a1 a2 . Map_0 a1 a2 -> Map_1 (f1 a1) (f2 a2)
{-# LINE 2384 "NOW.lhs" #-}
  map_0  :: Type_0 a -> Map_0 a a
  map_0 Int_0          =  id
  map_0 Char_0         =  id
  map_0 (App_0_0 f a)  =  (map_1 f) (map_0 a)
{-# LINE 2391 "NOW.lhs" #-}
  --  map_0 (Map_0 m)       =  m
  map_0 Unit0          =  id
{-# LINE 2394 "NOW.lhs" #-}
  map_1  :: Type_1 f -> Map_1 f f
  map_1 List_1          =  mapList
  map_1 TreeNOW_1          =  mapTree
  map_1 (App_0_1 f a)   =  (map_2 f) (map_0 a)
{-# LINE 2401 "NOW.lhs" #-}
  --  Typ von |Map_0| ist zu speziell:
  --  |map_1 (Abs00 f)    =  \ m -> map_0 (f (Map_0 m))|

  map_1 (S_0_0 s t) = \ x -> InS_0_0 . (map_2 s x) (map_1 t x) . outS_0_0

  map_1 f = \ m -> fromSpine' . mapSpine m . toSpine1 f

{- The rest will never be matched!
  map_1 f = case structure1 f of
             View_1C g fromData toData -> \ m -> toData . map_1 g m . fromData
-}
{-# LINE 2411 "NOW.lhs" #-}
  toSpine1 :: Type_1 f -> f a -> Spine' f a
  toSpine1 List_1 x = toSpine' (ListR' ::>  x)
{-# LINE 2414 "NOW.lhs" #-}
  map_2  :: Type_2 f -> Map_2 f f
  map_2 Pair_2 f g (a, b)   =  (f a, g b)
{-# LINE 2419 "NOW.lhs" #-}
  map_2 Either2 f g (Left a)  =  Left (f a)
  map_2 Either2 f g (Right b)  =  Right (g b)
  map_2 (K_1 s) x a elem = (InK_1 . map_1 s a  . outK_1) elem
{-# LINE 2460 "NOW.lhs" #-}
  type Pretty_0 a = a -> Text
  type Pretty_1 a = forall x . Type_0 x -> Pretty_0 (a x)
  type Pretty_2 a = forall x . Type_0 x -> Pretty_1 (a x)
{-# LINE 2472 "NOW.lhs" #-}
  pretty_0  :: Type_0 a -> Pretty_0 a
  pretty_0 Char_0  c            =  prettyChar  c
  pretty_0 Int_0   n            =  prettyInt   n
  pretty_0 (f `App_0_0` a)  x   =  pretty_1 f a x

  pretty_1  :: Type_1 a -> Pretty_1 a
  pretty_1 List_1 a xs          =  bracketed [ pretty_0 a x | x <- xs ]
  pretty_1 TreeNOW_1 a Empty       =  text "Empty"
  pretty_1 TreeNOW_1 a (Node l x r)
    =   align "(Node " (  pretty_1 TreeNOW_1 a l  <> nl <>
                          pretty_0 a x        <> nl <>
                          pretty_1 TreeNOW_1 a r  <> text ")")
  pretty_1 (f `App_0_1` a) b x  =  pretty_2 f a b x

  pretty_2  :: Type_2 a -> Pretty_2 a
  pretty_2 Pair_2 a b (x, y)    =  align "( " (pretty_0 a x) <> nl <>
                                   align ", " (pretty_0 b y) <> text ")"
{-# LINE 2595 "NOW.lhs" #-}
  count :: Type a -> (a -> Int)
  count (CharR)      =  const 0
  count (IntR)       =  const 0
  count (PairR a b)  =  \ (x, y) -> count a x + count b y
  count (ListR a)    =  sumList . mapList (count a)
  count (TreeNOWR a)    =  sumList . mapList (count a) . inorder
{-# LINE 3 "count" #-}
  --  environment look-up
  count (Default)    =  const 0
{-# LINE 6 "count" #-}
  count (Use a d)    =  case d of { CountI c -> c; otherwise -> count a }
{-# LINE 2 "count" #-}
  count (CountR c)      =  c
{-# LINE 2678 "NOW.lhs" #-}
  infixl `Use`
{-# LINE 2686 "NOW.lhs" #-}
  data Instance ::  * ->  * where
    PrettyI   :: (a -> Text)            -> Instance a
    CountI    :: (a -> Int)             -> Instance a
{-# LINE 2692 "NOW.lhs" #-}
    MapI      :: (a -> a)              -> Instance a
    CompareI  :: (a -> a -> Ordering)  -> Instance a
{-# LINE 2778 "NOW.lhs" #-}

{-# LINE 2816 "NOW.lhs" #-}
  type PType_0  polyT a  =  PType polyT a
  type PType_1  polyT a  =  forall x . PType_0 polyT x -> PType_0 polyT (a x)

  type PPoly_0  polyT a  =  polyT a
  type PPoly_1  polyT a  =  forall x . PType_0 polyT x -> PPoly_0 polyT (a x)
{-# LINE 2838 "NOW.lhs" #-}
  newtype Count a = InCount { outCount :: a -> Int }

  pcount :: PType Count a -> (a -> Int)
  pcount (PVar c)      =  outCount c
  pcount (PCharR)      =  const 0
  pcount (PIntR)       =  const 0
  pcount (PPairR a b)  =  \ (x, y) -> pcount a x + pcount b y
  pcount (PListR a)    =  sumList . mapList (pcount a)
  pcount (PTreeNOWR a)    =  sumList . mapList (pcount a) . inorder
{-# LINE 2887 "NOW.lhs" #-}
  psize f  =  pcount (f a)  where  a  =  PVar (InCount (const 1))
  psum  f  =  pcount (f a)  where  a  =  PVar (InCount id)
{-# LINE 2915 "NOW.lhs" #-}
  psize  :: (PType Count a   -> PType Count b)  -> (b -> Int)
  psum   :: (PType Count Int  -> PType Count b)  -> (b -> Int)
{-# LINE 3008 "NOW.lhs" #-}
  size_1 :: (Type_0 a -> Type_0 b) -> Count_0 b
  size_1 (f) = count_0 (f a) where a = Use_0 Default_0 (Count_0I (const 1))
{-# LINE 3013 "NOW.lhs" #-}
  setR a    =   Set_1 `App_0_0` a
{-# LINE 3016 "NOW.lhs" #-}
  type MyMap_0  a  =  a -> a
  type MyMap_1  f  =  forall a . Type_0 a -> MyMap_0 (f a)
  type MyMap_2  f  =  forall a . Type_0 a -> MyMap_1 (f a)
{-# LINE 3023 "NOW.lhs" #-}
  mymap_0 :: Type_0 a -> MyMap_0 a
  mymap_0 Int_0            =  id
  --  |mymap_0 (Map_0 m)         =  m|
  mymap_0 (Use_0 a d)      =  case d of { Map_0I m -> m ; otherwise -> mymap_0 a }
  mymap_0 (App_0_0 f s)    =  (mymap_1 f) s
{-# LINE 3032 "NOW.lhs" #-}
  mymap_1 :: Type_1 f -> MyMap_1 f
  mymap_1 List_1 a         =  mapList (mymap_0(a))
  mymap_1 Set_1  a         =  sortBy (compare0(a)) . mapList (mymap_0(a))
  mymap_1 (App_0_1 f s) a  =  (mymap_2 f) s a

  mymap_2 :: Type_2 f -> MyMap_2 f
  mymap_2 Pair_2 a b = \ (x, y) -> (mymap_0(a) x, mymap_0(b) y)
{-# LINE 3041 "NOW.lhs" #-}
  compare0 :: Type_0 a -> a -> a -> Ordering
  compare0 (Int_0)      =  compareInt
  compare0 (Use_0 a d)  =  case d of { Compare_0I c -> c ; otherwise -> compare0 a }
{-# LINE 3045 "NOW.lhs" #-}
  data Instance_0 ::  * ->  * where
    Map_0I      :: (a -> a)              -> Instance_0 a
    Compare_0I  :: (a -> a -> Ordering)  -> Instance_0 a
    Count_0I    :: (a -> Int)             -> Instance_0 a
{-# LINE 3066 "NOW.lhs" #-}
  compareOp x y = op (Prelude.compare x y)
  op LT  =  GT
  op EQ  =  EQ
  op GT  =  LT
{-# LINE 3095 "NOW.lhs" #-}
  class Rep a where
    rep  ::  Type a
  instance Rep Int where
    rep  =   IntR
  instance Rep Char where
    rep  =   CharR
  instance (Rep a) => Rep [a] where
    rep  =   ListR rep
  instance (Rep a, Rep b) => Rep (a, b) where
    rep  =   PairR rep rep
  instance (Rep a) => Rep (TreeNOW a) where
    rep  =   TreeNOWR rep
{-# LINE 3109 "NOW.lhs" #-}
  typed :: forall a . (Rep a) => a -> Typed a
  typed a  =  rep :>  a
{-# LINE 3113 "NOW.lhs" #-}
  cpretty :: (Rep a) => a -> Text
  cpretty x = pretty (typed x)
{-# LINE 3144 "NOW.lhs" #-}
  data Rose f a  =  Branch a (f (Rose f a))
{-# LINE 3151 "NOW.lhs" #-}
  sum' :: Typed' f Int -> Int
  sum' (IdR ::>  InId n)  =  n
  sum' (ListR' ::>  xs)     =  sumList xs
{-# LINE 3174 "NOW.lhs" #-}
  map' :: Type' f -> (forall a b . (a -> b) -> (f a -> f b))
  map' (ListR')     m x              =  mapList m x
  map' (TreeNOWR')     m Empty          =  Empty
  map' (TreeNOWR')     m (Node l a r)   =  Node (map' TreeNOWR' m l) (m a) (map' TreeNOWR' m r)
  map' (Rose'0 f)   m (Branch a ts)  =  Branch (m a) (map' f (map' (Rose'0 f) m) ts)

  map' (IdR)        m x               =  (InId . m . outId) x
  map' (CharR'0)     m c              =  (InChar'  . outChar'  )  c
  map' (IntR'0)      m i              =  (InInt'   . outInt'   )  i
  map' (ListR'1 a')  m Nil'           =  Nil'
  map' (ListR'1 a')  m (Cons' x xs)   =  Cons' (map' a' m x) (map' (ListR'1 a') m xs)
  map' (TreeNOWR'1 a')  m Empty'         =  Empty'
  map' (TreeNOWR'1 a')  m (Node' l x r)  =  Node' (map' (TreeNOWR'1 a') m l) (map' a' m x) (map' (TreeNOWR'1 a') m r)
{-# LINE 2 "map" #-}
  map' (Var1List'1 f)   m xs         =  (InList' . map' ListR' (map' f m) . outList') xs
  map' (Var1Tree'1 f)   m t          =  (InTree' . map' TreeNOWR' (map' f m) . outTree') t
{-# LINE 2 "map" #-}
  map' (Unit'R)     m Unit'C        =  Unit'C
  map' (Const' a)   m (InConst x)   =  InConst x
  map' (Sum' f g)   m (Inl' a)      =  Inl' (map' (f) m a)
  map' (Sum' f g)   m (Inr' b)      =  Inr' (map' (g) m b)
  map' (Pair' f g)  m (Pair'C a b)  =  Pair'C (map' (f) m a) (map' (g) m b)
  map' (ComR f g)   m x             =  (InCom . map' f (map' g m) . outCom) x
{-# LINE 2 "map" #-}
  map' f m x = (fromSpine' . mapSpine m . toSpine') (f ::>  x)
{- -- The rest will never be matched!
{-# LINE 2 "map" #-}
  map' f m x  =  case view' f of
                 View'C g fromData toData -> (toData . map' g m . fromData) x
-}
{-# LINE 6 "map" #-}
  view' = structure'
{-# LINE 3195 "NOW.lhs" #-}
  tmap :: (a -> b) -> (Typed' f a -> Typed' f b)
  tmap m (f ::>  x)  =  f ::>  map' f m x
{-# LINE 3246 "NOW.lhs" #-}
  infixr  5  :->
  infixl  5  :<-

  type a :<- b  =  b -> a

  data View ::  * ->  * where
    ViewC :: Type b -> (a :-> b) -> (a :<- b) -> View a
{-# LINE 3279 "NOW.lhs" #-}
  type a :-> b  =  a -> b
{-# LINE 3288 "NOW.lhs" #-}
  infixr  5  :=>
  infixl  5  :<=

  type f :=> g  =  forall a . f a -> g a
  type f :<= g  =  forall a . g a -> f a

  data View' :: ( * ->  *) ->  * where
    View'C :: Type' g -> (f :=> g) -> (f :<= g) -> View' f
{-# LINE 3313 "NOW.lhs" #-}
  data View_1 :: ( * ->  *) ->  * where
    View_1C :: Type_1 g -> (f :=> g) -> (f :<= g) -> View_1 f
{-# LINE 3345 "NOW.lhs" #-}
  spine    ::  Type a -> View a
  spine a  =   ViewC (SpineR a) (\ x -> toSpine (a :>  x)) fromSpine
{-# LINE 3450 "NOW.lhs" #-}
  dyn     =  Descr { constr = Dyn,    name = "Dyn",     arity = 1,  fixity = Prefix 10,  order = (0, 1) }
  ty      =  Descr { constr = (:>),   name = "(:)",     arity = 2,  fixity = Infix  10,  order = (0, 1) }
{-# LINE 3454 "NOW.lhs" #-}
  fromType :: Type a -> Spine (Type a)
  fromType IntR         =  con IntR "Int"
  fromType CharR        =  con CharR "Char"
  fromType (PairR a b)  =  con PairR "Pair" :$ (TypeR  a :> a) :$ (TypeR  b :> b)
  fromType (ListR a)    =  con ListR "List" :$ (TypeR  a :> a)
  fromType (TreeNOWR a)    =  con TreeNOWR "TreeNOW" :$ (TypeR  a :> a)
  fromType DynamicR     =  con DynamicR "Dynamic"
  fromType (TypeR a)    =  con TypeR "Type" :$ (TypeR  a :> a)
  fromType (TypedR a)   =  con TypedR "Typed" :$ (TypeR  a :> a)
{-# LINE 3464 "NOW.lhs" #-}
  con c s = Con (default_Descr c s)
  default_Descr c s = Descr { constr = c, name = s, arity = error "con: unknown arity", fixity = Prefix 10, order = error "con: unknown arity"}
{-# LINE 3519 "NOW.lhs" #-}
  type Datatype a = [ Signature a]

  infixl  0  :*
  data Signature ::  * ->  * where
    Sig   ::  Constr a -> Signature a
    (:*)  ::  Signature (a -> b) -> Type a -> Signature b
{-# LINE 3547 "NOW.lhs" #-}
  datatype  :: Type a -> Datatype a
  datatype (BoolR)      =  [ Sig false, Sig true ]
  datatype (CharR)      =  [ Sig (char c) | c <- [minBound .. maxBound] ]
  datatype (IntR)       =  [ Sig (int i)  | i <- [minBound .. maxBound] ]
  datatype (ListR a)    =  [ Sig nil, Sig cons :* a :* ListR a ]
  datatype (PairR a b)  =  [ Sig pair :* a :* b ]
  datatype (TreeNOWR a)    =  [ Sig empty, Sig node :* TreeNOWR a :* a :* TreeNOWR a ]
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
{-# LINE 3559 "NOW.lhs" #-}
  eq   =  default_Descr Eq "Eq"
  ifV  =  default_Descr If "If"
{-# LINE 3577 "NOW.lhs" #-}
  generate :: Type a -> Int -> [ a]
{-# LINE 3580 "NOW.lhs" #-}
  generate IntR d     =  [ 0 | d > 0 ]
{-# LINE 3582 "NOW.lhs" #-}
  generate a  0              =  []
  generate a  (d + 1)        =  concat [ generateSpine s d | s <- datatype a ]

  generateSpine :: Signature a -> Int -> [ a]
  generateSpine (Sig c)   d  =  [constr c]
  generateSpine (s :* a)  d  =  [ f x | f <- generateSpine s d, x <- generate a d ]
{-# LINE 3599 "NOW.lhs" #-}
  test_gen1= generate (ListR BoolR) 3
  {-  \eval{test_gen1}  -}
  test_gen2= generate (ListR (ListR BoolR)) 3
  {-  \eval{test_gen2}  -}
{-# LINE 3614 "NOW.lhs" #-}
  readsPrec  :: Type a -> Int -> ReadS a
  readsPrec (CharR)        d  =  readsPrecChar d
  readsPrec (IntR)         d  =  readsPrecInt d
  readsPrec ((ListR CharR))      d  =  readsPrecString d
  readsPrec (ListR a)      d  =  readsList (reads a)
  readsPrec (PairR a b)    d
    =  readParen False (\ s0 -> [ ((x, y), s5) |  ("(",  s1)  <-  lex      s0,
                                                  (x,    s2)  <-  reads a  s1,
                                                  (",",  s3)  <-  lex      s2,
                                                  (y,    s4)  <-  reads b  s3,
                                                  (")",  s5)  <-  lex      s4 ])
  readsPrec a              d
    =  alt [ readParen (arity' s > 0 && d > 10) (readsSpine s) | s <- datatype a ]
{-# LINE 3643 "NOW.lhs" #-}
  readsSpine :: Signature a -> ReadS a
  readsSpine (Sig c)   s0  =  [ (constr c, s1) | (t, s1) <- lex s0, name c == t ]
  readsSpine (s :* a)  s0  =  [ (f x, s2) |  (f,  s1)  <-  readsSpine s        s0,
                                             (x,  s2)  <-  readsPrec a 11  s1 ]
{-# LINE 3650 "NOW.lhs" #-}
  arity' :: Signature a -> Int
  arity' (Sig c)   =  0
  arity' (s :* a)  =  arity' s + 1
{-# LINE 3657 "NOW.lhs" #-}
  reads  ::  Type a -> ReadS a
  reads a   =  readsPrec a 0

  read   ::  Type a -> String -> a
  read a s  =  case [ x | (x, t) <- reads a s, ("", "") <- lex t ] of
               [x]  ->  x
               []   ->  error "read: no parse"
               _    ->  error "read: ambiguous parse"
{-# LINE 3723 "NOW.lhs" #-}
  gen a d = putStrLn (show (ListR  a :> generate a d))
  test_gen4= gen (ExprR IntR) 4
    {-  \eval{test_gen4}  -}
  test_gen5= gen (ExprR BoolR) 4
    {-  \eval{test_gen5}  -}
  test_gen6= gen (ExprR CharR) 4
    {-  \eval{test_gen6}  -}
{-# LINE 3772 "NOW.lhs" #-}
  infixr :+>
  newtype (f :+> g) x  =  Fun { app :: f x -> g x }
{-# LINE 3780 "NOW.lhs" #-}
  nil'   ::  forall x . forall a' . (List' a') x
  nil'   =   Nil'

  cons'  ::  forall x . forall a' . (a' :+> List' a' :+> List' a') x
  cons'  =   Fun (\ x -> Fun (\ xs -> Cons' x xs))
{-# LINE 3791 "NOW.lhs" #-}
  data Spine' :: ( * ->  *) ->  * ->  * where
    Con'   ::  (forall x . f x) -> Spine' f a
    (:$$)  ::  Spine' (f :+> g) a -> Typed' f a -> Spine' g a
{-# LINE 3805 "NOW.lhs" #-}
  fromSpine' :: Spine' f a -> f a
  fromSpine' (Con' c)   =  c
  fromSpine' (f :$$ x)  =  fromSpine' f `app` val' x
{-# LINE 3822 "NOW.lhs" #-}
  toSpine' :: Typed' f a -> Spine' f a
  toSpine' (ListR'1  a' ::> Nil')             =  Con' nil'
  toSpine' (ListR'1  a' ::> Cons' x xs)     =  Con' cons' :$$ (a' ::>  x) :$$ (ListR'1  a' ::> xs)
{-# LINE 2 "toSpine1" #-}
  toSpine' (PerfectR'1  a' ::> Zero'C x)     =  Con' zero' :$$ (a' ::>  x)
  toSpine' (PerfectR'1  a' ::> Succ'C x)     =  Con' succ' :$$ (PerfectR'1  (PairR'2 a' a') ::> x)
{-# LINE 3830 "NOW.lhs" #-}
  toSpine' (CharR'0 ::>  c)                     =  Con' (InChar' (outChar' c))
  toSpine' (IntR'0 ::>   i)                     =  Con' (InInt' (outInt' i))
  toSpine' (PairR'2  a' b' ::> PairC' x y)  =  Con' pair' :$$ (a' ::>  x) :$$ (b' ::>  y)
  toSpine' (TreeNOWR'1  a' ::> Empty')           =  Con' empty'
  toSpine' (TreeNOWR'1  a' ::> Node' l x r)    =  Con' node' :$$ (TreeNOWR'1  a' ::> l) :$$ (a' ::>  x) :$$ (TreeNOWR'1  a' ::> r)
{-# LINE 3837 "NOW.lhs" #-}
  toSpine' (ListR' ::>  [])          =  Con' []
  toSpine' (ListR' ::>  a :  as)
    =  Con' (Fun (\ (InId a) -> Fun (\ as -> a :  as))) :$$ (IdR ::>  InId a) :$$ (ListR' ::>  as)
{-# LINE 3841 "NOW.lhs" #-}
  pair'   ::  forall x . forall a' b' . (a' :+> b' :+> Pair' a' b') x
  pair'   =   Fun (\ x -> Fun (\ y -> PairC' x y))

  zero'   ::  forall x . forall a' . (a' :+> Perfect' a') x
  zero'   =   Fun (\ x -> Zero'C x)

  succ'   ::  forall x . forall a' . (Perfect' (Pair' a' a') :+> Perfect' a') x
  succ'   =   Fun (\ x -> Succ'C x)

  empty'  ::  forall x . forall a' . (TreeNOW' a') x
  empty'  =   Empty'

  node'   ::  forall x . forall a' . (TreeNOW' a' :+> a' :+> TreeNOW' a' :+> TreeNOW' a') x
  node'   =   Fun (\ l -> Fun (\ x -> Fun (\ r -> Node' l x r)))
{-# LINE 3867 "NOW.lhs" #-}

  spine' :: Type' f -> View' f
{-# LINE 2 "spineView" #-}
  spine' ListR' = View'C (ListR'1 IdR) (fromList InId) (toList outId)
{-# LINE 3871 "NOW.lhs" #-}
  spine' (TreeNOWR')     =  View'C (TreeNOWR'1 IdR) (fromTree InId) (toTree outId)
{-# LINE 2 "spineView" #-}
  spine' (PerfectR')
    =  View'C (PerfectR'1 IdR) (fromPerfect InId) (toPerfect outId)
{-# LINE 3874 "NOW.lhs" #-}
  spine' a'     = View'C (SpineR' a') (\ x -> toSpine' (a' ::>  x)) fromSpine'
{-# LINE 3891 "NOW.lhs" #-}
  sizeSpine :: Spine' f a -> Int
  sizeSpine (Con' c)   =  0
  sizeSpine (f :$$ x)  =  sizeSpine f + size x
{-# LINE 3906 "NOW.lhs" #-}
  map :: Type' f -> (a -> b) -> (f a -> f b)
  map IdR           m  =  InId . m . outId
  map (SpineR' a')  m  =  mapSpine m
  map a'            m  =  case spine' a' of
                          View'C b' from to -> to . map b' m . from

  mapSpine :: (a -> b) -> (Spine' f a -> Spine' f b)
  mapSpine m (Con' c)            =  Con' c
  mapSpine m (f :$$ (a' ::>  x))  =  mapSpine m f :$$ (a' ::>  map a' m x)
{-# LINE 3950 "NOW.lhs" #-}
  fromList  :: (a -> a' x)  ->  ([ a] -> List' a' x)
  fromList from []          =  Nil'
  fromList from (x :  xs)  =  Cons' (from x) (fromList from xs)

  toList    :: (a' x -> a)  ->  (List' a' x -> [ a])
  toList to Nil'           =  []
  toList to (Cons' x xs)   =  ( to x) : (toList to xs)
{-# LINE 3960 "NOW.lhs" #-}
  fromTree  :: (a -> a' x)  ->  (TreeNOW a -> TreeNOW' a' x)
  fromTree f Empty         =  Empty'
  fromTree f (Node l x r)  =  Node' (fromTree f l) (f x) (fromTree f r)

  toTree    :: (a' x -> a)  ->  (TreeNOW' a' x -> TreeNOW a)
  toTree f Empty'         =  Empty
  toTree f (Node' l x r)  =  Node (toTree f l) (f x) (toTree f r)
{-# LINE 3984 "NOW.lhs" #-}
  ts = [ tree [0 .. i :: Int] | i <- [0 .. 9] ]
  test_size1= size (ListR' ::>  ts)
  {-  \eval{test_size1}  -}
  test_size5= size (ListR'1  (TreeNOWR'1 IntR'0) ::> fromList (fromTree InInt') ts)
  {-  \eval{test_size5}  -}
  test_size2= size (IdR ::>  InId ts)
  {-  \eval{test_size2}  -}
  test_size3= size (ListR'1  IdR ::> fromList InId ts)
  {-  \eval{test_size3}  -}
  test_size4= size (ListR'1  (TreeNOWR'1 IdR) ::> fromList (fromTree InId) ts)
  {-  \eval{test_size4}  -}
{-# LINE 4020 "NOW.lhs" #-}
  data Perfect' a' x = Zero'C (a' x) | Succ'C (Perfect' (Pair' a' a') x)

{-# LINE 4039 "NOW.lhs" #-}

  fromPerfect  :: (a -> a' x)  ->  (Perfect a -> Perfect' a' x)
  fromPerfect from (Zero x)  =  Zero'C (from x)
  fromPerfect from (Succ x)  =  Succ'C (fromPerfect (fromPair from from) x)

  toPerfect    :: (a' x -> a)  ->  (Perfect' a' x -> Perfect a)
  toPerfect to (Zero'C x)   =  Zero (to x)
  toPerfect to (Succ'C x)   =  Succ (toPerfect (toPair to to) x)
{-# LINE 4051 "NOW.lhs" #-}
  fromPair  :: (a -> a' x)  ->  (b -> b' x)  ->  (( a, b) -> Pair' a' b' x)
  fromPair f g (x, y)      =  PairC' (f x) (g y)

  toPair    :: (a' x -> a)  ->  (b' x -> b)  ->  (Pair' a' b' x -> ( a, b))
  toPair f g (PairC' x y)  =  (f x, g y)
{-# LINE 4065 "NOW.lhs" #-}
  test_perfect1= size (PerfectR' ::>  Succ (Zero (1, 2)))
  {-  \eval{test_perfect1}  -}
  test_perfect2= map (PerfectR') (+1) (Succ (Zero (1, 2)))
  {-  \eval{test_perfect2}  -}
{-# LINE 4091 "NOW.lhs" #-}
  data Expr' :: ( * ->  *) ->  * ->  * where
    Num' :: Int' x -> Expr' Int' x
{-# LINE 4146 "NOW.lhs" #-}
  infixr  7  :*:
  infixr  6  :+:

  data ZeroT

  data Unit       =  UnitC

  data a :+: b  =  Inl a | Inr b

  data a :*: b  =  PairC { outl :: a, outr :: b }
{-# LINE 4171 "NOW.lhs" #-}
  infixr  7  :*$
  infixr  6  :+$

{-# LINE 4185 "NOW.lhs" #-}
  structure :: Type a -> View a
  structure BoolR = ViewC (UnitR :+$ UnitR) fromBool toBool
    where
    fromBool  ::  Bool -> Unit :+: Unit
    fromBool False      =  Inl UnitC
    fromBool True       =  Inr UnitC

    toBool    ::  Unit :+: Unit -> Bool
    toBool (Inl UnitC)  =  False
    toBool (Inr UnitC)  =  True
  structure (ListR a) = ViewC (UnitR :+$ a :*$ ListR a) fromList toList
    where
    fromList  ::  [ a] -> Unit :+: a :*: [ a]
    fromList []               =  Inl UnitC
    fromList (x :  xs)       =  Inr (PairC x xs)
    toList    ::  Unit :+: a :*: [ a] -> [ a]
    toList (Inl UnitC)         =  []
    toList (Inr (PairC x xs))  =  x :  xs
  structure (TreeNOWR a) = ViewC (UnitR :+$ TreeNOWR a :*$ a :*$ TreeNOWR a) fromTree toTree
    where
    fromTree  ::  TreeNOW a -> Unit :+: TreeNOW a :*: a :*: TreeNOW a
    fromTree Empty                      =  Inl UnitC
    fromTree (Node l x r)               =  Inr (PairC l (PairC x r))
    toTree    ::  Unit :+: TreeNOW a :*: a :*: TreeNOW a -> TreeNOW a
    toTree (Inl UnitC)                  =  Empty
    toTree (Inr (PairC l (PairC x r)))  =  Node l x r
{-# LINE 4214 "NOW.lhs" #-}
  structure NatR = ViewC (UnitR :+$ NatR) fromNat toNat
    where
    fromNat  ::  Nat -> Unit :+: Nat
    fromNat Z               =  Inl UnitC
    fromNat (S x)       =  Inr x
    toNat    ::  Unit :+: Nat -> Nat
    toNat (Inl UnitC)         =  Z
    toNat (Inr x)  =  S x
{-# LINE 2 "structure" #-}
  structure (ExprR BoolR)  =  ViewC expr fromExpr toExpr
    where
    expr                        =  ExprR IntR :*$ ExprR IntR :+$
                                   ExprR BoolR :*$ ExprR BoolR :*$ ExprR BoolR

    fromExpr :: Expr Bool -> (Expr Int :*: Expr Int) :+: 
                             (Expr Bool :*: Expr Bool :*: Expr Bool)
    fromExpr (Eq x1 x2)         =  Inl (PairC x1 x2)
    fromExpr (If x1 x2 x3)      =  Inr (PairC x1 (PairC x2 x3))

    toExpr (Inl (PairC x1 x2))  =  Eq x1 x2
    toExpr (Inr (PairC x1 (PairC x2 x3)))
                                =  If x1 x2 x3

  structure (ExprR IntR)   =  ViewC expr fromExpr toExpr
    where
    expr                        =  IntR :+$
                                   ExprR IntR :*$ ExprR IntR :+$
                                   ExprR BoolR :*$ ExprR IntR :*$ ExprR IntR

    fromExpr :: Expr Int -> Int :+: ((Expr Int :*: Expr Int) :+: 
                                     (Expr Bool :*: Expr Int :*: Expr Int))
    fromExpr (Num i)            =  Inl i
    fromExpr (Plus x1 x2)       =  Inr (Inl (PairC x1 x2))
    fromExpr (If x1 x2 x3)      =  Inr (Inr (PairC x1 (PairC x2 x3)))

    toExpr (Inl i)              =  Num i
    toExpr (Inr (Inl (PairC x1 x2)))
                                =  Plus x1 x2
    toExpr (Inr (Inr (PairC x1 (PairC x2 x3))))
                                =  If x1 x2 x3

  structure (ExprR a)      =  ViewC expr fromExpr toExpr
    where
    expr                             =  ExprR BoolR :*$ ExprR a :*$ ExprR a

    fromExpr (If x1 x2 x3)           =  PairC x1 (PairC x2 x3)

    toExpr (PairC x1 (PairC x2 x3))  =  If x1 x2 x3
{-# LINE 4246 "NOW.lhs" #-}
  memo :: Type a -> (a -> v) -> (a -> v)
  memo CharR      f  c            =  f c        --  no memoisation
  memo IntR       f  i            =  f i        --  no memoisation
  memo UnitR      f  UnitC        =  f_Unit
    where f_Unit                  =  f UnitC
  memo (a :+$ b)  f  (Inl x)      =  f_Inl x
    where f_Inl                   =  memo a (\ x -> f (Inl x))
  memo (a :+$ b)  f  (Inr y)      =  f_Inr y
    where f_Inr                   =  memo b (\ y -> f (Inr y))
  memo (a :*$ b)  f  (PairC x y)  =  (f_Pair x) y
    where f_Pair                  =  memo a (\ x -> memo b (\ y -> f (PairC x y)))
  memo a          f  x            =  f_View x
    where f_View                  =  case structure a of
                                     ViewC b from to -> memo b (f . to) . from
{-# LINE 4278 "NOW.lhs" #-}
  data Nat = Z | S Nat
             deriving (Show, Eq)
{-# LINE 4282 "NOW.lhs" #-}
  fib :: Nat -> Int
  fib Z = 0
  fib (S Z) = 1
  fib (S (S n)) = fib (S n) + fib n
{-# LINE 4287 "NOW.lhs" #-}
  fib' :: Nat -> Int
  fib' = memo NatR fib
    where
    fib Z = 0
    fib (S Z) = 1
    fib (S (S n)) = fib' (S n) + fib' n
{-# LINE 4294 "NOW.lhs" #-}
  integer n = integer' n 0
  integer' Z acc = acc
  integer' (S n) acc = let acc' = acc + 1 in acc' `seq` integer' n acc'
{-# LINE 4298 "NOW.lhs" #-}
  gx Z = 0
  gx (S Z) = fib 30
  gx (S n) = gx n + 1
{-# LINE 4303 "NOW.lhs" #-}
  gx' = memo NatR gx
    where gx Z = 0
          gx (S Z) = fib 30
          gx (S n) = gx' n + 1
{-# LINE 4312 "NOW.lhs" #-}
  instance Num Nat where
    fromInteger 0 = Z
    fromInteger (n + 1) = S (fromInteger n)
    Z + n = n
    S m + n = S (m + n)
    Z * n = Z
    S m * n = (m * n) + n
    Z - n = Z
    S m - Z = S m
    S m - S n = m - n
    abs n = n
    signum Z = Z
    signum (S _) = S Z
{-# LINE 4324 "NOW.lhs" #-}
  instance Enum Nat where
      succ                      =  S

      pred Z                    =  Z
      pred (S n)                =  n

      toEnum                    =  fromInteger . toInteger
      fromEnum                  =  integer
{-# LINE 4353 "NOW.lhs" #-}
  newtype I x            =  InI { outI :: x }
  newtype K_1 s x a    =  InK_1 { outK_1 :: s a }
  newtype S_0_0 s t x  =  InS_0_0 { outS_0_0 :: (s x) (t x) }
{-# LINE 4358 "NOW.lhs" #-}
  mylist'  ::  Type_1 (S_0_0 (K_1 (Either ())) (S_0_0 (,) []))
  mylist'  =   S_0_0 (K_1 (Either2 `App_0_1` Unit0)) (S_0_0 Pair_2 List_1)

  fromMyList' :: [] :=> S_0_0 (K_1 (Either ())) (S_0_0 (,) [])
  fromMyList' []        =  InS_0_0 (InK_1 (Left ()))
  fromMyList' (a : as)  =  InS_0_0 (InK_1 (Right (InS_0_0 (a, as))))

  toMyList' :: [] :<= S_0_0 (K_1 (Either ())) (S_0_0 (,) [])
  toMyList' (InS_0_0 (InK_1 (Left ())))                  =  []
  toMyList' (InS_0_0 (InK_1 (Right (InS_0_0 (a, as)))))  =  a : as
{-# LINE 4374 "NOW.lhs" #-}
  compare :: Type a -> a -> a -> Ordering
  compare CharR      c1             c2        =  compareChar  c1 c2
  compare IntR       i1             i2        =  compareInt   i1 i2
  compare UnitR      UnitC          UnitC     =  EQ
  compare (a :+$ b)  (Inl x1)       (Inl x2)  =  compare a x1 x2
  compare (a :+$ b)  (Inl x1)       (Inr y2)  =  LT
  compare (a :+$ b)  (Inr y1)       (Inl x2)  =  GT
  compare (a :+$ b)  (Inr y1)       (Inr y2)  =  compare b y1 y2
  compare (a :*$ b)  (PairC x1 y1)  (PairC x2 y2)
    =  case compare a x1 x2 of
       EQ   ->  compare b y1 y2
       ord  ->  ord
  compare a          x1             x2
    =  case structure a of
       ViewC b from to -> compare b (from x1) (from x2)
{-# LINE 4438 "NOW.lhs" #-}
  infixr  7  :*.:
  infixr  6  :+.:

  data Zero' a

  data Unit' a         =  Unit'C

  data (f :+.: g) a  =  Inl' (f a) | Inr' (g a)

  data (f :*.: g) a  =  Pair'C { outl' :: f a, outr' :: g a }
{-# LINE 4461 "NOW.lhs" #-}
  structure' :: Type' f -> View' f
  structure' ListR'      =  View'C list' fromList' toList'
  structure' (Var1List'1 f)  =  View'C (list'1 f) fromList'1 toList'1
{-# LINE 4467 "NOW.lhs" #-}
  list'  ::  Type' (Unit' :+.: Id :*.: [])
  list'  =   Sum' Unit'R (Pair' IdR ListR')

  fromList' :: [] :=> Unit' :+.: Id :*.: []
  fromList' []          =  Inl' Unit'C
  fromList' (a :  as)  =  Inr' (Pair'C (InId a) as)

  toList' :: [] :<= Unit' :+.: Id :*.: []
  toList' (Inl' Unit'C)          =  []
  toList' (Inr' (Pair'C (InId a) as))  =  a :  as
{-# LINE 4480 "NOW.lhs" #-}
  list'1    ::  Type' f -> Type' (Unit' :+.: f :*.: Var1List' f)
  list'1 f  =   Sum' Unit'R (Pair' f (Var1List'1 f))

  fromList'1 :: Var1List' f :=> Unit' :+.: f :*.: Var1List' f
  fromList'1 (InList' [])          =  Inl' Unit'C
  fromList'1 (InList' (a :  as))  =  Inr' (Pair'C a (InList' as))

  toList'1 :: Var1List' f :<= Unit' :+.: f :*.: Var1List' f
  toList'1 (Inl' Unit'C)                   =  InList' []
  toList'1 (Inr' (Pair'C a (InList' as)))  =  InList' (a :  as)
{-# LINE 4504 "NOW.lhs" #-}
  data Type_23 :: ((( * ->  *) -> ( * ->  *) ->  * ->  *) ->  *) where
    Sum23   ::  Type_23 (:+.:)
    Pair_23  ::  Type_23 (:*.:)
{-# LINE 4515 "NOW.lhs" #-}
  list1  ::  Type_1 (Unit' :+.: Id :*.: [])
  list1  =   Sum11 Unit1 (Pair11 Id1 List_1)
{-# LINE 4519 "NOW.lhs" #-}
  structure1 :: Type_1 f -> View_1 f
  structure1 List_1  =  View_1C list1 fromList' toList'
--  structure1 List_1  =  View_1C mylist' fromMyList' toMyList'
{-# LINE 4909 "NOW.lhs" #-}
  newtype Var1List' f a = InList' { outList' :: [f a] }
  newtype Var1Tree' f a = InTree' { outTree' :: TreeNOW (f a) }
{-# LINE 4944 "NOW.lhs" #-}
  newtype Const a b   =  InConst { outConst :: a }
  newtype Com f g a  =  InCom { outCom :: f (g a) } --  |Com = (.)|
{-# LINE 4950 "NOW.lhs" #-}
  type VarInt'       =  Const Int
  type VarChar'      =  Const Char
  type VarList' a'  =  Com List a'
  type VarTree' a'  =  Com TreeNOW a'
{-# LINE 5025 "NOW.lhs" #-}
  inorder :: forall a . TreeNOW a -> [ a]
  inorder Empty         =  []
  inorder (Node l a r)  =  inorder l ++ [a] ++ inorder r
{-# LINE 5046 "NOW.lhs" #-}
  tree :: forall a . [ a] -> TreeNOW a
  tree x
    | null x               =  Empty
    | otherwise            =  Node (tree x1) a (tree x2)
    where (x1, a :  x2)  =  splitAt (length x `div` 2) x
{-# LINE 5055 "NOW.lhs" #-}
  perfect :: forall a . Int -> a -> Perfect a
  perfect 0        a  =  Zero a
  perfect (n + 1)  a  =  Succ (perfect n ( a, a))
{-# LINE 5071 "NOW.lhs" #-}
  text    ::  String -> Text
  nl      ::  Text
  indent  ::  Int -> Text -> Text
  (<>)    ::  Text -> Text -> Text
{-# LINE 5081 "NOW.lhs" #-}
  data Text  =  TextC String
             |  NL
             |  Indent Int Text
             |  Text :<> Text

  text    =  TextC
  nl      =  NL
  indent  =  Indent
  (<>)    =  (:<>)
{-# LINE 5095 "NOW.lhs" #-}
  render' :: Int -> Text -> String -> String
  render' i (TextC s)    x  =  s ++ x
  render' i NL           x  =  "\n" ++ replicate i ' ' ++ x
  render' i (Indent j d) x  =  render' (i + j) d x
  render' i (d1 :<> d2)  x  =  render' i d1 (render' i d2 x)

  render :: Text -> String
  render d                  =  render' 0 d ""
{-# LINE 5106 "NOW.lhs" #-}
  append  ::  [ Text] -> Text
  append  =   foldr (<>) (text "")

  bracketed :: [ Text] -> Text
  bracketed []          =   text "[]"
  bracketed (d :  ds)  =   align "[ " d
                         <>  append [ nl <> align ", " d | d <- ds ] <> text "]"
{-# LINE 5122 "NOW.lhs" #-}
  instance Show Text where
    showsPrec p x = render' 0 x
{-# LINE 5140 "NOW.lhs" #-}
  alt :: [ ReadS a] -> ReadS a
  alt rs = \ s -> concatMap (\ r -> r s) rs
{-# LINE 5147 "NOW.lhs" #-}
  readsList :: ReadS a -> ReadS [ a]
  readsList r = readParen False (\ s -> [ x | ("[", s1) <- lex s, x <- readl s1 ])
    where  readl   s  =   [ ([],        s1)  |  ("]",  s1)  <-  lex     s ]
                      ++  [ (x :  xs,  s2)  |  (x,    s1)  <-  r       s,
                                                 (xs,   s2)  <-  readl'  s1 ]
           readl'  s  =   [ ([],        s1)  |  ("]",  s1)  <-  lex     s ]
                      ++  [ (x :  xs,  s3)  |  (",",  s1)  <-  lex     s,
                                                 (x,    s2)  <-  r       s1,
                                                 (xs,   s3)  <-  readl'  s2  ]
{-# LINE 5180 "NOW.lhs" #-}
  data Kind = Star | Arrow Kind Kind

  instance Show Kind where
    showsPrec d Star
      =  Prelude.showString "*"
    showsPrec d (Arrow k1 k2)
      =  showParen (d > 9) (Prelude.showsPrec 10 k1 . Prelude.showString " -> " . Prelude.showsPrec 9 k2)
{-# LINE 5189 "NOW.lhs" #-}
  kinds = Star :  (diag [ [ Arrow k1 k2 | k2 <- kinds ] | k1 <- kinds ])
{-# LINE 5191 "NOW.lhs" #-}
  diag :: [[a]] -> [a]
  diag x  =  diagn 1 x

  diagn n []  =  []
  diagn n x    =  [ a | a :  y <- px ] ++ diagn (length px + 1) ([ y | a :  y <- px ] ++ drop n x)
    where px   =  [ z | z@(a :  y) <- take n x ]
{-# LINE 5220 "NOW.lhs" #-}
  v0=tree [0 .. 3]
  {- \eval{tree [0 .. 3]} -}
