% ghci -fglasgow-exts ComparingGP.lhs

> module ComparingGP
> where
> import XBitz
>
> infixr 9 `o`
> f `o` g                       =  \a -> f (g a)

> data Tree      =  Leaf Int | Binn Tree Char Tree
> data     Company   =  C [Dept]
> data     Dept      =  D Name Manager [SubUnit]
> data     SubUnit   =  PU Employee | DU Dept
> data     Employee  =  E Person Salary
> data     Person    =  P Name Address
> data     Salary    =  S Float
> type     Manager   =  Employee
> type     Name      =  String
> type     Address   =  String

> encodeChar c  =  char2bits c
> encodeInt i   =  let ib = int2bits i in addzeroes (8-length ib) ib

> decodesChar bits  = [(bits2char (take 8 bits),drop 8 bits)]
> decodesInt bits   = [(bits2int (take 8 bits),drop 8 bits)]

> eqChar x y  =  x == y
> eqInt  x y  =  x == y

> rElem                         :: forall tT . Rep tT -> tT -> [tT] -> Bool
> rElem rT t x                  =  or [ rEqual rT t a | a <- x ]

> newtype Tri aT bT cT          =  ToTri { fromTri :: Pair aT (Pair bT cT) }

> rTri                          :: forall aT . Rep aT -> (forall bT . Rep bT -> (forall cT . Rep cT -> 
>                                    Rep (Tri aT bT cT)))
> rTri rA rB rC                 =  RPair rA (rPair rB rC) (EP fromTri ToTri)

> printf                                :: String -> [Dynamic] -> ShowS
> printf "" _                           =  showString ""
> printf ('%' : 'd' : cs) (d : ds)      =  shows (cast d :: Int) 
>                                            `o` printf cs ds
> printf ('%' : 's' : cs) (d : ds)      =  showString (cast d :: String) 
>                                            `o` printf cs ds
> printf ('%' : '%' : cs) ds            =  showChar '%' `o` printf cs ds
> printf ('%' : c   : cs) []            =  error "printf: missing \ 
>                                                 \argument"
> printf (c : cs) ds                    =  showChar c `o` printf cs ds

> {-
> data Tree fT aT               =  Node aT (fT (Tree fT aT))

> rTree                         :: forall fT . (forall aT . Rep aT -> Rep (fT aT)) -> 
>                                                (forall aT . Rep aT -> Rep (Tree fT aT)) {-"."-}

> fromTree                      :: forall fT aT . Tree fT aT -> Pair aT (fT (Tree fT aT))
> fromTree (Node a ts)          =  (a :*: ts)
>
> toTree                        :: forall fT aT . Pair aT (fT (Tree fT aT)) -> Tree fT aT
> toTree  (a :*: ts)            =  Node a ts

> rTree rF rA                   =  RType (App "Tree" [term' rF, term rA])
>                                        (RCon "Node" (rPair rA (rF (rTree rF rA))))
>                                        (EP fromTree toTree)
> -}

> cShow                         :: forall tT . (Representable tT) => tT -> String
> cShow t                       =  rShows rep t ""

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function encode}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> encode :: forall tT . Rep tT -> tT -> [Bit]
> encode (RChar      ep)  t  =  encodeChar (from ep t)
> encode (RInt       ep)  t  =  encodeInt (from ep t)
> encode (RUnit      ep)  t  =  case from ep t of
>                               Unit -> []
> encode (RSum a b   ep)  t  =  case from ep t of 
>                               Inl x ->  O : encode a x
>                               Inr y ->  I : encode b y
> encode (RPair a b  ep)  t  =  case from ep t of
>                               x :*: y -> encode a x ++ encode b y
> encode (RType e a  ep)  t  =  encode a (from ep t)
> encode (RCon s a)       t  =  encode a t

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function decode}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> decodes :: forall tT . Rep tT -> [Bit] -> [(tT, [Bit])]
> decodes (RChar      ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesChar  bs ]
> decodes (RInt       ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesInt   bs ]
> decodes (RUnit      ep)  bs        =  [ (to ep Unit, bs) ]
> decodes (RSum a b   ep)  []        =  []
> decodes (RSum a b   ep)  (O : bs)  =  [ (to ep (Inl x),  cs)  |  (x,  cs) <- decodes a bs ]
> decodes (RSum a b   ep)  (I : bs)  =  [ (to ep (Inr y),  cs)  |  (y,  cs) <- decodes b bs ]
> decodes (RPair a b  ep)  bs        =  [ (to ep (x :*: y), ds)  |  (x,  cs) <- decodes a bs
>                                                                ,  (y,  ds) <- decodes b cs ]
> decodes (RType e a  ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodes a bs ]
> decodes (RCon s a)       bs        =  decodes a bs

> decode :: Rep aT -> [Bit] -> aT
> decode a bs  =  case decodes a bs of
>                 [(x, [])]  ->  x
>                 _          ->  error "decode: no parse"

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function eq}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> rEqual :: forall tT . Rep tT -> tT -> tT -> Bool
> rEqual (RInt ep) t1 t2        =  from ep t1 == from ep t2
> rEqual (RChar ep) t1 t2       =  from ep t1 == from ep t2
> rEqual (RUnit ep) t1 t2       =  case (from ep t1, from ep t2) of
>                                    (Unit, Unit) -> True
> rEqual (RSum rA rB ep) t1 t2  =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> rEqual rA a1 a2
>                                    (Inr b1, Inr b2) -> rEqual rB b1 b2
>                                    _            -> False
> rEqual (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) -> 
>                                      rEqual rA a1 a2 && rEqual rB b1 b2
> rEqual (RType e rA  ep) t1 t2 =  rEqual rA (from ep t1) (from ep t2)
> rEqual (RCon s rA) t1 t2      =  rEqual rA t1 t2

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> rShows :: forall tT . Rep tT -> tT -> ShowS
> rShows (RInt         ep) t    =  shows (from ep t)
> rShows (RChar        ep) t    =  shows (from ep t)
> rShows (RUnit        ep) t    =  showString ""
> rShows (RSum rA rB   ep) t    =  case from ep t of
>                                    Inl a -> rShows rA a
>                                    Inr b -> rShows rB b
> rShows (RPair rA rB  ep) t    =  case from ep t of
>                                    (a :*: b) -> rShows rA a `o` showString " " `o` rShows rB b
> rShows (RType e rA   ep) t    =  rShows rA (from ep t)
> rShows (RCon  s (RUnit ep)) t =  showString s
> rShows (RCon  s rA)      t    =  showChar '(' `o` showString s `o` showChar ' '
>                                  `o` rShows rA t `o` showChar ')'

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function update}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Generic representation types}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data UnitT                    =  Unit
>
> data Sum  aT bT               =  Inl aT | Inr bT
>
> data Pair aT bT               =  aT :*: bT

> type (:*:) = Pair
> type (:+:) = Sum

Standard mapping functions on the above types (and on the function
type).

>
> ($+)                          :: forall aT bT . (aT -> bT) -> (forall cT dT . (cT -> dT) -> ((Sum aT cT) -> (Sum bT dT)))
> (f $+ g) (Inl a)              =  Inl (f a)
> (f $+ g) (Inr b)              =  Inr (g b)
>
> ($*)                          :: forall aT bT . (aT -> bT) -> (forall cT dT . (cT -> dT) -> ((Pair aT cT) -> (Pair bT dT)))
> (f $* g) (a :*: b)            =  f a :*: g b
>
> ($->)                         :: forall aT bT . (aT -> bT) -> (forall cT dT . (cT -> dT) -> ((bT -> cT) -> (aT -> dT)))
> (f $-> g) h                   =  g `o` h `o` f

% $
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Type equivalence}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data EPT aT bT                =  EP { from :: aT -> bT, to :: bT -> aT }

Reflexivity, symmetry and transitivity.

> self                          :: forall aT . EPT aT aT
> self                          =  EP { from = id, to = id }
>
> inv                           :: forall aT bT . (EPT aT bT) -> (EPT bT aT)
> inv f                         =  EP { from = to f, to = from f }
>
> infixr 9 {-"\;"-} <>
> (<>)                          :: forall aT bT cT . (EPT bT cT) -> (EPT aT bT) -> (EPT aT cT)
> f <> g                        =  EP { from = from f `o` from g, to = to g `o` to f }

Mapping functions for generic representation types (and for the
function type) implementing the laws of congruence.

> ($+$)                         :: forall aT bT . (EPT aT bT) -> (forall cT dT . (EPT cT dT) -> (EPT (Sum aT cT) (Sum bT dT)))
> f $+$ g                       =  EP { from = from f $+ from g, to = to f $+ to g }
>
> ($*$)                         :: forall aT bT . (EPT aT bT) -> (forall cT dT . (EPT cT dT) -> (EPT (Pair aT cT) (Pair bT dT)))
> f $*$ g                       =  EP { from = from f $* from g, to = to f $* to g }
>
> ($->$)                        :: forall aT bT . (EPT aT bT) -> (forall cT dT . (EPT cT dT) -> (EPT (aT -> cT) (bT -> dT)))
> f $->$ g                      =  EP { from = to f $-> from g, to = from f $-> to g }

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Type representations}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Rep tT                   =  {-"\phantom{\forall \alpha \;\beta \,.\,}"-} RInt     {-"}&{"-} (EPT tT Int)
>                               |  {-"\phantom{\forall \alpha \;\beta \,.\,}"-} RChar    {-"}&{"-} (EPT tT Char)
>                               |  {-"\phantom{\forall \alpha \;\beta \,.\,}"-} RDynamic {-"}&{"-} (EPT tT Dynamic)
>                               |  forall aT bT . RArrow (Rep aT) (Rep bT) {-"}&{"-} (EPT tT (aT -> bT))
>                               |                 RUnit                    {-"}&{"-} (EPT tT UnitT)
>                               |  forall aT bT . RSum   (Rep aT) (Rep bT) {-"}&{"-} (EPT tT (Sum aT bT))
>                               |  forall aT bT . RPair  (Rep aT) (Rep bT) {-"}&{"-} (EPT tT (Pair aT bT))
>                               |  forall aT {-"\phantom{\;\beta}"-} . RType Term   (Rep aT) {-"}&{"-} (EPT tT aT)
>                               |                                      RCon  String (Rep tT)


%if False
Extension for polymorphic types.

>                               |  forall fT . RForall (forall aT . Rep aT -> Rep (fT aT)) (EPT tT (Forall fT))
>
> newtype Forall fT             =  Forall { unForall :: forall aT . fT aT }
> rForall                       :: forall fT . (forall aT . Rep aT -> Rep (fT aT)) -> Rep (Forall fT)
> rForall rF                    =  RForall rF self
>
> newtype Id aT                 =  Id { unId :: aT -> aT }
> rId                           :: forall aT . Rep aT -> Rep (Id aT)
> rId rA                        =  RArrow rA rA (EP unId Id)
> dynId                         =  Dyn (rForall rId) (Forall (Id id))

%endif

Smart constructors.

> rInt                          :: Rep Int
> rInt                          =  RInt         self
> rChar                         :: Rep Char
> rChar                         =  RChar        self
> rDynamic                      :: Rep Dynamic
> rDynamic                      =  RDynamic     self
> rArrow                        :: forall aT . Rep aT -> (forall bT . Rep bT -> Rep (aT -> bT))
> rArrow rA rB                  =  RArrow rA rB self
> rUnit                         :: Rep UnitT
> rUnit                         =  RUnit        self
> rSum                          :: forall aT . Rep aT -> (forall bT . Rep bT -> Rep (Sum  aT bT))
> rSum   rA rB                  =  RSum   rA rB self
> rPair                         :: forall aT . Rep aT -> (forall bT . Rep bT -> Rep (Pair aT bT))
> rPair  rA rB                  =  RPair  rA rB self


A class for representable types.

> class Representable tT where
>   rep                         :: Rep tT
>
> instance Representable Int where
>   rep                         =  rInt
> instance Representable Char where
>   rep                         =  rChar
> instance Representable Dynamic where
>   rep                         =  rDynamic
> instance (Representable a,Representable b) => Representable (a -> b) where
>   rep                         =  rArrow rep rep
> instance (Representable aT, Representable bT) => Representable (Sum aT bT) where
>   rep                         =  rSum rep rep
> instance (Representable aT, Representable bT) => Representable (Pair aT bT) where
>   rep                         =  rPair rep rep

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Dynamics}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Dynamic                  =  forall aT . Dyn (Rep aT) aT
>
> dynamic                       :: forall aT . (Representable aT) => aT -> Dynamic
> dynamic x                     =  Dyn rep x

Run-time unification of types.

> unify'                         :: forall t1T t2T . Rep t1T -> Rep t2T -> [EPT t1T t2T]
> unify' (RInt ep1) (RInt ep2)   
>                               =  [ inv ep2 <> ep1 ]
> unify' (RChar ep1) (RChar ep2)   
>                               =  [ inv ep2 <> ep1 ]
> unify' (RDynamic ep1) (RDynamic ep2)
>                               =  [ inv ep2 <> ep1 ]
> unify' (RArrow rA1 rB1 ep1) (RArrow rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $->$ epB) <> ep1 | epA <- unify' rA1 rA2 , epB <- unify' rB1 rB2 ]
> unify' (RUnit ep1) (RUnit ep2) 
>                               =  [ inv ep2 <> ep1 ]
> unify' (RSum rA1 rB1 ep1) (RSum rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $+$ epB) <> ep1 | epA <- unify' rA1 rA2, epB <- unify' rB1 rB2 ]
> unify' (RPair rA1 rB1 ep1) (RPair rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $*$ epB) <> ep1 | epA <- unify' rA1 rA2, epB <- unify' rB1 rB2 ]
> unify' (RCon s1 rA1) (RCon s2 rA2)
>                               =  unify' rA1 rA2
> unify' (RType t1 rA1 ep1) (RType t2 rA2 ep2)
>                               =  [ inv ep2 <> head (unify' rA1 rA2) <> ep1 | t1 == t2 ]
> unify' _ _                    =  []
>
> unify                         :: forall t1T t2T . Rep t1T -> Rep t2T -> Maybe (EPT t1T t2T)
> unify r1 r2                   =  case unify' r1 r2 of
>                                    x:_ -> Just x
>                                    [] -> Nothing

Type-safe cast and dynamic function application.

> rCast                         :: forall tT . Rep tT -> Dynamic -> tT
> rCast rT (Dyn rA a)           =  case unify rT rA of
>                                    Just ep -> to ep a
>                                    Nothing -> error "cast: type mismatch"
>
> cast                          :: (Representable tT) => Dynamic -> tT
> cast d                        =  rCast rep d
>
> apply                         :: Dynamic -> Dynamic -> Dynamic
> apply (Dyn (RArrow rA rB ep)f) (Dyn rA' x)
>                               =  case unify rA rA' of
>                                    Just ep' -> Dyn rB ((from ep f) (to ep' x))
>                                    Nothing  -> error "apply: type mismatch"
> apply _ _                     =  error "apply: not a function"


% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Type terms}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Term                     =  App String [Term]
>                                  deriving (Show, Eq)
>
> term                          :: forall tT . Rep tT -> Term
> term (RInt         ep)        =  App "Int" []
> term (RChar        ep)        =  App "Char" []
> term (RDynamic     ep)        =  App "Dynamic" []
> term (RArrow rA rB ep)        =  App "->" [term rA, term rB]
> term (RUnit        ep)        =  App "1" []
> term (RSum   rA rB ep)        =  App "(+)" [term rA, term rB]
> term (RPair  rA rB ep)        =  App "(*)" [term rA, term rB]
> term (RType t rA   ep)        =  t
>
> term'                         :: forall fT . (forall aT . Rep aT -> Rep (fT aT)) -> Term
> term' rF                      =  case term (rF rUnit) of App t ts -> App t (init ts)


% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Generics}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -



Examples of type representations.

> rBool                         :: Rep Bool
> rBool                         =  RType (App "Bool" [])
>                                        (rSum (RCon "False" rUnit) (RCon "True" rUnit))
>                                        (EP fromBool toBool)
>
> fromBool                      :: Bool -> Sum UnitT UnitT
> fromBool False                =  Inl Unit
> fromBool True                 =  Inr Unit
>
> toBool                        :: Sum UnitT UnitT -> Bool
> toBool (Inl Unit)             =  False
> toBool (Inr Unit)             =  True
> rList                         :: forall aT . Rep aT -> Rep [aT]
> rList rA                      =  RType (App "[]" [term rA])
>                                        (rSum (RCon "[]"  rUnit) (RCon "(:)" (rPair rA (rList rA))))
>                                        (EP fromList toList)

>
> fromList                      :: forall aT . [aT] -> Sum UnitT (Pair aT [aT])
> fromList []                   =  Inl Unit
> fromList (a : as)             =  Inr (a :*: as)
>
> toList                        :: forall aT . Sum UnitT (Pair aT [aT]) -> [aT]
> toList (Inl Unit)             =  []
> toList (Inr (a :*: as))       =  a : as

> instance Representable Bool where
>   rep                         =  rBool
> instance (Representable aT) => Representable [aT] where
>   rep                         =  rList rep




Generic minimum.

> rMinBound                     :: forall tT . Rep tT -> tT
> rMinBound (RInt         ep)   =  to ep (minBound)
> rMinBound (RChar        ep)   =  to ep (minBound)
> rMinBound (RDynamic     ep)   =  error "rMinBound: dynamic"
> rMinBound (RArrow rA rB ep)   =  to ep (\ a -> rMinBound rB)
> rMinBound (RUnit        ep)   =  to ep (Unit)
> rMinBound (RSum   rA rB ep)   =  to ep (Inl (rMinBound rA))
> rMinBound (RPair  rA rB ep)   =  to ep (rMinBound rA :*: rMinBound rB)
> rMinBound (RType t rA   ep)   =  to ep (rMinBound rA)
> rMinBound (RCon  s rA)        =  rMinBound rA

Generic memoization.

> rMemo                         :: forall tT vT . Rep tT -> (tT -> vT) -> (tT -> vT)
> rMemo (RInt         ep) f     =  \ t -> f t       -- no memoization
> rMemo (RChar        ep) f     =  \ t -> f t       -- no memoization
> rMemo (RDynamic     ep) f     =  \ t -> f t       -- no memoization
> rMemo (RArrow rA rB ep) f     =  \ t -> f t       -- no memoization
> rMemo (RUnit        ep) f     =  \ t -> case from ep t of
>                                          Unit -> fUnit
>   where fUnit                 =  f (to ep (Unit))
> rMemo (RSum rA rB   ep) f     =  \ t -> case from ep t of
>                                          Inl a -> fInl a
>                                          Inr b -> fInr b
>   where fInl                  =  rMemo rA (\ a -> f (to ep (Inl a)))
>         fInr                  =  rMemo rB (\ b -> f (to ep (Inr b)))
> rMemo (RPair rA rB  ep) f     =  \ t -> case from ep t of
>                                          a :*: b -> fPair a b
>   where fPair                 =  rMemo rA (\a -> rMemo rB (\b -> f (to ep (a :*: b))))
> rMemo (RType e rA   ep) f     =  \ t -> rMemo rA (\ a -> f (to ep a)) (from ep t)
> rMemo (RCon  s rA)      f     =  rMemo rA f

Note that we do \emph{not} memoize primitive types such as |Int|s or
|Char|s (this would require building a look-up table).