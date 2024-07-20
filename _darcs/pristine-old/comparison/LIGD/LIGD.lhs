> {-# OPTIONS -fglasgow-exts #-}
> module LIGD where

> infixr 9 `o`
> f `o` g                       =  \a -> f (g a)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Generic representation types}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> infixr 8 :*:
> infixr 8 :+:

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
>                               |  {-"\phantom{\forall \alpha \;\beta \,.\,}"-} RFloat   {-"}&{"-} (EPT tT Float)
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
> rFloat                        :: Rep Float
> rFloat                        =  RFloat       self

Representation for generic functions of arity 2, with one non-generic variable

> data Rep2 fT aT bT cT         =  RInt2    (EPT aT Int) (EPT bT Int)
>                               |  RFloat2  (EPT aT Float) (EPT bT Float)
>                               |  RChar2   (EPT aT Char) (EPT bT Char)
>                               |  RDynamic2 (EPT aT Dynamic) (EPT bT Dynamic)
>                               |  forall aT1 aT2 bT1 bT2 .
>                                  RArrow2 (Rep2 fT aT1 bT1 cT) (Rep2 fT aT2 bT2 cT)
>                                          (EPT aT (aT1 -> aT2)) (EPT bT (bT1 -> bT2))
>                               |  RUnit2   (EPT aT UnitT) (EPT bT UnitT)
>                               |  forall aT1 aT2 bT1 bT2 .
>                                  RSum2 (Rep2 fT aT1 bT1 cT) (Rep2 fT aT2 bT2 cT)
>                                        (EPT aT (Sum aT1 aT2)) (EPT bT (Sum bT1 bT2))
>                               |  forall aT1 aT2 bT1 bT2 .
>                                  RPair2 (Rep2 fT aT1 bT1 cT) (Rep2 fT aT2 bT2 cT)
>                                         (EPT aT (Pair aT1 aT2)) (EPT bT (Pair bT1 bT2))
>                               |  forall aT' bT' .
>                                  RType2 Term   (Rep2 fT aT' bT' cT) (EPT aT aT') (EPT bT bT')
>                               |  RCon2  String (Rep2 fT aT bT cT)
>                               |  RVar2  (fT aT bT cT)

> rInt2                          :: Rep2 fT Int Int cT
> rInt2                          =  RInt2         self self

> rChar2                         :: Rep2 fT Char Char cT
> rChar2                         =  RChar2        self self

> rDynamic2                      :: Rep2 fT Dynamic Dynamic cT
> rDynamic2                      =  RDynamic2     self self

> rArrow2                        :: forall fT aT bT cT .
>                                   Rep2 fT aT bT cT ->
>                                   (forall aT' bT' .
>                                    Rep2 fT aT' bT' cT ->
>                                    Rep2 fT (aT -> aT') (bT -> bT') cT
>                                   )
> rArrow2 rA rB                  =  RArrow2 rA rB self self

> rUnit2                         :: Rep2 fT UnitT UnitT cT
> rUnit2                         =  RUnit2        self self

> rSum2                          :: forall fT aT bT cT .
>                                   Rep2 fT aT bT cT ->
>                                   (forall aT' bT' .
>                                    Rep2 fT aT' bT' cT ->
>                                    Rep2 fT (Sum aT aT') (Sum bT bT') cT
>                                   )
> rSum2   rA rB                  =  RSum2   rA rB self self

> rPair2                         :: forall fT aT bT cT .
>                                   Rep2 fT aT bT cT ->
>                                   (forall aT' bT' .
>                                    Rep2 fT aT' bT' cT ->
>                                    Rep2 fT (Pair aT aT') (Pair bT bT') cT
>                                   )
> rPair2  rA rB                  =  RPair2  rA rB self self

> rFloat2                        :: Rep2 fT  Float Float cT
> rFloat2                        =  RFloat2   self    self



A class for representable types.

> class Representable tT where
>   rep                         :: Rep tT
>
> instance Representable Int where
>   rep                         =  rInt
> instance Representable Char where
>   rep                         =  rChar
> instance Representable Float where
>   rep                         =  rFloat
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

> term2                          :: forall fT aT bT cT . Rep2 fT aT bT cT -> Term
> term2 (RInt2         ep _)        =  App "Int" []
> term2 (RChar2        ep _)        =  App "Char" []
> term2 (RDynamic2     ep _)        =  App "Dynamic" []
> term2 (RArrow2 rA rB ep _)        =  App "->" [term2 rA, term2 rB]
> term2 (RUnit2        ep _)        =  App "1" []
> term2 (RSum2   rA rB ep _)        =  App "(+)" [term2 rA, term2 rB]
> term2 (RPair2  rA rB ep _)        =  App "(*)" [term2 rA, term2 rB]
> term2 (RType2 t rA   ep _)        =  t
>


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

> rList2                         :: forall aT bT cT fT . Rep2 fT aT bT cT -> Rep2 fT [aT] [bT] cT
> rList2 rA                      =  RType2 (App "[]" [term2 rA])
>                                          (rSum2 (RCon2 "[]"  rUnit2) (RCon2 "(:)" (rPair2 rA (rList2 rA))))
>                                          (EP fromList toList) (EP fromList toList)

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
