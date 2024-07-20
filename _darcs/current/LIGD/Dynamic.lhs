> module Dynamic
> where
>
> infixr 9 `o`
> f `o` g                       =  \a -> f (g a)



> data UnitT                    =  Unit
>
> data Sum  aT bT               =  Inl aT | Inr bT
>
> data Pair aT bT               =  aT :*: bT

Standard mapping functions on the above types (and on the function
type).

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
>                               |                                      ROverride VTable (Rep tT)


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

> unify                         :: forall t1T t2T . Rep t1T -> Rep t2T -> [EPT t1T t2T]
> unify (RInt ep1) (RInt ep2)   
>                               =  [ inv ep2 <> ep1 ]
> unify (RChar ep1) (RChar ep2)   
>                               =  [ inv ep2 <> ep1 ]
> unify (RDynamic ep1) (RDynamic ep2)
>                               =  [ inv ep2 <> ep1 ]
> unify (RArrow rA1 rB1 ep1) (RArrow rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $->$ epB) <> ep1 | epA <- unify rA1 rA2 , epB <- unify rB1 rB2 ]
> unify (RUnit ep1) (RUnit ep2) 
>                               =  [ inv ep2 <> ep1 ]
> unify (RSum rA1 rB1 ep1) (RSum rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $+$ epB) <> ep1 | epA <- unify rA1 rA2, epB <- unify rB1 rB2 ]
> unify (RPair rA1 rB1 ep1) (RPair rA2 rB2 ep2)
>                               =  [ inv ep2 <> (epA $*$ epB) <> ep1 | epA <- unify rA1 rA2, epB <- unify rB1 rB2 ]
> unify (RCon s1 rA1) (RCon s2 rA2)
>                               =  unify rA1 rA2
> unify (RType t1 rA1 ep1) (RType t2 rA2 ep2)
>                               =  [ inv ep2 <> head (unify rA1 rA2) <> ep1 | t1 == t2 ]
> unify _ _                     =  []

Type-safe cast and dynamic function application.

> rCast                         :: forall tT . Rep tT -> Dynamic -> tT
> rCast rT (Dyn rA a)           =  case unify rT rA of
>                                    [ep] -> to ep a
>                                    _    -> error "cast: type mismatch"
>
> cast                          :: (Representable tT) => Dynamic -> tT
> cast d                        =  rCast rep d
>
> apply                         :: Dynamic -> Dynamic -> Dynamic
> apply (Dyn (RArrow rA rB ep)f) (Dyn rA' x)
>                               =  case unify rA rA' of
>                                    [ep'] -> Dyn rB ((from ep f) (to ep' x))
>                                    _     -> error "apply: type mismatch"
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
\subsection{VTables}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type VTable = String -> [Dynamic]

A function that takes a name and a type representation and 
tries to find an implementation in a vtable, if one exists:

> vlookup :: VTable -> String -> Rep a -> [a]
> vlookup vtbl nm rA = [from ep f | (Dyn rf f) <- vtbl nm, ep <- unify rf rA]



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

Generic functions: generic equality.

> rEqual                        :: forall tT . Rep tT -> tT -> tT -> Bool
> rEqual (RInt ep) t1 t2        =  from ep t1 == from ep t2
> rEqual (RChar ep) t1 t2       =  from ep t1 == from ep t2
> rEqual (RDynamic ep) d1 d2    =  case (from ep d1, from ep d2) of
>                                    (Dyn rA1 v1, Dyn rA2 v2)
>                                      -> case unify rA1 rA2 of
>                                           [ep'] -> rEqual rA1 v1 (to ep' v2)
>                                           _     -> False
> rEqual (RArrow rA rB ep) t1 t2=  error "rEqual: equality of functions"
> rEqual (RUnit ep) t1 t2       =  case (from ep t1, from ep t2) of
>                                    (Unit, Unit) -> True
> rEqual (RSum rA rB ep) t1 t2  =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> rEqual rA a1 a2
>                                    (Inl a1, Inr b2) -> False
>                                    (Inr b1, Inl a2) -> False
>                                    (Inr b1, Inr b2) -> rEqual rB b1 b2
> rEqual (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) -> rEqual rA a1 a2
>                                                              && rEqual rB b1 b2
> rEqual (RType e rA  ep) t1 t2 =  rEqual rA (from ep t1) (from ep t2)
> rEqual (RCon s rA) t1 t2      =  rEqual rA t1 t2
> rEqual (ROverride vtbl rA) x y 
>				= (case (vlookup vtbl "rEqual" (fEqual rA)) of
>                                   f:fs -> f x y;        
>                                   [] -> rEqual rA x y  ) 
>   where fEqual rep 		= rArrow rep (rArrow rep rBool)

> {-# RULES
>   "rEqualInt"			forall (r :: Rep Int) t1 t2 .
>				rEqual r t1 t2 = t1 == t2
> #-}
> {-# RULES
>   "rEqualChar"		forall (r :: Rep Char) t1 t2 .
>				rEqual r t1 t2 = t1 == t2
>
> #-}
> {-# RULES
>   "rEqualArrow"		forall (r :: Rep (a -> b)) t1 t2 .
>				rEqual r t1 t2 = error "rEqual: equality of functions"
>
> #-}
> {-# RULES
>   "rEqualSum"			forall (rA :: Rep a) (rB :: Rep b) e t1 t2 .
>				rEqual (RSum rA rB e) t1 t2 = case (t1, t2) of
>                                    (Left a1, Left a2) -> rEqual rA a1 a2
>                                    (Left a1, Right b2) -> False
>                                    (Right b1, Left a2) -> False
>                                    (Right b1, Right b2) -> rEqual rB b1 b2
>
> #-}
> {-# RULES
>   "rEqualSum"			forall (rA :: Rep a) (rB :: Rep b) ep t1 t2 .
>				rEqual (RPair rA rB ep) t1 t2 =  
>				  case (from ep t1, from ep t2) of
>                                   ((a1 :*: b1), (a2 :*: b2)) -> 
>				      rEqual rA a1 a2 && rEqual rB b1 b2
> #-}
> {-# RULES
>   "rEqualType"		forall e rA ep t1 t2 .
> 				rEqual (RType e rA  ep) t1 t2 =  
>				rEqual rA (from ep t1) (from ep t2)
> #-}
> {-# RULES
>   "rEqualCon"			forall s rA t1 t2 .
> 				rEqual (RCon s rA) t1 t2      =  
>				rEqual rA t1 t2
> #-}
> 
> {-# RULES
>   "rEqualOverride"		forall (vtbl::VTable) (rA::Rep a) (t1::a) (t2::a) .
> 				rEqual (ROverride vtbl rA) t1 t2
>				= case rArrow rA (rArrow rA rBool) of
>				    frep -> 
>				      (case (vlookup vtbl "rEqual" frep) of
>                                       f:fs -> f t1 t2;        
>                                       [] -> rEqual rA t1 t2  )
> #-}
> 

> lEqual :: (a -> a -> Bool) -> ([a] -> [a] -> Bool)  
> lEqual aEqual [] [] = True
> lEqual aEqual [] x  = lEqual aEqual x []
> lEqual aEqual (x:xs) [] = False
> lEqual aEqual (x:xs) (y:ys) = aEqual x y && lEqual aEqual xs ys

function to extract parameter type of an rList type (don't need a value...)

listParamTypeOf :: Rep [a] -> (forall b. Rep b)
listParamTypeOf (RType ty (RSum _ (RCon "Cons" (RPair rA _ _)) _) _) = rA

special case rule for List:




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

Generic unparsing.

> rShows                        :: forall tT . Rep tT -> tT -> ShowS
> rShows (RInt         ep) t    =  shows (from ep t)
> rShows (RChar        ep) t    =  shows (from ep t)
> rShows (RDynamic     ep) t    =  case from ep t of
>                                    Dyn rA x -> showChar '(' `o` showString "dynamic "
>                                                `o` rShows rA x `o` showChar ')'
> rShows (RArrow rA rB ep) t    =  showString "<function>"
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


Testing overriding for ==:

> eqmod :: Int -> Int -> Int -> Bool -- eq k m n := m = n mod k
> eqmod k m n = mod m k == mod n k
> rIntmodk k = ROverride (\x -> if x == "rEqual" then [dynamic (eqmod k)] else []) rInt
> rInt2 = ROverride (\x -> if x == "rEqual" then [dynamic (eqmod 2)] else []) rInt
> eq2 :: Int -> Int -> Bool
> eq2 i j = rEqual rInt2 i j 
> eqk k i j = rEqual (rIntmodk k) i j
> 
> do_n 0 f x = id 
> do_n n f x = shows (f x) . do_n (n-1) f x
