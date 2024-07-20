> module DynamicLeibniz
> where
>
> infixr 9 `o`
> f `o` g                       =  \a -> f (g a)


%
Unit type, sum type and pair type (actually generic representation
types).

> data UnitT                    =  Unit
>
> data Sum  aT bT               =  Inl aT | Inr bT
>
> data Pair aT bT               =  aT :*: bT

Standard maps.

> ($+)                          :: forall aT bT cT dT . (aT -> bT) -> (cT -> dT) -> ((Sum aT cT) -> (Sum bT dT))
> (f $+ g) (Inl a)              =  Inl (f a)
> (f $+ g) (Inr b)              =  Inr (g b)
>
> ($*)                          :: forall aT bT cT dT . (aT -> bT) -> (cT -> dT) -> ((Pair aT cT) -> (Pair bT dT))
> (f $* g) (a :*: b)            =  f a :*: g b
>
> ($->)                         :: forall aT bT cT dT . (aT -> bT) -> (cT -> dT) -> ((bT -> cT) -> (aT -> dT))
> (f $-> g) h                   =  g `o` h `o` f

% $
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Type equivalence}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Embedding/projection maps, bidirectional maps.

> data EPT a b                  =  EP { unEP :: forall f . f a -> f b }

> newtype Id a			=  Id { unId :: a }
>
> from				:: forall a b . EPT a b -> (a -> b)
> from ep			=  \ a -> unId (unEP ep (Id a))
>
> newtype Inv a b		=  Inv { unInv :: b -> a }
>
> to				:: forall a b . EPT a b -> (b -> a)
> to ep				=  unInv (unEP ep (Inv id))

Alternatively: |BiMap|, |Bimap|, |BI|.

> self                          :: forall aT . EPT aT aT
> self                          =  EP id

> newtype Flip g a b            = Flip{unFlip :: g b -> g a}
> inv                           :: forall aT bT . (EPT aT bT) -> (EPT bT aT)
> inv f                         =  EP (unFlip (unEP f (Flip id)))


> infixr 9 {-"\;"-} <>
> (<>)                          :: forall aT bT cT . (EPT bT cT) -> (EPT aT bT) -> (EPT aT cT)
> f <> g                        =  EP (unEP f `o` unEP g)

> newtype Op1 fT gT cT dT aT bT = Op1 {unOp1 :: gT(fT aT cT) -> gT(fT bT dT)}
 
> newtype Op2 fT gT aT bT cT dT	= Op2 {unOp2 :: gT (fT aT cT) -> gT(fT bT dT)}

> ($+$)                         :: forall aT bT cT dT . (EPT aT bT) -> (EPT cT dT) -> (EPT (Sum aT cT) (Sum bT dT))
> f $+$ g                       =  EP (unOp1 (unEP f (Op1 id))) <> 
>				   EP (unOp2 (unEP g (Op2 id)))

The above code is equivalent to 

let x1 = (Op1 id) in
let y1 = unOp1 (unEP f x1) in
let e1 :: EPT(Sum aT dT) (Sum bT dT)
       = EP(y1) in
let x2 = (Op2 id) in
let y2 = unOp2 (unEP g x2) in
let e2 :: EPT(Sum aT cT) (Sum aT dT) 
       = EP(y2) in
e1 <> e2

The same thing works for the other operations:



> ($*$)                         :: forall aT bT cT dT . (EPT aT bT) -> (EPT cT dT) -> (EPT (Pair aT cT) (Pair bT dT))
> f $*$ g                       = EP (unOp1 (unEP f (Op1 id))) <> 
>				   EP (unOp2 (unEP g (Op2 id)))
 
>
> ($->$)                        :: forall aT bT cT dT . (EPT aT bT) -> (EPT cT dT) -> (EPT (aT -> cT) (bT -> dT))
> f $->$ g                      =  EP (unOp1 (unEP f (Op1 id))) <> 
>				   EP (unOp2 (unEP g (Op2 id)))


NB. We use the same symbols as for the standard maps.

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
>                               |  forall aT {-"\phantom{\;\beta}"-} . RType Type   (Rep aT) {-"}&{"-} (EPT tT aT)
>                               |                                      RCon  String (Rep tT)
> 				| 				       RVar

Extension for polymorphic types.

>                               |  forall fT . RForall (forall aT . Rep aT -> Rep (fT aT)) (EPT tT (Forall fT))
>
> newtype Forall fT             =  Forall { unForall :: forall aT . fT aT }
> rForall                       :: forall fT . (forall aT . Rep aT -> Rep (fT aT)) -> Rep (Forall fT)
> rForall rF                    =  RForall rF self
>

FIX

 rId                           :: forall aT . Rep aT -> Rep (Id aT)
 rId rA                        =  RArrow rA rA (EP unId Id)
 dynId                         =  Dynamic (rForall rId) (Forall (Id id))

Smart constructors.

> rInt                          :: Rep Int
> rInt                          =  RInt         self
> rChar                         :: Rep Char
> rChar                         =  RChar        self
> rDynamic                      :: Rep Dynamic
> rDynamic                      =  RDynamic     self
> rArrow                        :: forall aT bT . Rep aT -> Rep bT -> Rep (aT -> bT)
> rArrow rA rB                  =  RArrow rA rB self
> rUnit                         :: Rep UnitT
> rUnit                         =  RUnit        self
> rSum                          :: forall aT bT . Rep aT -> Rep bT -> Rep (Sum  aT bT)
> rSum   rA rB                  =  RSum   rA rB self
> rPair                         :: forall aT bT . Rep aT -> Rep bT -> Rep (Pair aT bT)
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
> instance (Representable a,Representable b) => Representable (Pair a b) where
>   rep                         =  rPair rep rep 
> instance (Representable a,Representable b) => Representable (Sum a b) where
>   rep                         =  rSum rep rep




% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Dynamics}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Dynamic                  =  forall aT . Dynamic (Rep aT) aT
>
> dynamic                       :: forall aT . (Representable aT) => aT -> Dynamic
> dynamic x                     =  Dynamic rep x
>
> unify                         :: forall aT bT . Rep aT -> Rep bT -> [EPT aT bT]
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

Type-safe cast.

> rCast                         :: forall tT . Rep tT -> Dynamic -> tT
> rCast rT (Dynamic rA x)       =  case unify rT rA of
>                                    [ep] -> to ep x
>                                    _    -> error "cast: type mismatch"
>
> cast                          :: (Representable tT) => Dynamic -> tT
> cast d                        =  rCast rep d
>
> apply                         :: Dynamic -> Dynamic -> Dynamic
> apply (Dynamic (RArrow rA rB ep) f) (Dynamic rA' x)
>                               =  case unify rA' rA of
>                                    [ep'] -> Dynamic rB ((from ep f) (from ep' x))
>                                    _     -> error "apply: type mismatch"
> apply _ _                     =  error "apply: not a function"

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Generics}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Type                     =  App String [Type]
>                                  deriving (Show, Eq, Ord)

> typeOf                        :: forall tT . Rep tT -> Type
> typeOf (RInt     ep)          =  App "Int" []
> typeOf (RChar    ep)          =  App "Char" []
> typeOf (RDynamic ep)          =  App "Dynamic" []
> typeOf (RArrow rA rB ep)      =  App "->" [typeOf rA, typeOf rB]
> typeOf (RType t rA ep)        =  t
> typeOf _                      =  error "typeOf: unexpected argument"
>
> typeOf'                       :: (forall aT . Rep aT -> Rep (fT aT)) -> Type
> typeOf' rF                    =  case typeOf (rF undefined) of App t ts -> App t (init ts)

Examples for \ldots

FIX

 rBool                         :: Rep Bool
 rBool                         =  RType (App "Bool" [])
                                        (rSum (RCon "False" rUnit) (RCon "True" rUnit))
                                        (EP id)

 fromBool                      :: Bool -> Sum UnitT UnitT
 fromBool False                =  Inl Unit
 fromBool True                 =  Inr Unit

 toBool                        :: Sum UnitT UnitT -> Bool
 toBool (Inl Unit)             =  False
 toBool (Inr Unit)             =  True

FIX

 rList                         :: forall tT . Rep tT -> Rep [tT]
 rList rA                      =  RType (App "[]" [typeOf rA])
                                        (rSum (RCon "Nil"  rUnit) 
					      (RCon "Cons" (rPair rA (rList rA))))
                                        (EP fromList toList)

 fromList                      :: forall aT . [aT] -> Sum UnitT (Pair aT [aT])
 fromList []                   =  Inl Unit
 fromList (a : as)             =  Inr (a :*: as)

 toList                        :: forall aT . Sum UnitT (Pair aT [aT]) -> [aT]
 toList (Inl Unit)             =  []
 toList (Inr (a :*: as))       =  a : as

FIX
 instance Representable Bool where
   rep                         =  rBool

 instance (Representable aT) => Representable [aT] where
   rep                         =  rList rep

Generic functions: generic equality.


> rEqual                        :: forall tT . Rep tT -> tT -> tT -> Bool
> rEqual (RInt ep) t1 t2        =  from ep t1 == from ep t2
> rEqual (RChar ep) t1 t2       =  from ep t1 == from ep t2
> rEqual (RDynamic ep) d1 d2    =  case (from ep d1, from ep d2) of
>                                    (Dynamic t1 v1, Dynamic t2 v2)
>                                      -> case unify t1 t2 of
>                                           [ep'] -> rEqual t1 v1 (to ep' v2)
>                                           _     -> False
> rEqual (RArrow rA rB ep) t1 t2=  error "rEqual: equality of functions"
> rEqual (RUnit ep) t1 t2       =  True
> rEqual (RSum rA rB ep) t1 t2  =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> rEqual rA a1 a2
>                                    (Inl a1, Inr b2) -> False
>                                    (Inr b1, Inl a2) -> False
>                                    (Inr b1, Inr b2) -> rEqual rB b1 b2
> rEqual (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) -> rEqual rA a1 a2 && rEqual rB b1 b2
> rEqual (RType e rA  ep) t1 t2 =  rEqual rA (from ep t1) (from ep t2)
> rEqual (RCon s rA) t1 t2      =  rEqual rA t1 t2


> cEqual                        :: forall tT . (Representable tT) => tT -> tT -> Bool
> cEqual t1 t2                  =  rEqual rep t1 t2

> class Eq2 aT where
>   (===) :: aT -> aT -> Bool

> instance (Representable aT) => Eq2 aT where
>   t1 === t2 = cEqual t1 t2


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
>                                    (Inl a1, Inl a2) -> rEqual rA a1 a2
>                                    (Inl a1, Inr b2) -> False
>                                    (Inr b1, Inl a2) -> False
>                                    (Inr b1, Inr b2) -> rEqual rB b1 b2
>
> #-}
> {-# RULES
>   "rEqualSum"			forall (rA :: Rep a) (rB :: Rep b) ep t1 t2 .
>				rEqual (RPair rA rB ep) t1 t2 =  
>				  case (from ep t1, from ep t2) of
>                                   (a1 :*: b1, a2 :*: b2) -> 
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



> y :: Int
> y = 5
> z :: Int
> z = 6

> r :: Rep (Pair Int Int)
> r = RPair (RInt self) (RInt self) self

> x :: (Pair Int Int) -> (Pair Int Int) -> Bool
> x = rEqual rep 

> do_n 0 f x = id 
> do_n n f x = shows (f x) . do_n (n-1) f x

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

Generic comparison.

> rComp                        :: forall tT . Rep tT -> tT -> tT -> Ordering
> rComp (RInt ep) t1 t2        =  compare(from ep t1)(from ep t2)
> rComp (RChar ep) t1 t2       =  compare(from ep t1)(from ep t2)
> rComp (RDynamic ep) d1 d2    =  case (from ep d1, from ep d2) of
>                                    (Dynamic t1 v1, Dynamic t2 v2)
>                                      -> case unify t1 t2 of
>	 			           [ep'] -> rComp t1 v1 (to ep' v2);
>                                           _     -> compare (typeOf t1) (typeOf t2)
> rComp (RArrow rA rB ep) t1 t2=  error "rComp: comparison of functions"
> rComp (RUnit ep) t1 t2       =  EQ
> rComp (RSum rA rB ep) t1 t2  =  case (from ep t1, from ep t2) of
>                                    (Inl a1, Inl a2) -> rComp rA a1 a2
>                                    (Inl a1, Inr b2) -> LT
>                                    (Inr b1, Inl a2) -> GT
>                                    (Inr b1, Inr b2) -> rComp rB b1 b2
> rComp (RPair rA rB ep) t1 t2 =  case (from ep t1, from ep t2) of
>                                    (a1 :*: b1, a2 :*: b2) -> 
>	                                case rComp rA a1 a2 of 
>	                                  EQ ->  rComp rB b1 b2;
>	                                  x -> x
> rComp (RType e rA  ep) t1 t2 =  rComp rA (from ep t1) (from ep t2)
> rComp (RCon s rA) t1 t2      =  rComp rA t1 t2


Generic unparsing.

> rShows                        :: forall tT . Rep tT -> tT -> ShowS
> rShows (RInt         ep) t    =  shows (from ep t)
> rShows (RChar        ep) t    =  shows (from ep t)
> rShows (RDynamic     ep) t    =  case from ep t of
>                                    Dynamic rA x -> showChar '(' `o` showString "dynamic "
>                                                    `o` rShows rA x `o` showChar ')'
> rShows (RArrow rA rB ep) t    =  showString "<function>"
> rShows (RUnit        ep) t    =  id
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
>                                           Unit -> fUnit
>   where fUnit                 =  f (to ep (Unit))
> rMemo (RSum rA rB   ep) f     =  \ t -> case from ep t of
>                                           Inl a -> fInl a
>                                           Inr b -> fInr b
>   where fInl                  =  rMemo rA (\ a -> f (to ep (Inl a)))
>         fInr                  =  rMemo rB (\ b -> f (to ep (Inr b)))
> rMemo (RPair rA rB  ep) f     =  \ t -> case from ep t of
>                                           a :*: b -> fPair a b
>   where fPair                 =  rMemo rA (\a -> rMemo rB (\b -> f (to ep (a :*: b))))
> rMemo (RType e rA   ep) f     =  \ t -> rMemo rA (\ a -> f (to ep a)) (from ep t)
> rMemo (RCon  s rA)      f     =  rMemo rA f

\end{document}


> data TypeRep                  =  forall tT . TypeRep (Rep tT)

> typeRep                       :: String -> TypeRep
> typeRep "rInt"                =  TypeRep rInt

FIX

typeRep "rBool"               =  TypeRep rBool

\end{document}
