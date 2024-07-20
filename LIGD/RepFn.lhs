Representations for type functions (e.g., here, mapping all Ints to 
pairs of Ints).

> module RepFn
> where
>
> infixr 9 `o`
> f `o` g                       =  \a -> f (g a)

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

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Type representations}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Rep aT 			= RInt (EPT Int aT)
>				| forall bT cT . RPair (Rep bT) (Rep cT) (EPT (bT,cT) aT)
>				| forall bT cT . RArrow (Rep bT) (Rep cT) (EPT (bT -> cT) aT)

Smart constructors

> rInt :: Rep Int
> rInt = RInt self
> rPair :: Rep aT -> Rep bT -> Rep (aT,bT)
> rPair rA rB = RPair rA rB self
> rArrow :: Rep aT -> Rep bT -> Rep (aT -> bT)
> rArrow rA rB = RArrow rA rB self
 

> data FRep a 			= FInt (EPT (Int , Int) a)
> 				| forall b c. FPair (FRep b) (FRep c) (EPT (b , c) a)
> 				| forall b c. FArrow (FRep b) (FRep c) (EPT (b -> c) a)
>
> 

> fInt :: FRep (Int,Int)
> fInt = FInt self
> fPair :: FRep aT -> FRep bT -> FRep (aT,bT)
> fPair rA rB = FPair rA rB self
> fArrow :: FRep aT -> FRep bT -> FRep (aT -> bT)
> fArrow rA rB = FArrow rA rB self


> coerceFRep :: FRep a -> Rep a
> coerceFRep (FInt ep) = RPair rInt rInt ep
> coerceFRep (FPair rA rB ep) = RPair (coerceFRep rA) (coerceFRep rB) ep
> coerceFRep (FArrow rA rB ep) = RArrow (coerceFRep rA) (coerceFRep rB) ep

> data Abstract		 	= forall b. Abs (Rep b)
> data AbstractF 		= forall b. FAbs (FRep b)

> repF :: Rep a -> AbstractF
> repF (RInt ep) 		= FAbs fInt
> repF (RPair rA rB ep) 	= case ((repF rA), (repF rB))
>				  of (FAbs(fA), FAbs(fB)) -> 
> 					FAbs (fPair fA fB)
> repF (RArrow rA rB ep) 	= case ((repF rA), (repF rB))
>				  of (FAbs(fA), FAbs(fB)) -> 
> 						FAbs (fArrow fA fB)
> absF :: Abstract -> AbstractF
> absF (Abs rA) 		= repF rA

> data Dynamic		 	= forall b. Dyn (Rep b) b
> data DynamicF 		= forall b. FDyn (FRep b) b

These are like Leroy's S (specialize) and G (generalize) functions.
This would seem  more useful if F had something like boxed vs. unboxed types.

> coerceFrom 				:: FRep a -> Rep b -> a -> b
> coerceFrom (FInt ep) (RInt ep') x	= let (y,_) = to ep x in from ep' y
> coerceFrom (FPair fA fB ep) (RPair rA rB ep') x = 
> 					let (y,z) = to ep x in 
>				  	let y' = coerceFrom fA rA y in
>					let z' = coerceFrom fB rB z in
>					from ep' (y',z')
> coerceFrom (FArrow fA fB ep) (RArrow rA rB ep') x = 
>					let f = to ep x in
>					from ep' (\x -> coerceFrom fB rB (f (coerceTo rA fA x)))

> coerceTo 				:: Rep a -> FRep b -> a -> b
> coerceTo (RInt ep) (FInt ep') x	= let y = to ep x in from ep' (y,y)
> coerceTo (RPair rA rB ep) (FPair fA fB ep') x = 
> 					let (y,z) = to ep x in 
>				  	let y' = coerceTo rA fA y in
>					let z' = coerceTo rB fB z in
>					from ep' (y',z')
> coerceTo (RArrow rA rB ep) (FArrow fA fB ep') x = 
>					let f = to ep x in
>					from ep' (\x -> coerceTo rB fB (f (coerceFrom fA rA x)))

> makeF 				:: Rep a -> a -> DynamicF
> makeF rA x 				= case repF rA
>					  of FAbs fA -> 
>					      FDyn fA (coerceTo rA fA x)

The function case is the hard case.  You have to create a new function that takes an F a, converts it to an a, passes it to f which returns a b, then convert the b back to an F b.  This function then needs to be packed with representations of the argument types, fA and fB.
