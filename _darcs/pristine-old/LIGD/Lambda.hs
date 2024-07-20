-- Lambda terms using a de Bruijn representation.
-- LT g a is the type of lambda terms of type a in context g.
-- - where contexts g are represented by a list of types encoded using
-- pair and unit.  
-- For example, the de Bruijn term 
--  Lam (Lam (App (Var z) (Var (s z))) refl) refl
-- has type
-- forall a g a1. LT g (a1 -> (a1 -> a) -> a)
-- I think this is from Bird and Meertens.

module Lambda
where


data EPT a b = EP{to :: a -> b, from :: b -> a}
refl :: EPT a a
refl = EP id id

data V g a  = forall h  . Hyp(EPT g (h, a))
            | forall h b. Weak(V h a) (EPT g (h,b))

data LT g a =             Var (V g a)
	    | forall b c. Lam(LT (g,b) c)    (EPT a (b -> c))
	    | forall b  . App(LT g (b -> a)) (LT g b)
	    |             IntE(Int)	     (EPT a Int)

		
z :: V (g,a) a
z = Hyp refl

s :: V g a -> V (g,b) a
s v = Weak v refl





id' :: LT g (a -> a)
id' = Lam (Var z) refl



fv :: LT g Int
fv = IntE(5) refl

app' :: LT g Int
app' = App id' fv



countVar :: V g a -> Int
countVar (Hyp _) = 1
countVar (Weak v _ ) = (countVar v) + 1

showVar :: Int -> V g a -> ShowS
showVar d v = showString "X" . shows (d - countVar v)

lShows' :: Int -> LT g a -> ShowS
lShows' d  (Var v) = showVar d v
lShows' d (IntE i _) = shows i
lShows' d (Lam t e) =  showString "(\\x" . shows d . showString " -> ". (lShows' (d+1) t) . showString ")" 
lShows' d (App e1 e2) = lShows' d e1 . showString " ". lShows' d  e2 



lShows ::  LT g a -> ShowS
lShows t = lShows' 0 t
           
		
