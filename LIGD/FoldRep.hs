module FoldRep
where

type Id a = a



data EPT a b = EP{to :: a -> b, from :: b -> a}
refl :: EPT a a
refl = EP id id

newtype Forall f = Forall { fromForall :: forall b . f b}

data Rep f a = RInt (EPT (f a) Int)
		 | RVar (EPT (f a) a)
		 | RBVar
		 | forall g h . RProd (Rep g a) (Rep h a) 
				(EPT (f a) (g a,h a))
		 | forall g h . RSum (Rep g a) (Rep h a) 
				(EPT (f a) (Either (g a) (h a)))
		 | forall g h . RArrow (Rep g a) (Rep h a) 
				(EPT (f a) (g a -> h a))


app' :: forall f a . (Rep f a) -> (a -> ()) -> (f a -> ())
app' (RInt ep) f x = ()
app' (RVar ep) (f::(a -> ())) (x::(f a)) = f (to ep x)
app' RBVar f x = ()
app' (RProd rB rC ep) f x = 
  let (x1 ,x2) = to ep x in
  let () = app' rB f x1 in
  app' rC f x2
app' (RSum rB rC ep) f x = 
  case (to ep x) of
    Left x1 -> app' rB f x1;
    Right x2 -> app' rC f x2
app' (RArrow rB rC ep) f x = () -- app' doesn't go under functions

fold' :: forall f a b . (Rep f a) -> (b -> a -> b) -> (b -> f a -> b)
fold' (RInt ep) f v x = v
fold' (RVar ep) f v x = f v (to ep x)
fold' RBVar f v x = v
fold' (RProd rB rC ep) f v x = 
  let (x1 ,x2) = to ep x in
  let v' = fold' rB f v x1 in
  fold' rC f v' x2
fold' (RSum rB rC ep) f v x = 
  case (to ep x) of
    Left x1 -> fold' rB f v x1;
    Right x2 -> fold' rC f v x2
fold' (RArrow rB rC ep) f v x = v -- ignore function pointers...



-- Idea: Add enough stuff to this that you can go "map' rList" and get list
-- map, etc.
-- Also, consider adding symbol tables to some cases of Rep and allowing
-- generic functions to look up overriding definitions of themselves.
-- So, if you want to have lists print out bracketed, you would 
-- replace the plain RCon "[]" constructor with an ROverride constructor
-- that contains a dictionary of string, Dynamic pairs.  Then rShow
-- would have a case for ROverride that looks up the name "rShow" in
-- the provided symbol table, and dynamically casts the result to 
-- the required type.  If the cast fails, then rShow could either stop with
-- an error or continue by falling through to the non-overrided 
-- definition (override could have a Bool flag that has a hint for this)
-- Note that it is up to rShow to handle the overriding behavior correctly and
-- up to the type representation's provider to provide a good 
-- implementation of rShow.
-- Does this give us open recursion?  That is, can we combine overridings that 
-- don't know about each other dynamically?  Similarly, does this give us
-- extensible sums for free?  

-- I think it does!  In fact the types of the extensible sums/dictionaries 
-- needn't even be 
-- uniform or unique (the symbol table could have multiple entries).

-- Cool!
