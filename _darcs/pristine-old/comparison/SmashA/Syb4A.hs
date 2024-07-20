{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
-- The overlapping instances are needed exclusively for the implementation
-- of TypeEq.

-- Smash your boilerplate IVA, without class and without Typeable
-- and with extensible traversal strategies

-- This file defines the SYB4A core and several typical traversal
-- strategies: 
--   - reconstruction, used for generalized gmap;
--   - reduction, aka generalized gfold, used for computing the size or 
--     the depth of a term, for collecting in a list all values of a specified
--     type occurring in a term or for showing the term; and 
--   - two-terms-in-a-lockstep reduction (used for generic equality). 
-- The user may add more classes of traversal at any time 
-- (see Syb4ABuild.hs for an example).
-- We give some examples, e.g., replacing all Ints in a term 
-- with Floats. The type of the resulting term is the function of the type
-- of the input term and the gmapping function; that result type is 
-- _computed_ and need not be specified.
-- See other files in this directory for more examples.

module SmashA.Syb4A (
	     -- core
	     LDat(..),
	     STApply(..),
	     HNil(..),
	     HCons(..),
	     -- general generic applicator
	     GAPP(..),
	     -- sample traversal strategies
	     TL_recon(..),
	     TL_red(..),
	     TL_red_ctr(..),

	     TL_red_lockstep(..),
             Couple(..),
             eq_clauses,
             geq

	    ) where

-- ============= 
-- The library core: Applying exceptional rules
--
--	stapply :: spec -> a -> d -> w
-- Given a heterogeneous list |spec| of functions |ai->wi| (where 
-- |ai| and |wi| vary from one function to another in the list) 
-- and a datum of type |a|, check to see if there is a function in that
-- list whose argument type |ai| is equal to |a|. If so, apply that 
-- function to the datum, and return its result of the type |wi|. 
-- If no such function is found, return the supplied default value, 
-- of the type |d|. Thus the result type |w| of stapply is a function
-- of the types |spec|, |a|, and |d|.
-- The core procedure above is essentially overloading resolution,
-- but stated without the use of typeclasses.

class STApply spec a d w | spec a d -> w where
    stapply :: spec -> a -> d -> w

instance STApply HNil a d d where
    stapply _ a deflt  = deflt

instance (TypeEq a a' bf, STApply' bf (HCons (a'->w') r) a d w)
    => STApply (HCons (a'->w') r) a d w where
    stapply = stapply' (undefined::bf)

class STApply' bf spec a d w | bf spec a d -> w where
    stapply' :: bf -> spec -> a -> d -> w

instance STApply' HTrue (HCons (a->w) r) a d w where
    stapply' _ (p :+: _) x _ = p x

instance STApply r a d w => STApply' HFalse (HCons p' r) a d w where
    stapply' _ (_ :+: r) x deflt = stapply r x deflt

{-
instance (TypeEq2 a a' bf, SApply'' bf (SCons2 a' l r) a)
    => SApply (SCons2 a' l r) a where
    sapply = sapply'' (undefined::bf)

class SApply'' bf spec a where
    sapply'' :: bf -> spec w -> a -> w -> w

instance ApplyW l a => SApply'' HTrue (SCons2 a l r) a where
    sapply'' _ (SCons2 _ p _) x _ = applyW p x

instance SApply r a => SApply'' HFalse (SCons2 a' l r) a where
    sapply'' _ (SCons2 _ _ r) x deflt = sapply r x deflt
-}


-- ============= 
-- This part of the code is the SYB library
-- It is independent of any generic functions. The instances therein
-- depend on the data types only (and, can hopefully, can be automatically
-- derived for new data types)

-- First, we define the function |gapp| that applies a generic function
-- to a term. A generic function is (quite literally) made of two parts.
-- First, there is a term traversal strategy, identified by a _label_
-- |tlab|. One strategy may be to `reduce' a term using a supplied
-- reducing function (cf. fold over a tree). Another strategy may be to
-- rebuild a term. The second component of a generic function is |spec|,
-- the list of `exceptions'. Each component of |spec| is a function that
-- tells how to transform a term of a specific type. Exceptions override
-- the generic traversal.

-- The function |gapp| is defined generically. The code below says:
-- first, check to see if any of the exceptions apply. If not, do the
-- generic traversal.
class (STApply spec a df w, LDat tlab spec a df)
    => GAPP tlab spec a df w | tlab spec a -> df w where
    gapp :: tlab -> spec -> a -> w

instance (STApply spec a df w, LDat tlab spec a df)
    => GAPP tlab spec a df w where
    gapp tlab spec x = stapply spec x (gin tlab spec x)

-- The function |gin| does a generic traversal. It (may) invoke |gapp|
-- on the children of the term, if any (and if the traversal strategy
-- calls for traversing the children).
-- This class is similar to the class Data of SYB.
-- However, different traversal strategies of Data are methods of this class.
-- Here, different strategies are identified by the label |tlab|
-- and so the set of strategies is _extensible_.
class LDat tlab spec x w | tlab spec x -> w where
    gin :: tlab -> spec -> x -> w


-- We define labels for some popular traversal strategies

		-- Reconstruct the term: used in generic mapping
data TL_recon = TL_recon

		-- Reduce the term: used in folding over a term
newtype TL_red w = TL_red ([w]->w)

		-- The same as above but the reducer accepts the name
		-- of the term's constructor, a string
		-- This is used for generic show
newtype TL_red_ctr w = TL_red_ctr (String -> [w]->w)


-- ============= 
-- In this part of the code, we define the instances of LDat for some
-- popular data types. The user may define more instances for their
-- own data types.

-- Primitive, atomic data types

instance LDat TL_recon spec Int Int where
    gin _ spec x = x

instance LDat (TL_red w) spec Int w where
    gin (TL_red f) spec x = f []

instance LDat (TL_red_ctr w) spec Int w where
    gin (TL_red_ctr f) spec x = f "Int" []


instance LDat TL_recon spec Char Char where
    gin _ spec x = x
instance LDat (TL_red w) spec Char w where
    gin (TL_red f) spec x = f []
instance LDat (TL_red_ctr w) spec Char w where
    gin (TL_red_ctr f) spec x = f "Char" []


instance LDat TL_recon spec Bool Bool where
    gin _ spec x = x
instance LDat (TL_red w) spec Bool w where
    gin (TL_red f) spec x = f []
instance LDat (TL_red_ctr w) spec Bool w where
    gin (TL_red_ctr f) spec x = f "Bool" []


-- product
instance (GAPP TL_recon spec a dfa wa, GAPP TL_recon spec b dfb wb)
    => LDat TL_recon spec (a,b) (wa,wb) where
    gin tlab spec (x,y) = (gapp tlab spec x, gapp tlab spec y)

instance (GAPP (TL_red w) spec a w w, GAPP (TL_red w) spec b w w)
    => LDat (TL_red w) spec (a,b) w where
    gin tlab@(TL_red f) spec (x,y) = f [gapp tlab spec x, gapp tlab spec y]


instance (GAPP (TL_red_ctr w) spec a w w, GAPP (TL_red_ctr w) spec b w w)
    => LDat (TL_red_ctr w) spec (a,b) w where
    gin tlab@(TL_red_ctr f) spec (x,y) = 
	f "(,)" [gapp tlab spec x, gapp tlab spec y]


-- (semi-)sums
instance (GAPP TL_recon spec a dfa w)
    => LDat TL_recon spec (Maybe a) (Maybe w) where
    gin _    spec Nothing  = Nothing
    gin tlab spec (Just x) = Just (gapp tlab spec x)


instance (GAPP (TL_red w) spec a w w)
    => LDat (TL_red w) spec (Maybe a) w where
    gin (TL_red f)      spec Nothing  = f []
    gin tlab@(TL_red f) spec (Just x) = f [gapp tlab spec x]

instance (GAPP (TL_red_ctr w) spec a w w)
    => LDat (TL_red_ctr w) spec (Maybe a) w where
    gin (TL_red_ctr f)      spec Nothing  = f "Nothing" []
    gin tlab@(TL_red_ctr f) spec (Just x) = f "Just" [gapp tlab spec x]

-- Lists
{-
-- The following is the simplified/optimized definition
instance (STApply spec a df w,
	  LDat TL_recon spec a df)
    => LDat TL_recon spec [a] [w] where
    gin tlab spec xs = map (gapp tlab spec) xs

instance (STApply spec a w w,
	  LDat (TL_red w) spec a w)
    => LDat (TL_red w) spec [a] w where
    gin tlab@(TL_red f) spec xs = f . map (gapp tlab spec) $ xs
-}

-- The following definition is in the spirit of SYB. Cf. Maybe
instance (GAPP TL_recon spec [a] [w] [w], GAPP TL_recon spec a dfa w)
    => LDat TL_recon spec [a] [w] where
    gin tlab spec []     = []
    gin tlab spec (x:xs) = (gapp tlab spec x):(gapp tlab spec xs)

instance (GAPP (TL_red w) spec [a] w w, GAPP (TL_red w) spec a w w)
    => LDat (TL_red w) spec [a] w where
    gin tlab@(TL_red f) spec []     = f []
    gin tlab@(TL_red f) spec (x:xs) = f [gapp tlab spec x, gapp tlab spec xs]


instance (GAPP (TL_red_ctr w) spec [a] w w, GAPP (TL_red_ctr w) spec a w w)
    => LDat (TL_red_ctr w) spec [a] w where
    gin tlab@(TL_red_ctr f) spec []     = f "[]" []
    gin tlab@(TL_red_ctr f) spec (x:xs) = f ":" [gapp tlab spec x, 
						 gapp tlab spec xs]

-- Full sums (co-products)
instance (GAPP TL_recon spec a dfa wa, GAPP TL_recon spec b dfb wb)
    => LDat TL_recon spec (Either a b) (Either wa wb) where
    gin tlab spec (Left  x) = Left  (gapp tlab spec x)
    gin tlab spec (Right y) = Right (gapp tlab spec y)

instance (GAPP (TL_red w) spec a w w, GAPP (TL_red w) spec b w w)
    => LDat (TL_red w) spec (Either a b) w where
    gin tlab@(TL_red f) spec (Left x)  = f [gapp tlab spec x]
    gin tlab@(TL_red f) spec (Right y) = f [gapp tlab spec y]


-- Functions

instance (GAPP TL_recon spec b dfb wb)
    => LDat TL_recon spec (a->b) (a->wb) where
    gin tlab spec f = \a -> gapp tlab spec (f a)



-- ============= 
-- This part of the code is gsize client code. See also tests in this dir

-- First, the fully generic gsize functions: it counts the data constructors
-- in a complex term

gsize a = gapp (TL_red (\l -> 1 + sum l)) HNil  a


test1 = gsize (1::Int) -- 1

test2 = gsize [1::Int,2,3] -- 7 = 3 integers + 3 (:) plus one []

test3 = gsize "abc" -- 7, as above, with Char instead of Int

test4 = gsize ["abc"] -- 9: one extra (:) and one extra []

-- We now override the generic size processing for some specific
-- data type: string. We assign each String the fixed size 999.

gsize' a = gapp (TL_red (succ . sum)) 
	         ((\ (_::String) -> (999::Int)) :+: HNil)  a

test1' = gsize' (1::Int)

test2' = gsize' [1::Int,2,3] -- 7

test3' = gsize' "abc" -- 999

test4' = gsize' ["abc"] -- 1001


-- 999, 1, 1007
tests' = (gsize' ['a','b'],gsize' 'a', gsize' ([("a",True)],[1::Int]))

-- ============= 
-- Let us define a new generic function, to test if a given data structure
-- contains the letter 'a' (or an integer with the 'a' code) somewhere

hasa a = gapp (TL_red or) ((== 'a') :+: (== (fromEnum 'a')) :+: HNil) a
testh = (hasa ('x',False), 
	 hasa ('x',97::Int), hasa 'a', hasa [[["cde"],["abc"]]])

-- (False,True,True,True)


-- ============= 
-- Using the generic term replacement

term1 = ([1::Int,2], (True,('2',[(3::Int,4::Int)])))


-- we can define generic gmap in one line:
-- It takes a function f::a->b and a term and returns a term with
-- all values of type a replaced with the corresponding values of
-- type b. The type of the result is computed. When the input term
-- is a list, gmap is the ordinary map.
gmap f = gapp TL_recon (f :+: HNil) 

-- Traverse a term and increment all integers. 
-- This is similar to Ralf's example of raising the salary.

testi1 = gmap inci term1 where inci (x::Int) = succ x
-- ([2,3],(True,('2',[(4,5)])))

termc = (["ab"],(Just 'c', Just 'd'))
-- Replace all Chars with their Int equivalents
testc1 = gmap ord termc where ord (c::Char) = fromEnum c
-- Only replace those Chars that occur in Maybe Char
testc2 = gmap maybeord termc 
    where maybeord (Just (c::Char)) = Right (fromEnum c)
	  maybeord Nothing = Left 0
-- Fuse both testc1 and testc2 traversals
testc3 = gapp TL_recon (ord :+: maybeord :+: HNil) termc
    where maybeord (Just (c::Char)) = Right (fromEnum c)
	  maybeord Nothing = Left 0
	  ord (c::Char) = fromEnum c

-- replace all tuples (x,y) with Int elements with an array [x,y], and
-- negate all booleans
p2l a = gapp TL_recon ((\ (x::Int,y) -> [x,y]) :+: not :+: HNil) a

test_p2l = p2l term1
-- ([1,2],(False,('2',[[3,4]])))

-- replace an Int with a Double everywhere
i2d a = gmap (fromIntegral::Int->Double) a
test_i2d = i2d term1
-- ([1.0,2.0],(True,('2',[(3.0,4.0)])))

-- Replacing functions and under the functions
term2 = ([not], (True,('2',[(&&),(||)])))

-- traverse the term and replace Bool->Bool->Bool with
-- Int->Int->Int

testt2 = gmap (\ (f::Bool->Bool->Bool) -> ((+)::Int->Int->Int)) term2

-- *Syb4> :t testt2
-- testt2 :: ([Bool -> Bool], (Bool, (Char, [Int -> Int -> Int])))

-- traverse the term and replace Bool with a Char. Do the traversal
-- under the lambda!
testt3 = gmap (\x -> if x then 'a' else 'b') term2

-- *Syb4> :t testt3
-- testt3 :: ([Bool -> Char], (Char, (Char, [Bool -> Bool -> Char])))

testtt31 = let ([f],_) = testt3 in f True
-- 'b'


{-
The following code emulates a type class

class C a where fn :: a -> Int
instance C Bool where fn x = if x then 10 else 20
instance C Char where fn x = fromEnum x

-}

testtyc_fn x = 
    stapply ((\ (x::Bool) -> if x then 10 else 20) :+:
             (\ (x::Char) -> fromEnum x) :+:
	     HNil) 
	    ((error "no match")::Int) x


-- Generic Equality
-- We define a traversal strategy that traverses and reduces
-- two terms in lock-step
-- That is, we traverse the term (Couple x y) where x and y
-- have the same type, and reduce the result using the supplied
-- function 'f' 

data Couple a = Couple a a

--  TL_red_lockstep d f
-- Here, f is a reduction function. It reduces the result of traversing
-- the corresponding kids of the two terms (see the instance of LDat for
-- pairs for illustration). The value 'd' is returned when the two
-- terms in question are of the sum type and have different branches
-- of that sum (like (Couple Nothing (Just x))).
data TL_red_lockstep w = TL_red_lockstep w ([w] -> w)


instance LDat (TL_red_lockstep w) spec (Couple Int) w where
    gin (TL_red_lockstep _ f) spec (Couple x y) = f []

instance LDat (TL_red_lockstep w) spec (Couple Char) w where
    gin (TL_red_lockstep _ f) spec (Couple x y) = f []

instance LDat (TL_red_lockstep w) spec (Couple Bool) w where
    gin (TL_red_lockstep _ f) spec (Couple x y) = f []

instance (GAPP (TL_red_lockstep w) spec (Couple a) w w, 
	  GAPP (TL_red_lockstep w) spec (Couple b) w w)
    => LDat (TL_red_lockstep w) spec (Couple (a,b)) w where
    gin tlab@(TL_red_lockstep _ f) spec (Couple (x1,y1) (x2,y2)) = 
	f [gapp tlab spec (Couple x1 x2), 
	   gapp tlab spec (Couple y1 y2)]


-- (semi-)sums
instance (GAPP (TL_red_lockstep w) spec (Couple a) w w)
    => LDat (TL_red_lockstep w) spec (Couple (Maybe a)) w where
    gin (TL_red_lockstep _ f)      spec (Couple Nothing Nothing)  = f []
    gin tlab@(TL_red_lockstep _ f) spec (Couple (Just x) (Just y))
	= f [gapp tlab spec (Couple x y)]
    gin tlab@(TL_red_lockstep d _) spec _ = d


instance (GAPP (TL_red_lockstep w) spec (Couple a) w w,
	  GAPP (TL_red_lockstep w) spec (Couple [a]) w w)
    => LDat (TL_red_lockstep w) spec (Couple [a]) w where
    gin (TL_red_lockstep _ f)      spec (Couple [] [])  = f []
    gin tlab@(TL_red_lockstep _ f) spec (Couple (x:xs) (y:ys))
	= f [gapp tlab spec (Couple x y),
	     gapp tlab spec (Couple xs ys)]
    gin tlab@(TL_red_lockstep d _) spec _ = d


eq_clauses = (\ (Couple (x::Int) y) -> x == y) :+:
	     (\ (Couple (x::Char) y) -> x == y) :+:
	     (\ (Couple (x::Bool) y) -> x == y) :+:
	     HNil

geq x y = gapp (TL_red_lockstep False and) eq_clauses (Couple x y)

tte10 = geq (1::Int) (1::Int)
tte11 = geq ((1::Int),(2::Int)) (1,2)
tte12 = geq ((1::Int),(2::Int)) (2,1)
tte13 = geq ((1::Int),((3::Int),(2::Int))) (1,(2,2))
tte14 = geq ((1::Int),((3::Int),(2::Int))) (1,(3,2))

tte2  = geq True False
tte2' = geq True True
tte3  = geq (1::Int,True) (2,False)
tte3' = geq (True,True) (True,True)
tte4  = geq [(1::Int,True),(2::Int,False)] [(1,True)]
tte4' = geq [(1::Int,True),(2::Int,False)] [(1,True),(2,False)]


-- in STApply, we need a case for a guarded polymorphism
-- We can't use explicit quantification. We need HLists' Apply
{-
newtype Forall l g = Forall l

instance (Apply g a bf, STApply'' bf (HCons (Forall l g) r) a d w) 
    => STApply (HCons (Forall l g) r) a d w where
    stapply = stapply'' (undefined::bf)
class STApply'' bf spec a d w | bf spec a d -> w where
    stapply'' :: bf -> spec -> a -> d -> w
instance Apply l a w => STApply'' HTrue (HCons (Forall l g) r) a d w
    stapply'' _ ((Forall l) :+: _) a _ = apply l a
instance STApply r a d w => STApply'' HFalse (HCons (Forall l g) r) a d w
    stapply'' _ (_ :+: r) a _ = stapply r l a

gsize'' a = gmapq (SCons (\ (_::String) -> (999::Int)) 
		   (SCons2 (Just ()) AW_maybe_size 
		   SNil))
	    (succ . sum) a
data AW_maybe_size w = AW_maybe_size
instance ApplyW AW_maybe_size (Maybe a) where
    applyW _ Nothing = 1000::Int

-}


-- extracted from HList

data HNil = HNil
data HCons a b = a :+: b
infixr 5 :+:
data HTrue
data HFalse

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

class  TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance TypeCast HFalse b => TypeEq x y b

class  TypeEq2 x y b | x y -> b
instance TypeEq2 (x a1) (x a2) HTrue
instance TypeCast HFalse b => TypeEq2 (x a1) (y a2) b

class Apply f x y | f x -> y where
    apply :: f -> x -> y
    apply = undefined
