{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
-- The overlapping instances are needed exclusively for the implementation
-- of TypeEq.

-- Scratch your boiler-plate IV:
-- Smash your boilerplate without class and without Typeable
-- No recursive instances required, no Typeable, and no higher-rank types.

-- First we describe gsize with exceptions (example from the SYB3 paper).
-- Then we tackle a more
-- complex transformation: traversing a complex term and replacing
-- subterm of a particular type with something else.
-- For example, traversing the data type of expressions and replacing
-- all `variables' with labeled variable types. Or traversing a term
-- and replacing all tuples with integer component with an array
-- of two elements. A particular, simpler case if Ralf's dream example: raising
-- the salary of everyone by 10%. The latter is a simple example because
-- the transformed term has the same type.


module Smash.Syb4 (
	     -- simple reducers
	     Dat(..),
	     SApply(..),
	     SCons(..),
	     SNil(..),
	     STApply(..),
	     TDat(..),
	     HNil(..),
	     HCons(..)

	    ) where

-- ============= 
-- This part of the code is the SYB library
-- It is independent of any generic functions. The instances therein
-- depend on the data types only (and, can hopefully, can be automatically
-- derived for new data types)

-- In this part, we define a generic query function gmapq, which recursively
-- traverses a term in the depth-first order, and applies reducer
-- to the list of term's children.

-- This class 'Dat' is analogous to 'Data' of SYB. This class
-- determines how to traverse a particular data structure. The instances
-- of the class depend only on the data to traverse and do not depend
-- on a generic transformation to apply.
class SApply spec a => Dat spec a where
  gmapq   :: spec w -> ([w]->w) -> a -> w
  genmapq :: spec w -> ([w]->w) -> a -> w
  gmapq spec reducer x = sapply spec x (genmapq spec reducer x)
  genmapq _ reducer x = reducer []


instance SApply spec Int  => Dat spec Int
instance SApply spec Bool => Dat spec Bool
instance SApply spec Char => Dat spec Char

instance (SApply spec [a], Dat spec a) => Dat spec [a] where
  genmapq _ reducer [] = reducer []
  genmapq spec reducer (a:am)  = reducer [gmapq spec reducer a,
					  gmapq spec reducer am]

instance (SApply spec (a,b), SApply spec a, SApply spec b, 
	  Dat spec a, Dat spec b) 
    => Dat spec (a,b) where
  genmapq spec reducer (a,b)  = reducer [gmapq spec reducer a,
					 gmapq spec reducer b]


-- Applying exceptional rules

-- A heterogeneous list of functions a->w where all functions in the
-- list have the same w. The type a varies from one list member to another.
-- The property that each function in the list has the same result type
-- is guaranteed by construction of the datatype.

data SNil w = SNil
data SCons a b w = SCons (a->w) (b w)
data SCons2 a l b w = SCons2 a (l w) (b w)

-- Given the datum of the type 'a' check if any function in the list
-- spec w has the argument type 'a' and so can be applied. If there is
-- such a function, we apply it. Otherwise, we return the default,
-- the third argument of sapply.
class SApply spec a where
    sapply :: spec w -> a -> w -> w

instance SApply SNil a where
    sapply _ _ deflt = deflt

instance (TypeEq a a' bf, SApply' bf (SCons a' r) a)
    => SApply (SCons a' r) a where
    sapply = sapply' (undefined::bf)

class SApply' bf spec a where
    sapply' :: bf -> spec w -> a -> w -> w

instance SApply' HTrue (SCons a r) a where
    sapply' _ (SCons p _) x _ = p x

instance SApply r a => SApply' HFalse (SCons a' r) a where
    sapply' _ (SCons _ r) x deflt = sapply r x deflt


-- ============= 
-- This part of the code is gsize client code.

-- First, the fully generic gsize functions: it counts the data constructors
-- in a complex term

gsize a = gmapq SNil (\l -> 1 + sum l) a


test1 = gsize (1::Int) -- 1

test2 = gsize [1::Int,2,3] -- 7 = 3 integers + 3 (:) plus one []

test3 = gsize "abc" -- 7, as above, with Char instead of Int

test4 = gsize ["abc"] -- 9: one extra (:) and one extra []

-- First, we override the generic size processing for some specific
-- data type: string. We assign each String the fixed size 999.

gsize' a = gmapq (SCons (\ (_::String) -> (999::Int)) SNil) (succ . sum) a

test1' = gsize' (1::Int)

test2' = gsize' [1::Int,2,3] -- 7

test3' = gsize' "abc" -- 999

test4' = gsize' ["abc"] -- 1001


-- 1001, 1, 1007
tests' = (gsize' ['a','b'],gsize' 'a', gsize' ([("a",True)],[1::Int]))

-- ============= 
-- Let us define a new generic function, to test if a given data structure
-- contains the letter 'a' (or an integer with the 'a' code) somewhere

hasa a = gmapq (SCons (== 'a') (SCons (== (fromEnum 'a')) SNil)) or a
testh = (hasa ('x',False), 
	 hasa ('x',97::Int), hasa 'a', hasa [[["cde"],["abc"]]])

-- (False,True,True,True)


-- ----------------------------------------------------------------------
-- Term replacement. Can't expect the result to be of the same type.
-- OTH, the purely generic part is easy: since we can't know anything
-- about the argument, the replacement is either the same or a value of some
-- fixed type.

-- This part of the code is again in the library.

-- Like SApply, the `spec' is the heterogeneous list of functions a->w
-- Unlike SApply, here the return types w can vary, too, from one function
-- to another. Given the list of functions a_i->w_i and the datum of the type
-- a, we check to see if 'a' is the same as one of the a_i. If we found
-- the function that takes the value of the type a, we apply that function.
-- Otherwise we return the default value, of some type d.
-- Because the default value d and the return values of each function in
-- spec are in general different, the return value 'w' is one of those, and
-- determined by spec, a, and d.

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

class ApplyW f a where
    applyW :: f w -> a -> w
    applyW = undefined
-}


-- Traverse a datum of the type 'a' and return another datum 'w'
-- This class TDat only tells which data types are traversible.
-- TDat does not depend on the generic function.
class TDat spec a w | spec a -> w where
  gtmapq   :: spec -> a -> w

instance STApply spec Int Int w => TDat spec Int w where
    gtmapq spec x = stapply spec x x

instance STApply spec Char Char w => TDat spec Char w where
    gtmapq spec x = stapply spec x x

instance STApply spec Bool Bool w => TDat spec Bool w where
    gtmapq spec x = stapply spec x x

instance (TDat spec a w', STApply spec (Maybe a) (Maybe w') w)
    => TDat spec (Maybe a) w where
    gtmapq spec x@Nothing  = stapply spec x (Nothing::Maybe w')
    gtmapq spec x@(Just a) = stapply spec x (Just $ gtmapq spec a)

instance (TDat spec a wa, TDat spec b wb,
	  STApply spec (Either a b) (Either wa wb) w)
    => TDat spec (Either a b) w where
    gtmapq spec x@(Left a)
	= stapply spec x ((Left  $ gtmapq spec a)::Either wa wb)
    gtmapq spec x@(Right a)
	= stapply spec x ((Right $ gtmapq spec a)::Either wa wb)

instance (TDat spec a w', STApply spec [a] [w'] w) => TDat spec [a] w where
    gtmapq spec x = stapply spec x (map (gtmapq spec) x)
				    

instance (TDat spec a a', TDat spec b b',
	  STApply spec (a,b) (a',b') w)
    => TDat spec (a,b) w where
    gtmapq spec x@(a,b) = stapply spec x (gtmapq spec a,gtmapq spec b)


instance (TDat spec b b', STApply spec (a->b) (a->b') w)
    => TDat spec (a->b) w where
    gtmapq spec f = stapply spec f (\a -> gtmapq spec (f a))

-- ============= 
-- Using the generic term replacement

term1 = ([1::Int,2], (True,('2',[(3::Int,4::Int)])))

-- Traverse a term and increment all integers. 
-- This is similar to Ralf's example of raising the salary.

inci a = gtmapq ((\ (x::Int) -> x + 1) :+: HNil) a

testi1 = inci term1
-- ([2,3],(True,('2',[(4,5)])))

-- replace all tuples (x,y) with Int elements with an array [x,y], and
-- negate all booleans
p2l a = gtmapq ((\ (x::Int,y) -> [x,y]) :+: not :+: HNil) a

test_p2l = p2l term1
-- ([1,2],(False,('2',[[3,4]])))

-- replace an Int with a Double everywhere
i2d a = gtmapq ((fromIntegral::Int->Double) :+: HNil) a
test_i2d = i2d term1
-- ([1.0,2.0],(True,('2',[(3.0,4.0)])))


-- Replacing functions and under the functions
term2 = ([not], (True,('2',[(&&),(||)])))

-- traverse the term and replace Bool->Bool->Bool with
-- Int->Int->Int

testt2 = gtmapq ((\ (f::Bool->Bool->Bool) -> ((+)::Int->Int->Int))
		 :+: HNil) term2

-- *Syb4> :t testt2
-- testt2 :: ([Bool -> Bool], (Bool, (Char, [Int -> Int -> Int])))

-- traverse the term and replace Bool with a Char. Do the traversal
-- under the lambda!
testt3 = gtmapq (( \x -> if x then 'a' else 'b')
		 :+: HNil) term2

-- *Syb4> :t testt3
-- testt3 :: ([Bool -> Char], (Char, (Char, [Bool -> Bool -> Char])))

testtt31 = let ([f],_) = testt3 in f True
-- 'b'

{-
gsize'' a = gmapq (SCons (\ (_::String) -> (999::Int)) 
		   (SCons2 (Just ()) AW_maybe_size 
		   SNil))
	    (succ . sum) a
data AW_maybe_size w = AW_maybe_size
instance ApplyW AW_maybe_size (Maybe a) where
    applyW _ Nothing = 1000::Int
-}


{-
The following code emulates a type class

class C a where fn :: a -> Int
instance C Bool where fn x = if x then 10 else 20
instance C Char where fn x = fromEnum x

-}

testtyc_fn x = sapply (SCons (\ (x::Bool) -> if x then 10 else 20)
                 (SCons (\ (x::Char) -> fromEnum x)
		  SNil)) ((error "no match")::Int) x

__ = __
{-
gminimum () = r 
  where r = gtmapq lst (__ `asTypeOf` r)
        lst = (mb (__::Int)) :+: (mb (__::Bool)) :+: (mb (__::Char))
	      -- default behavior for pairs suffices
              -- for Maybe: return Nothing
              -- for Either a b: return Left (gminimum :: a)
              -- for arrays: []
	      :+: HNil
        mb :: Bounded t => t -> t -> t
	mb _ _ = minBound

test_gmin1 = gminimum () :: (Int,Bool)
-}

-- Alas, the following doesn't actually work for pairs and arrays.
-- reason: we'd like to match for polymorphic arrays (_any_ arrays)
-- Do we need a new traversal combinator?
-- Or we can somehow transform the two term to the term of
-- booleans, and then reduce that to a single boolean?
-- See the separate message on exactly this topic: poly2.txt

te1 x | False = \y -> const False [x,y]
te1 x = stapply ((\ (x::Int) -> (== x)) :+: (\ (x::Bool) -> (== x)) 
		 :+: (\ (x,y) (x',y') -> te1 x x' && te1 y y')
		 :+: (\x y -> and $ zipWith te1 x y)
		 :+: HNil) x (const False)

tte1 = te1 (1::Int) (1::Int)
tte2 = te1 True False
tte2' = te1 True True
tte3 = te1 (1,True) (2,False)
tte3' = te1 (True,True) (True,True)
tte4 = te1 [] []
-- tte4 = te1 [(1,True),(2,False)] [(1,True),(2,False)]

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

