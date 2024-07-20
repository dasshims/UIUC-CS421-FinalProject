{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Scratch your boler-plate III: an alternative approach
-- It still needs recursive instances, but in a different way
-- compared to SYB3.


-- ============= 
-- This part of the code is the SYB library
-- It is independent of any generic functions. The instances therein
-- depend on the data types only (and, can hopefully, can be automatically
-- derived for new data types)

-- Unlike SYB3, Dat is not a subclass of any class (SAT class in SYB3).
-- Thus the dictionary for Dat does not depend on other dictionaries.
-- Furthermore, the function gmapq has _no_ higher-rank type.
-- The type ctx is the label to choose the appropriate FN function to
-- apply. To be more precise, we chose the FN instance based on 
-- the concrete data type and the expected result type.

class Dat ctx a where
  gmapq :: (ctx ->w) -> a -> [w]


instance Dat ctx Int where
  gmapq _ _ = []
instance Dat ctx Bool where
  gmapq _ _ = []
instance Dat ctx Char where
  gmapq _ _ = []

-- Because Dat ctx [a] depends on FN ctx [a], we get the recursive
-- instance dependency.
instance (FN ctx a, FN ctx [a]) => Dat ctx [a] where
  gmapq _ [] = []
  gmapq u (a:am)  = [u $ fn a, u $ fn am]

instance (FN ctx a, FN ctx b) => Dat ctx (a,b) where
  gmapq u (a,b)  = [u $ fn a, u $ fn b]

class FN label a where
  fn :: a -> label

-- ============= 
-- This part of the code is gsize library. It defines the generic
-- function gsize, irrespectively of the structure of the types
-- to which it may apply.

class Size a where
  gsize :: a -> Int

instance Dat SZ a => Size a  where
  gsize a = 1 + sum (gmapq unSZ a)

-- We use SZ in two ways. First, we use its type as a label to
-- select the proper instance of FN. Second, we use SZ to encapsulate
-- the result type. So, we can select the proper instance of generic
-- function based on the argument type and the result type.

newtype SZ = SZ{unSZ :: Int}

-- critical: Size a in the constraint delays the resolution and so
-- gets around overlapping instances
instance Size a => FN SZ a where fn = SZ . gsize

-- ============= 
-- This part of the code is gsize client code.

-- First, we override the generic size processing for some specific
-- data type: string.
instance Size String where gsize a = 999

test1 = gsize (1::Int)

test2 = gsize [1::Int,2,3]

test3 = gsize "abc"

test4 = gsize ["abc"]

tests = (gsize ['a','b'],gsize 'a', gsize ([("a",True)],[1::Int]))

-- ============= 
-- Let us define a new generic function, to test if a given data structure
-- contains the letter 'a' somewhere.

 -- Library part
newtype HASA = HASA{uhasa :: Bool}
class HasA a where hasa :: a -> Bool

instance Dat HASA a => HasA a where
  hasa a = or (gmapq uhasa a)
instance HasA a => FN HASA a where fn = HASA . hasa

 -- Client part: overriding generic processing for some particular
 -- data types
instance HasA Char where hasa x = x == 'a'
instance HasA Int  where hasa x = x == fromEnum 'a'

testh = (hasa ('x',False), 
	 hasa ('x',97::Int), hasa 'a', hasa [[["cde"],["abc"]]])

main = print testh
