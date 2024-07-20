{-# OPTIONS -fglasgow-exts #-}

 module GL where

 import Prettier hiding (Pretty, pretty, char, int, integer, float, block, render, prettyList)
 import qualified Prettier
 import Data.Generics hiding (Generic)

 data Bit = O | I

 class ShowBin2 t where
     showBin2 :: t -> [Bit]
 instance ShowBin2 Char where
     showBin2 = showBinChar
 instance ShowBin2 Int where
     showBin2 = showBinInt
 instance ShowBin2 a => ShowBin2 [a] where
     showBin2 []        = [O]
     showBin2 (x : xs)  = I : (showBin2 x ++ showBin2 xs)

 newtype ShowBin a  =  ShowBin { showBin' :: a -> [Bit] }

 instance Generic ShowBin where
   unit              =  ShowBin (const [])
   char              =  ShowBin showBinChar
   int               =  ShowBin showBinInt
   plus a b          =  ShowBin (\ x -> case x of  Inl l ->  O  :  showBin' a l
                                                   Inr r ->  I  :  showBin' b r)
   prod a b          =  ShowBin (\ ( x :*: y) -> showBin' a x ++ showBin' b y)
   view iso a        =  ShowBin (\ x -> showBin' a (from iso x))
   float             =  error "Not implemented"

 class Generic g where
    unit        :: g Unit
    char        :: g Char
    int         :: g Int
    plus        :: g a -> g b -> g (a :+: b)
    prod        :: g a -> g b -> g (a :*: b)
    constr      :: Name -> Arity -> g a -> g a
    constr _ _  = id
    view        :: Iso b a -> g a -> g b
    float       :: g Float          

 data Unit'       =  Unit'

 data a ::+::  b  =  Inl' a | Inr' b

 data a ::*:: b   =  a ::*:: b

 data Iso a b = Iso {-"~"-} {from :: a -> b, to :: b -> a}

 isoList :: Iso [a] (Unit :+: (a :*: [a]))
 isoList = Iso fromList toList

 fromList :: [a] -> Unit :+: (a :*: [a])
 fromList []               = Inl Unit
 fromList (x:xs)           = Inr (x :*: xs)

 toList :: Unit :+: (a :*: [a]) -> [a]
 toList (Inl Unit)        = []
 toList (Inr (x :*: xs))  = x : xs

 class Size a where
     size :: a -> Int
 instance Size Int where
     size _ = 1
 instance Size Char where
     size _ = 0
 instance Size Unit where
     size _ = 0
 instance (Size a, Size b) => Size (a :+: b) where
     size (Inl x)  = size x
     size (Inr y)  = size y
 instance (Size a, Size b) => Size (a :*: b) where
     size (x :*: y) = size x + size y
 instance Size a => Size [a] where
     size = size . fromList
 instance Size Bool where
     size _ = 1
 instance Size a => Size (Tree a) where
     size = size . fromTree

 isoTree = Iso fromTree toTree

 fromTree Empty           = Inl Unit
 fromTree (Fork i l x r)  = Inr (i :*: (l :*: (x :*: r)))

 toTree (Inl Unit)                        = Empty
 toTree (Inr (i :*: (l :*: (x :*: r))))   = Fork i l x r

 type Name   = String
 type Arity  = Int

 newtype Count a                =  Count { count' :: a -> Int }
 
 instance Generic Count where
    unit                        =  Count (const 0)
    plus a b                    =  Count (\x -> case x of  Inl l  ->  count' a l
                                                           Inr r  ->  count' b r)
    prod a b                    =  Count (\x -> count' a (outl x) + count' b (outr x))
    view iso a                  =  Count (count' a . from iso)
    char                        =  Count (const 0)
    int                         =  Count (const 0)
    float                       =  Count (const 0)

 outl (x :*: y) = x
 outr (x :*: y) = y

 fsize  :: FunctorRep f => f a -> Int
 fsize  =  count' (functorRep (Count (const 1)))

 class FunctorRep f where
      functorRep  :: Generic g => g a -> g (f a)

 fsum  :: FunctorRep f => f Int -> Int
 fsum  = count' (functorRep (Count id))

 instance FunctorRep [] where
   functorRep   =  rList

 infixr 8 <*>
 infixr 7 <|>

 (<|>)  :: Generic g => g a -> g b -> g (a :+: b)
 (<|>)  = plus

 (<*>)  :: Generic g => g a -> g b -> g (a :*: b)
 (<*>)  = prod

 instance FunctorRep Tree where
    functorRep   =  rTree

 class Rep a where
   rep                     :: (Generic g) => g a
 instance Rep Unit where
   rep                     =  unit
 instance Rep Char where
   rep                     =  char
 instance Rep Int where
   rep                     =  int
 instance (Rep a, Rep b) => Rep (a :+: b) where
   rep                     =  plus rep rep 
 instance (Rep a, Rep b) => Rep (a :*: b) where
   rep                     =  prod rep rep
 instance Rep a => Rep [a] where
   rep                     =  rList rep
 instance Rep a => Rep (Tree a) where
   rep                     = rTree rep

 showBin3 :: Rep t => t -> [Bit]
 showBin3 = showBin' rep

 rList :: Generic g => g a -> g [a]
 rList a = view isoList (constr "[]" 0 unit `plus` constr "(:)" 1 (a `prod` rList a))

 countZero :: Rep t => t -> Int
 countZero = count' rep

 encodeList :: (a -> [Bit]) -> [a] -> [Bit]
 encodeList f = showBin' (rList $ ShowBin f)

 class Generic g => GenericList g where
     list :: g a -> g [a]
     list = rList

 instance GenericList ShowBin where
    list a =  ShowBin (\ x ->  showBinInt (length x) ++ 
                               concatMap (showBin' a) x)

 class RBin' t where
     showbin                    :: t -> [Bit]

 instance RBin' Unit where
     showbin                    = showBin' unit
 instance RBin' Int where
     showbin                    = showBin' int
 instance RBin' Char where
     showbin                    = showBin' char
 instance (RBin' a, RBin' b) => RBin' (a :+: b) where
     showbin                    = showBin' (plus overbin overbin)
 instance (RBin' a, RBin' b) => RBin' (a :*: b) where
     showbin                    = showBin' (prod overbin overbin)

 class RCount a where
     count                 :: a -> Int

 instance RCount Unit where
     count                 =  count' overCount
 instance (RCount a, RCount b) => RCount (a :+: b) where
     count                 =  count' (plus overCount overCount)
 instance (RCount a, RCount b) => RCount (a :*: b) where
     count                 =  count' (prod overCount overCount)
 instance RCount Char where
     count                 =  count' char
 instance RCount Int where
     count                 =  count' int

 overCount :: RCount a => Count a
 overCount = Count count

 overbin :: RBin' a => ShowBin a
 overbin = ShowBin showbin

 instance RBin' a => RBin' [a] where
    showbin = showBin' (list overbin)

 class Generic g => GenericBool g where
     bool :: g Bool

 instance GenericBool Count where
     bool = Count (const 0)

 showBinBool False = [O]
 showBinBool True  = [I]

 instance GenericBool ShowBin where
     bool = ShowBin showBinBool
 instance RCount Bool where
     count                 = count' bool
 instance RBin' Bool where
     showbin               = showBin' bool
 instance RCount a => RCount [a] where
     count                 =  count' (list overCount)
 instance GenericList Count where
     list a = Count (\l -> 1 + count' (rList a) l)

 showBinString = showBin' (list (ShowBin showBinCharBE))

 showBinCharBE = showBinChar

 data BTree a b = BLeaf a | BFork b (BTree a b) (BTree a b) deriving Show

 instance RCount a => RCount (BTree a b) where
     count                  = count' (btree overCount (Count (const 0)))

 countBTree :: (RCount a, RCount b) => BTree a b -> Int
 countBTree = count' (btree overCount (Count (const 1)))

 btree ra rb = view isoBTree (  constr "BLeaf" 1 ra <|> 
                                constr "BFork" 3 (rb <*> btree ra rb <*> btree ra rb))

 isoBTree = Iso fromBTree toBTree

 fromBTree (BLeaf x)                 = Inl x
 fromBTree (BFork x r l)             = Inr (x :*: (r :*: l))

 toBTree (Inl x)                     = BLeaf x
 toBTree (Inr (x :*: (r :*: l)))     = BFork x r l

 testBTree :: BTree Integer Int
 testBTree = BFork 3 (BLeaf 2) (BFork 4 (BLeaf 5) (BLeaf 6))

 instance RCount Integer where
     count = const 1

 t1 :: BTree Int Int
 t1 = BFork 5 (BFork 3 (BLeaf 2) (BLeaf 4)) (BLeaf 6)

 line                          :: Doc
 line                          =  nl

 prettyChar                    :: Char -> Doc
 prettyChar                    =  Prettier.pretty

 prettyString                  :: String -> Doc
 prettyString                  =  Prettier.pretty

 prettyInt                     :: Int -> Doc
 prettyInt                     =  Prettier.int

 render                        :: Int -> Doc -> IO ()
 render n d                    =  putStrLn (Prettier.render (Page n) d)

 {-
 pretty                        :: (TypeRep a) => a -> Doc
 pretty                        =  pretty' typeRep
 -}

 atree :: Tree Int
 atree = Fork 2 (Fork 0 Empty 4 Empty) 5 (Fork 1 (Fork 0 Empty 3 Empty) 7 (Fork 0 Empty 3 Empty))

 t :: Tree Char
 t =  Fork 1 (Fork 0 Empty 'h' Empty) 'i' (Fork 0 Empty '!' Empty) 

 newtype Pretty a              =  Pretty { pretty' :: a -> Doc }

 instance Generic Pretty where
   unit                        =  Pretty (const empty)
   char                        =  Pretty (prettyChar)
   int                         =  Pretty (prettyInt)
   plus a b                    =  Pretty (\ x ->  case x of  Inl l  ->  pretty' a l
                                                             Inr r  ->  pretty' b r)
   prod a b                    =  Pretty (\ (x :*: y) ->  pretty' a x <> line <> pretty' b y)
   view iso a                  =  Pretty (pretty' a . from iso)
   constr n ar a               =  Pretty (prettyConstr n ar a)
   float                       =  error "not implemented"

 prettyConstr n ar a x = let s = text n in
     if ar == 0 then s
     else group (nest 1 (text "("  <> s <> line <> pretty' a x <> text ")" ))

 data Tree a = Empty | Fork Int (Tree a) a (Tree a)

 class Generic g => GenericTree g where
   tree :: g a -> g (Tree a)
   tree a = view isoTree (  constr  "Empty"  0  unit `plus`
                            constr  "Fork"   4  (int `prod` (  rTree a `prod` 
                                                               (a `prod` rTree a))))

 instance GenericTree Pretty

 rTree           :: Generic g => g a -> g (Tree a)
 rTree a         =  view isoTree (  constr  "Empty"  0  unit <|>
                                    constr  "Fork"   4  (int <*> rTree a <*> a <*> rTree a))

 class RPretty a where
     pretty                 :: a -> Doc
     prettyList             :: [a] -> Doc
     prettyList             =  pretty' (list oPretty)
 instance RPretty Unit where
     pretty                 =  pretty' oPretty
 instance RPretty Char where
     pretty                 =  pretty' char
     prettyList             =  prettyString
 instance RPretty Int where
     pretty                 =  pretty' int
 instance (RPretty a, RPretty b) => RPretty (a :+: b) where
     pretty                 =  pretty' (plus oPretty oPretty)
 instance (RPretty a, RPretty b) => RPretty (a :*: b) where
     pretty                 =  pretty' (prod oPretty oPretty)
 instance RPretty a => RPretty (Tree a) where
     pretty                 =  pretty' (tree oPretty)

 oPretty :: RPretty t => Pretty t
 oPretty = Pretty pretty

 instance GenericList Pretty where
    list p = Pretty (\x -> 
      case x of  []      -> text "[]"
                 (a:as)  -> group (nest 1 (text "[" <> pretty' p a <> rest as)))
      where  rest []              =  text "]"
             rest (x : xs)        =  text "," <> line <> pretty' p x <> rest xs

 instance RPretty a => RPretty [a] where
     pretty                 = prettyList 

 data Pretty1 a = Pretty1 {  pretty1      :: a -> Doc,
                             prettyList1  :: [a] -> Doc}

 showBinChar                   :: Char -> [Bit]
 showBinChar i                 =  showBinIntegral 7 (fromEnum i)       -- HACK

 showBinInt                    :: Int -> [Bit]
 showBinInt i                  =  showBinIntegral 16 (fromEnum i)       -- HACK

 showBinIntegral               :: (Integral a) => Int -> a -> [Bit]
 showBinIntegral 0 _n          =  []
 showBinIntegral (k + 1) n
     | r == 0                  =  O : showBinIntegral k q
     | otherwise               =  I : showBinIntegral k q
     where (q, r)              =  divMod n 2

{-
 class RBin t where
     showBin                    :: t -> [Bit]
     showBin {| Unit |}         = showBin' unit
     showBin {| a :+: b |}      = showBin' (plus overBin overBin)
     showBin {| a :*: b |}      = showBin' (prod overBin overBin)
 instance RBin Int where
     showBin                    = showBin' int
 instance RBin Char where
     showBin                    = showBin' char

 overBin :: RBin a => ShowBin a
 overBin = ShowBin showBin
-}

 data OddList a    = OValue a | OCons a (EvenList a)

 data EvenList a   = ENil | ECons a (OddList a)

 data PTree a      = PEmpty | PFork a (PTree (a,a))

{-
 instance RBin a => RBin (OddList a)
 instance RBin a => RBin (EvenList a)
 instance RBin a => RBin (PTree a)
 instance (RBin a, RBin b) => RBin (a,b)
-}

 class GRep g t where
    over :: g t

 instance Generic g => GRep g Unit where
    over = unit
 instance Generic g => GRep g Int where
    over = int
 instance Generic g => GRep g Char where
    over = char
 instance (Generic g, GRep g a, GRep g b) => GRep g (a :+: b) where
    over = plus over over
 instance (Generic g, GRep g a, GRep g b) => GRep g (a :*: b) where
    over = prod over over
 instance (GenericList g, GRep g a) => GRep g [a] where
    over = list over
 instance (GenericTree g, GRep g a) => GRep g (Tree a) where
    over = tree over

 showBin1 :: GRep ShowBin t => t -> [Bit]
 showBin1 = showBin' over

 pretty2 :: GRep Pretty t => t -> Doc
 pretty2 = pretty' over
