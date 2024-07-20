> {-# OPTIONS -fglasgow-exts #-}
> module GMap (mapList,mapListBTree,mapListBTreeList) where

> import Unsafe.Coerce

> import BinTreeDatatype
> import BinTreeReps
> import Data.Generics
> import Data.Char

> errorNotSupported = error ("SYB1_2.GMap: gmap cannot be implemented in SYB in a type safe way." ++
>                            " However, look at sources to see unsafe and non composition preserving variants")


======================
Oleg's gmap for SYB1_2
----------------------

Based on:

 * Claus' gmap        : http://www.haskell.org/pipermail/generics/2008-June/000343.html
                        http://www.haskell.org/pipermail/generics/2008-July/000351.html
and

 * Oleg's initial gmap: http://www.haskell.org/pipermail/generics/2008-July/000349.html


> mapList :: (Data a, Data b) => (a -> b) -> [a] -> [b]
> mapList = gmap


Note the type of gmap2 requires the input datum to have the type (c a)
The general solution could use the following type:
*> newtype Compose f g a = Compose{unComp :: f (g a)} deriving Data
Alas, we need to derive instance Typeable1 (Compose f g) by hand.
Leave this for now.

> mapListBTree :: (Data a, Data b) => (a -> b) -> [BinTree a] -> [BinTree b]
> mapListBTree f = unLB . gmap f . LB 
> newtype LB a = LB{unLB :: [BinTree a]} deriving (Data, Typeable)

> mapListBTreeList :: (Data a, Data b) => 
>	      (a -> b) -> [BinTree [a]] -> [BinTree [b]]
> mapListBTreeList f = unBLB . gmap f . BLB 
> newtype BLB a = BLB{unBLB :: [BinTree [a]]} deriving (Data, Typeable)


> example1 = [1,2,7,3,4]
> example2 = [Leaf 1 `Bin` Leaf 7,Leaf 3 `Bin` Leaf 4]
> example3 = [Leaf [1] `Bin` Leaf [2,7],Leaf [3] `Bin` Leaf [4]]

> toChar i = chr (i + ord 'A')
> test1 = mapList toChar example1
> test2 = mapListBTree toChar example2
> test3 = mapListBTreeList toChar example3

 example2b = [Leaf True `Bin` Leaf False,Leaf True `Bin` Leaf False]
 test2b :: [BinTree Int] = gmap' (\(x::Bool) -> fromEnum x) example2b


=======================================================================
-- I believe the following gmap is composition-preserving

> gmap :: forall a b c . (Data a, Data b, 
>                         Data (c a), Data (c b), Data (c X)) => 
>                         (a -> b) -> c a -> c b
> gmap f = gmapt f (Dyn (undefined::c X))


> data Tricky a = Tricky a Char deriving (Data,Typeable,Show)

> tricky1 = Tricky 'a' 'b'
> fun1 = chr . (+1) . ord
> fun2 = (=='a')


> mapTricky :: (Data a,Data b) => (a -> b) -> Tricky a -> Tricky b
> mapTricky = gmap


> tr_test3t = (mapTricky (fun2 . fun1) tricky1,
>              (mapTricky fun2 . mapTricky fun1) tricky1)

> clausTest = gmap not (True,True) :: (,) Bool Bool

Expected result: (True,False)


-- This code uses the following spot-mark defined in Claus' code

> -- "X marks the spots";-) X should be private
> data X = X deriving (Data,Typeable)

gmapt gets the value x to traverse and the template. The template is a
Dyn whose type has the same basic structure as that of x. The following
equation is supposed to hold:
  tt{X:=a} = typeOf x where (Dyn t) = template; tt = typeOf tt
where {X:=a} is a substitution that replaces all occurrences of a singleton
type X with some other suitable type a.
For example, 
   x has the type        [Int]
   template has the type [X]

   x has the type        Tricky Int Int
   template has the type Tricky X Int

   x has the type        Tricky Int Int
   template has the type Tricky X X
Although 'x' is the defined value, template is generally an undefined value.
The trick is to build the template `out of nothing', in a shallow way,
to the extent to enable further traversal. The trick is the 
observation that x and template should share the same data structure, 
or at least the same top-level data constructor.

The following includes an optimization: if typeof template == typeof x,
there is nothing to traverse. Only values that correspond to the mark X
in the template are mapped.

> gmapt :: (Data a, Data b, Data x, Data y) => (a -> b) -> Dyn -> x -> y
> gmapt f trep = maybe (\x -> traverse (trep,x)) ifmarked $ castfn f
>  where
>   hasmark :: Dyn -> Bool
>   hasmark (Dyn x) = typeOf x == typeOf X
>   -- ifmarked :: Typeable x => (x->y) -> (x->y)
>   ifmarked f x | hasmark trep = f x
>   ifmarked f x = traverse (trep,x)
>   -- optimization: t has no mark, there is nothing to map under it
>   traverse (Dyn t,x) | typeOf t == typeOf x = 
>               maybe (error "traverse1") id $ cast x
>   traverse (Dyn t,x) | (tcon,tkids) <- splitTyConApp (typeOf t),
>                        (con,kids)   <- splitTyConApp (typeOf x),
>                        not (length tkids == length kids &&
>                             tcon == con) =
>      error $ unwords ["template type", show (typeOf t),
>                       "inconsistent with value type", show (typeOf x)]
>   traverse (Dyn t,x) = rebuild (dynamize t1) xdyn
>     where xdyn@(con,kids) = dynamize x
>           t1 = fromConstr con `asTypeOf` t
>   rebuild (tcon, tkids) (con, kids) = 
>        case gunfold k (\g -> UnfldStateT g tkids kids) con of
>	      UnfldStateT a [] [] -> a
>   k (UnfldStateT ca (tkid:tkids) ((Dyn kid):kids)) = 
>         UnfldStateT (ca (gmapt f tkid kid)) tkids kids

> data UnfldStateT a = UnfldStateT a [Dyn] [Dyn]

> data Dyn = forall a. Data a => Dyn a
> data Kids a = Kids{growUp:: [Dyn]}

> dynamize :: Data a => a -> (Constr,[Dyn])
> dynamize x = (toConstr x, growUp $ gfoldl f (const (Kids [])) x)
>   where f (Kids l) a = Kids (l ++ [Dyn a])

> tdyn1 = dynamize "abcd"


> castfn :: (Typeable a, Typeable b, Typeable c, Typeable d) =>
>   (a -> b) -> Maybe (c -> d)
> castfn f = cast f





