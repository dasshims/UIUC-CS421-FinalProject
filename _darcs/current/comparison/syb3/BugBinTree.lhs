> {-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}

This file demonstrates a bug with generated Data instances.
(Note that this bug no longer exists in the Derive module in this
 directory. But probably it will crash using the library distribution
 ones.)

> module BugBinTree where

> import Data.Generics.SYB.WithClass.Basics
> import Data.Generics.SYB.WithClass.Instances
> import Data.Generics.SYB.WithClass.Derive
> import Language.Haskell.TH()

Run this to witness the bug

> main = print typeReps

> data BinTree a = Leaf a | Bin (BinTree a) (BinTree a)
>                  deriving Show



> tree = (Bin (Leaf 1) (Leaf 2))::BinTree Int

Datatype with existential type, it is Typeable.

> data Pack = forall x. Typeable x => Pack x

Map the children of |Bin| to Pack-ed values

> packedChildren = gmapQ geqCtx Pack tree

> typeOfPack (Pack x) = typeOf x

Now, get the representation of the children types.
Note, it loops when calling typeOf, this is solved when
the manual instance instead of the generated file is used (see below).

Note that the manual instance is like the generated one, but
comments out a recursive occurrence of the head in the context.

> typeReps = map typeOfPack packedChildren

> -- The context parameter
> geqCtx = undefined :: Proxy GEqD

> -- The dictionary type
> data GEqD a
>    = GEqD { geqD :: a -> a -> Bool }
> 
> -- The Sat instance
> instance GEq a => Sat (GEqD a)
>  where
>   dict = GEqD { geqD = geq }

> -- A new class for generic equality
> class GEq a
>  where
>   geq :: a -> a -> Bool
>

> -- The generic default
> instance Data GEqD a => GEq a where
>   geq x y = undefined

> $(derive [''BinTree])

The code below is a corrected version of the derived stuff above.
It will make "main" work.

> {-
> instance Typeable1 BinTree where
>   { typeOf1 _ = mkTyConApp
>                 (mkTyCon "BinTreeDatatype.BinTree") [] }
> 
> a1lh :: Constr
> a1lh = mkConstr a1lg "Leaf" [] Prefix
> a1li :: Constr
> a1li = mkConstr a1lg "Bin" [] Prefix
> a1lg :: DataType
> a1lg = mkDataType "BinTree" [a1lh, a1li]
> 
> instance (Data ctx a,
>                   {-Data ctx (BinTree a),-}
>                   Sat (ctx (BinTree a))) =>
>                   Data ctx (BinTree a) where
>               gfoldl _ f z x
>                                                           = case x of
>                                                               Leaf arg
>                                                                 -> f (z Leaf) arg
>                                                               Bin a1ln a1lo
>                                                                 -> f
>                                                                      (f (z Bin) a1ln)
>                                                                      a1lo
>               gunfold _ k z c
>                                                            = case
>                                                                  constrIndex c
>                                                              of
>                                                                1 -> k (z Leaf)
>                                                                2 -> k (k (z Bin))
>                                                                _ -> error "gunfold: fallthrough"
>               toConstr _ x
>                                                             = case x of
>                                                                 Leaf _ -> a1lh
>                                                                 Bin _ _ -> a1li
>               dataTypeOf _ _ = a1lg 
> -}

