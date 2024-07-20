{-# OPTIONS_GHC -fglasgow-exts  #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
  -- The latter extension is needed only for GHC 6.4, it seems...

-- This test is in a sense the `inverse' of SelectCompany
-- Given a data type such as company, we can extract all the leaf values
-- such as Names, Salaries, etc. into a list of strings, which can then
-- be transported across the network.
-- This module does the reverse operation: given an object A (which
-- defines the structure) and a list of strings made from deserialization of
-- an object B, `update' the object A with the new field so as to recover
-- an object B. 
-- The listification is the primitive form of serialization:
-- if the type of A includes sum, we assume that the object B had exactly
-- the same choice of the sum branch. A better serialization should
-- record which branch was taken. We evade this issue here for the sake
-- of simplicity.

module Deserialize (newCompany) where

import SmashA.Syb4A

-- Two ways to fold ints from the tree defined in
import CompanyDatatypes
import TreeDatatype

import SmashA.CompanyDats
import SmashA.TreeDats

import Control.Monad.State


primitive_fields_show =
    (\ (s::Int)    -> [show s]) :+:
    (\ (s::Float)  -> [show s]) :+:
    (\ (s::String) -> [s]) :+:
    HNil				-- more can be added in the future

readM proto = do
	      (s:_) <- get
	      modify tail
	      return (read s `asTypeOf` proto)

readSM proto = do
	      (s:_) <- get
	      modify tail
	      return (s `asTypeOf` proto)

asMType :: m a -> m b -> m a
asMType m1 m2 = m1

primitive_fields_read mproto =
    (\ (s::Int)    -> readM s `asMType` mproto) :+:
    (\ (s::Float)  -> readM s `asMType` mproto) :+:
    (\ (s::String) -> readSM s `asMType` mproto) :+:
    HNil				-- more can be added in the future

-- Serializer: take an object and produce the list of all its
-- primitive fields.

serialize xs = gapp (TL_red concat) primitive_fields_show xs

deserialize proto xs = 
    evalState (gapp (TL_reconM) (primitive_fields_read undefined) proto) xs


test1 = serialize genCom
-- ["Research","Laemmel","Amsterdam","8000.0","Joost","Amsterdam",
--  "1000.0","Marlow","Cambridge","2000.0","Strategy","Blair",
-- "London","100000.0"]

retro = ["Metaphysics",
	 "Kant","Koeningsberg","800.0",
	 "Hume","Edinburgh","100.0",
	 "Marlowe","Cambridge","200.0",
	 "Ruling","Thatcher","London","50000.0"]

test2 = deserialize genCom retro

newCompany = undefined

-- mytest = print ( selectSalary genCom
--               , selectInt mytree
--               )

getP = P "Lammel" "Amsterdam"
t2 = serialize getP

-- The following can be used from GHCi prompt without any flags...
serialize1 xs = gapp (TL_red (concat::[[String]]->[String])) primitive_fields_show xs
