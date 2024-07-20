{-# OPTIONS_GHC -fglasgow-exts  #-}

{-
This test exercices GENERIC read, show, and eq for the company
datatypes which we use a lot. The output of the program should be
"True" which means that "gread" reads what "gshow" shows while the
read term is equal to the original term in terms of "geq".
-}

module GEq (equalCompany, equalGRoseListInt, equalBTreeInt, geq,
	    equalNGRoseListInt) where


import CompanyDatatypes
import CompanyDats

import GRoseDatatype
import RoseDats

import NGRoseDatatype
import NGRoseDats

import BinTreeDatatype
import BTreeDats

import Syb4A hiding (geq)

-- We enhance the generic equality to compare Floats by value...
-- This is the illustartion of local redefinitions
eq_clauses1 = (\ (Couple (x::Float) y) _ -> x == y) :+:
	     eq_clauses

geq x y = gapp (TL_red_lockstep False and) eq_clauses1 (Couple x y)

equalCompany :: Company -> Company -> Bool
equalCompany = geq

equalGRoseListInt :: GRose [] Int -> GRose [] Int -> Bool
equalGRoseListInt x y = geq x y

equalNGRoseListInt :: NGRose [] Int -> NGRose [] Int -> Bool
equalNGRoseListInt x y = geq x y

testNGRose = geq ngrose1 ngrose2

equalBTreeInt:: BinTree Int -> BinTree Int -> Bool
equalBTreeInt = geq


gRoseEx1, gRoseEx2 :: GRose [] Int
gRoseEx1 = GRose 2 [GRose 1 [],GRose 2 []]
gRoseEx2 = GRose 2 [GRose 2 [],GRose 1 []]

main = print ( equalGRoseListInt gRoseEx1 gRoseEx2
             , equalGRoseListInt gRoseEx1 gRoseEx1
             )


main1 = print ( equalCompany genCom genCom
              , equalCompany genCom genCom'
              , equalCompany genCom genCom''
             )
