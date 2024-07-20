{-# OPTIONS -fglasgow-exts #-}

{-

This test exercices GENERIC eq for the company
datatypes which we use a lot. The output of the program should be
"(True, False)"

-}

module Main where
import Data.Generics
import CompanyDatatypes

main = print ( geq genCom genCom
             , geq genCom genCom'
             )
