-- XComprez - a compression tool for XML documents.
-- Copyright (c) 2001  The Generic Haskell Team. Utrecht University
-- 
-- This library is free software; you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation; either version 2.1 of the
-- License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- XBitz.ghs
-- 
-- author: Johan Jeuring (johanj@cs.uu.nl)

module XBitz where

import Char

{- ---------------------------------------------------------------- -
 - Bits Library
 -
 - Called XBitz to avoid clash with standard Bits library.
 -
 - ---------------------------------------------------------------- -}

data Bit                        = O | I
                                deriving (Show,Eq)

intinrange2bits :: Int   -> -- range (nrOfConstructors)
                   Int   -> -- int   (constructorNr)
                   [Bit]
intinrange2bits range int = if range > 0 
                            then if int < range
                                 then let intbits = int2bits int 
                                      in addzeroes 
                                           (ceiling (logBase 2 (fromIntegral range)) - length intbits) 
                                           intbits
                                 else error "intinrange2bits: int out of range"
                            else error "intinrange2bits: range=0"


bits2intinrange  :: Int   ->    -- range (nrOfConstructors)
                    [Bit] ->
                    (Int,[Bit]) -- constructorNr and remaining bits
bits2intinrange range bits  =  if range > 0 
                               then let (l,r) = splitAt (ceiling (logBase 2 (fromIntegral range))) bits
                                    in (bits2int l,r)
                               else error "bits2intinrange: range <= 0"

int2bits    :: Int -> [Bit]
int2bits n  =  if n >= 0
               then int2bits' n [] 
               else []
  where int2bits' 0 = id
        int2bits' n = int2bits' (n `div` 2) . (int2bit (n `mod` 2):)

bits2int     :: [Bit] -> Int
bits2int bs  =  bits2int' bs (length bs - 1) where
  bits2int' [] n = 0
  bits2int' (x:xs) n = bits2int' xs (n-1) + if x==I then 2^n else 0

int2bit    :: Int -> Bit
int2bit n  =  if n==0 then O else I

bit2int    :: Bit -> Int
bit2int b  =  case b of { O -> 0; I -> 1}

bits2string       :: [Bit] -> String
bits2string bits  =  chr (length bits `mod` 8):bits2string' bits
  where bits2string' bits  = if null bits
                             then ""
                             else let (byte,rest) = splitAt 8 bits
                                  in if null rest 
                                     then [bits2char byte]
                                     else bits2char byte : bits2string' rest

-- The first character is needed to determine the nr of bits of the last byte.

string2bits    :: String -> [Bit]
string2bits string  =  let l = ord (head string)
                       in if null (tail string)
                          then []
                          else let (is,ls) = splitAt (length string - 2) (tail string)
                                   barelastbits = remove_leading_zeroes (char2bits (unList ls))
                                   lastbits = addzeroes (l - length barelastbits) barelastbits
                               in concat (map char2bits is) ++ lastbits
  where unList [x] = x
        unList _   = error "unList"

-- was concat . map char2bits

remove_leading_zeroes []      =  []
remove_leading_zeroes [O]     =  [O] -- may also be removed? Don't know...
remove_leading_zeroes (O:xs)  =  remove_leading_zeroes xs
remove_leading_zeroes xs      =  xs

bits2char       :: [Bit] -> Char
bits2char bits  =  chr (bits2int bits)

char2bits    :: Char -> [Bit]
char2bits c  =  let bits = int2bits (ord c)
                in addzeroes (8 - length bits) bits
                
addzeroes :: Int -> [Bit] ->[Bit]
addzeroes n bits = if n >= 0 
                   then replicate n O ++ bits
                   else bits

--
data CharList  =  Nil | Cons Char CharList deriving (Show)

encodeCharList              ::  CharList -> [Bit]
encodeCharList Nil          =   [O]
encodeCharList (Cons c cs)  =   I : char2bits c ++ encodeCharList cs


