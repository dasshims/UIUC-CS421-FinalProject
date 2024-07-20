> {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

The undecidable instances are for  "instance Representable a => Code a"

{-

-}

> module Main where

> import XBitz 
> import CompanyDatatypes
> import LIGD
> import CompanyReps

> encodeChar c  =  char2bits c
> encodeInt i   =  let ib = int2bits i in addzeroes (8-length ib) ib

> decodesChar bits  = [(bits2char (take 8 bits),drop 8 bits)]
> decodesInt bits   = [(bits2int (take 8 bits),drop 8 bits)]

> encFloat :: Float -> [Bit]
> encFloat f = encode (show f)
> decodesFloat :: [Bit] -> [(Float,[Bit])]
> decodesFloat bs = map (\(c,cs) -> (read c, cs))(decodes bs)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function encode}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> encodeR :: forall tT . Rep tT -> tT -> [Bit]
> encodeR (RChar      ep)  t  =  encodeChar (from ep t)
> encodeR (RInt       ep)  t  =  encodeInt (from ep t)
> encodeR (RUnit      ep)  t  =  case from ep t of
>                               Unit -> []
> encodeR (RFloat     ep)  t  =  encFloat (from ep t)
> encodeR (RSum a b   ep)  t  =  case from ep t of 
>                               Inl x ->  O : encodeR a x
>                               Inr y ->  I : encodeR b y
> encodeR (RPair a b  ep)  t  =  case from ep t of
>                               x :*: y -> encodeR a x ++ encodeR b y
> encodeR (RType e a  ep)  t  =  encodeR a (from ep t)
> encodeR (RCon s a)       t  =  encodeR a t

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function decode}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> decodesR :: forall tT . Rep tT -> [Bit] -> [(tT, [Bit])]
> decodesR (RChar      ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesChar  bs ]
> decodesR (RInt       ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesInt   bs ]
> decodesR (RFloat     ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesFloat bs ]
> decodesR (RUnit      ep)  bs        =  [ (to ep Unit, bs) ]
> decodesR (RSum a b   ep)  []        =  []
> decodesR (RSum a b   ep)  (O : bs)  =  [ (to ep (Inl x),  cs)  |  (x,  cs) <- decodesR a bs ]
> decodesR (RSum a b   ep)  (I : bs)  =  [ (to ep (Inr y),  cs)  |  (y,  cs) <- decodesR b bs ]
> decodesR (RPair a b  ep)  bs        =  [ (to ep (x :*: y), ds)  |  (x,  cs) <- decodesR a bs
>                                                                ,  (y,  ds) <- decodesR b cs ]
> decodesR (RType e a  ep)  bs        =  [ (to ep c, cs) | (c, cs) <- decodesR a bs ]
> decodesR (RCon s a)       bs        =  decodesR a bs

> decodeR :: Rep aT -> [Bit] -> aT
> decodeR a bs  =  case decodesR a bs of
>                 [(x, [])]  ->  x
>                 _          ->  error "decode: no parse"

> ---------------------------------------------------------
>
> class Code a where
>  encode :: a -> [Bit]
>  decodes :: [Bit] -> [(a,[Bit])]
>  decode ::  [Bit] -> a

> instance Representable a => Code a where
>  encode    = encodeR rep
>  decodes   = decodesR rep
>  decode bs = case decodes bs of 
>                    [(x, [])] ->  x
>                    _         ->  error "decode: no parse"

----------------------------------------------------------------------------------

>
> main = print $  ( encode True
>               , ( encode [True]
>               , ( encode (1::Int)
>               , ( encode "1"
>               , ( encode genCom
>               , ( genCom == genCom' 
>               ))))))
>   where
>     genCom' = decode (encode genCom) :: Company
>