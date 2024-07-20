> module GMapDef where
> import qualified SYB1(Type', map)

Cheating: adding the representation to the library (datatype Type').

> gmap :: SYB1.Type' f -> (a -> b) -> (f a -> f b)
> gmap = SYB1.map
