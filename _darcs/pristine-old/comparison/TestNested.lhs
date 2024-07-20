> import Nested
> import PerfectDatatype

> example :: Perfect Int
> example = Succ (Succ (Succ (Zero (Fork (Fork (Fork 2 3)
>                                              (Fork 5 7))
>                                  (Fork (Fork 11 13)
>                                              (Fork 17 19))))))

> main = print ( collectPerfect example
>              )
