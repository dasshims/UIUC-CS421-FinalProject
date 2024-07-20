> module UpdateSalaryDef where
> import LIGD(Rep(..), EPT(from,to), ($+), ($*))

> wrap :: EPT aT bT -> (bT -> bT) -> (aT -> aT)
> wrap ep f = to ep . f . from ep
  
> updateSalary :: Float -> Rep a -> a -> a
> updateSalary f (RSum a b   ep) = wrap ep $
>    updateSalary f a $+ updateSalary f b
> updateSalary f (RPair a b  ep) = wrap ep $
>    updateSalary f a $* updateSalary f b
> updateSalary f (RType e a  ep) = wrap ep $
>    updateSalary f a
> updateSalary f (RCon "S" a)    = 
>    case a of RFloat ep -> wrap ep (incS f)
> updateSalary f (RCon s a)      = updateSalary f a
> updateSalary f _               = id

> -- Specific case for salaries
> incS :: Float -> Float -> Float
> incS f s = (s * (1+f))

