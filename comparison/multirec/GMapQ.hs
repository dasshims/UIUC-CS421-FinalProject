module GMapQ where

-- For the definition of gmapQ
import Generics.MultiRec
import Control.Monad
import Control.Monad.Writer



-- Higher order gmapQ
gmapQ :: (HFunctor (PF s),Ix s ix)
      => (forall ix . (Ix s ix) => s ix -> ix -> u) -> ix -> [u]
gmapQ f = snd . runWriter . composM f'
  where
    f' ix x = tell [f ix x] >> return x


